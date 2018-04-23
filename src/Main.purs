module Main where

import Prelude

import Control.Async (Async)
import Control.File as File
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Cont (ContT(ContT))
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error, message)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository, RepositoryParse(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (affjax, defaultRequest, AffjaxRequest, AffjaxResponse, AJAX)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Node.Buffer (BUFFER)
import Node.FS (FS)



---------------------------
-- REQUEST
---------------------------
request
  :: forall eff a b. Requestable a => Respondable b
  =>  AffjaxRequest a
  -> (Either Error (AffjaxResponse b) -> Eff (ajax :: AJAX | eff) Unit)
  -> Eff (ajax :: AJAX | eff) Unit
request req cb = runAff cb aff *> pure unit where
  -- innerCb :: (Either Error (AffjaxResponse b) → Eff (ajax :: AJAX | eff) Unit)
  -- innerCb (Left err)  = cb $ Left err
  -- innerCb (Right val) = cb $ Right val

  aff :: Aff (ajax :: AJAX | eff) (AffjaxResponse b)
  aff = affjax req

requestCont
  :: forall eff a b. Requestable a => Respondable b
  => AffjaxRequest a
  -> Async (ajax :: AJAX | eff) (Either Error (AffjaxResponse b))
requestCont req = ContT (\cb -> request req cb)

authHeader :: String -> RequestHeader
authHeader token = RequestHeader "Authorization" ("Bearer " <> token)

addAccessTokenIfPresent :: Maybe String -> Array RequestHeader  -> Array RequestHeader
addAccessTokenIfPresent Nothing            headers = headers
addAccessTokenIfPresent (Just accessToken) headers = headers <> [authHeader accessToken]

getStatusCode :: StatusCode -> Int
getStatusCode (StatusCode n) = n

---------------------------
-- API
---------------------------

api :: String -> String
api url = "https://api.github.com/" <> url

site :: String -> String
site url = "https://www.github.com/" <> url

orgRepos :: String -> String
orgRepos org = api $ "orgs/" <> org <> "/repos"

apiRepoUrl :: String -> String -> String
apiRepoUrl owner repo = api $ "repos/" <> owner <> "/" <> repo

siteRepoUrl :: String -> String -> String
siteRepoUrl owner repo = site $ owner <> "/" <> repo


getRepo
  :: forall eff
  .  Maybe String -- Access token
  -> String -- Organization name
  -> String -- Repository name
  -> Async (ajax :: AJAX | eff) (Either GetRepoErrors Repository)
getRepo maybeAccessToken org repo =
  -- Request the url and transform both the error and the result
  transformResponse <$> requestCont req
    where
      req = defaultRequest  { url = (apiRepoUrl org repo)
                            , headers = addAccessTokenIfPresent maybeAccessToken []
                            , method = Left GET
                            }

      transformResponse :: Either Error (AffjaxResponse RepositoryParse) -> Either GetRepoErrors Repository
      transformResponse (Left err)  = Left (InternalError $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse val.response
        401 -> Left InvalidCredentials
        404 -> Left (RepoNotFound $ siteRepoUrl org repo)
        n   -> Left (InternalError $ "Unexpected status code " <> show n)

      interpretParsedResponse :: RepositoryParse -> Either GetRepoErrors Repository
      interpretParsedResponse (RepositoryParse (Left err)) = Left (InternalError ("parsing problems: " <> explain err))
      interpretParsedResponse (RepositoryParse (Right r)) = Right r


data GetRepoErrors
  = InternalError String
  | RepoNotFound String
  | InvalidCredentials

instance explainGetRepoErrors :: Explain GetRepoErrors where
  explain :: GetRepoErrors -> String
  explain (InternalError str)    = "There was an internal error: " <> str
  explain (RepoNotFound repoUrl) = "The repository (" <> repoUrl <> ") is not found, maybe it's private?"
  explain InvalidCredentials     = "The access token you provided is invalid or cancelled"



---------------------------
-- PROGRAM
---------------------------
newtype Config = Config
  { githubToken  :: Maybe String
  , organization :: String
  , repository   :: String
  }

derive instance newtypeConfig :: Newtype Config _


instance showConfig :: Show Config  where
  show (Config c) = "{ githubToken: " <> (show (c.githubToken)) <> " }"


readConfig :: forall eff. String -> Async (fs :: FS, buffer :: BUFFER | eff) (Either File.ReadJsonError Config)
readConfig = File.readJsonFile


data ProgramErrors
  = ConfigError File.ReadJsonError
  | GetRepositoryError GetRepoErrors


instance explainProgramErrors :: Explain ProgramErrors where
  explain :: ProgramErrors -> String
  explain (ConfigError err) = "There was an error while reading the configuration file" <> explain err
  explain (GetRepositoryError err) = "Error fetching the repository: " <> explain err

program :: forall eff. Async (fs :: FS, buffer :: BUFFER, ajax :: AJAX | eff) (Either ProgramErrors Repository)
program = do
  -- maybeConfig :: Either ProgramErrors Config
  maybeConfig <- lmap ConfigError <$> readConfig "./config.json"
  -- getRepo t o r :: Async (ajax :: AJAX | eff) (Either GetRepoErrors Repository)
  case maybeConfig of
    Left err -> pure $ Left err
    Right (Config config) -> lmap GetRepositoryError <$> getRepo config.githubToken config.organization config.repository


main :: Eff (console :: CONSOLE, buffer :: BUFFER, fs :: FS, ajax :: AJAX) Unit
main = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ explain err
      Right result -> log $ "We got the information for: " <> show result
  )
