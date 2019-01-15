module Poc where

import Prelude

import Control.Async (Async, ifItWorked, withError)
import Control.File as File
import Control.Monad.Cont.Trans (runContT)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)
import Github.Api.Api (AccessToken(..))
import Github.Api.Repository (Repository, GetRepoErrors, getRepo)

newtype Config = Config
  { githubToken  :: Maybe AccessToken
  , organization :: String
  , repository   :: String
  }

derive instance newtypeConfig :: Newtype Config _


instance showConfig :: Show Config  where
  show (Config c) = "{ githubToken: " <> (show (c.githubToken)) <> " }"

readConfig :: String -> Async (Either File.ReadJsonError Config)
readConfig = File.readJsonFile


data ProgramErrors
  = ConfigError File.ReadJsonError
  | GetRepositoryError GetRepoErrors

instance showProgramErrors :: Show ProgramErrors  where
  show (ConfigError config) = "(ConfigError " <> show config <> ")"
  show (GetRepositoryError error) = "(GetRepositoryError " <> show error <> ")"

instance explainProgramErrors :: Explain ProgramErrors where
  explain :: ProgramErrors -> String
  explain (ConfigError err) = "There was an error while reading the configuration file: " <> explain err
  explain (GetRepositoryError err) = "Error fetching the repository: " <> explain err


program :: Async (Either ProgramErrors Repository)
program = do
  -- maybeConfig :: Either ProgramErrors Config
  maybeConfig <- readConfig "./poc-config.json" `withError` ConfigError
  ifItWorked maybeConfig (\
    (Config config) -> getRepo config.githubToken config.organization config.repository `withError` GetRepositoryError
  )


main :: Effect Unit
main = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Buu: " <> explain err
      Right result -> log $ "Yeay: " <> show result
  )
