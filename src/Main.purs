module Main where

import Prelude

import Control.Async (Async)
import Control.File as File
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Control.Github.Api (GetRepoErrors, getRepo)

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
  explain (ConfigError err) = "There was an error while reading the configuration file: " <> explain err
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
      Left err     -> log $ "Buu: " <> explain err
      Right result -> log $ "Yeay: " <> show result
  )
