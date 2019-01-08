module Main where

import Prelude

import Control.Async (Async, ifItWorked, withError)
import Control.File as File
import Control.Github.Api (GetRepoErrors, getRepo)
import Control.Monad.Cont.Trans (runContT)
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Github.Settings.BranchProtection (BranchProtectionSettings)

newtype Config = Config
  { githubToken  :: Maybe String
  , organization :: String
  , repository   :: String
  , branchProtection :: BranchProtectionSettings
  }

derive instance newtypeConfig :: Newtype Config _

instance showConfig :: Show Config  where
  show (Config c) = "{ githubToken: " <> (show (c.githubToken)) <> " }"

readConfig :: String -> Async (Either File.ReadJsonError Config)
readConfig = File.readJsonFile


data ProgramErrors
  = ConfigError File.ReadJsonError
  | GetRepositoryError GetRepoErrors


instance explainProgramErrors :: Explain ProgramErrors where
  explain :: ProgramErrors -> String
  explain (ConfigError err) = "There was an error while reading the configuration file: " <> explain err
  explain (GetRepositoryError err) = "Error fetching the repository: " <> explain err


program :: Async (Either ProgramErrors Repository)
program = do
  -- maybeConfig :: Either ProgramErrors Config
  maybeConfig <- readConfig "./config.json" `withError` ConfigError
  ifItWorked maybeConfig (\
    (Config config) -> getRepo config.githubToken config.organization config.repository `withError` GetRepositoryError
  )


main :: Effect Unit
main = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Buu: " <> explain err
      Right result -> log $ "Yeay: " <> show result
  )

-- Testing
readBrachProtectionSettings :: Async (Either ProgramErrors String)
readBrachProtectionSettings = do
  -- maybeConfig :: Either ProgramErrors Config
  maybeConfig <- readConfig "./config.json" `withError` ConfigError
  pure $ (\(Config config) -> explain config.branchProtection) <$> maybeConfig
