module Main where

import Prelude

import Control.Async (Async, runAsync)
import Control.File (ReadJsonFileError)
import Control.File as File
import Data.Either (Either(..))
import Data.Explain (explain)
import Data.JSON.ParseForeign (class ParseForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)
import Github.Api.Api (AccessToken)
import Github.Api.Repository (Repository, GetRepoError, getRepo)
import Github.Entities (OrgName, RepoName)
import Github.Settings.BranchProtection (BranchProtectionSettings)
import Type.Row (RowApply)


-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃

-- Empty Set (unicode option+00D8)
type Ø = ()


newtype Config = Config
  { githubToken  :: Maybe AccessToken
  , organization :: OrgName
  , repository   :: RepoName
  , branchProtection :: BranchProtectionSettings
  }

derive instance newtypeConfig :: Newtype Config _

derive newtype instance parseForeignConfig :: ParseForeign Config

readConfig :: ∀ e. String -> Async (ReadJsonFileError e) Config
readConfig = File.readJsonFile

-- TODO: If I want to have this type of error message where I add context, I should be able to group
-- by ConfigError, etc
-- instance explainProgramErrors :: Explain ProgramErrors where
--   explain :: ProgramErrors -> String
--   explain (ConfigError err) = "There was an error while reading the configuration file: " <> explain err
--   explain (GetRepositoryError err) = "Error fetching the repository: " <> explain err
type ProgramErrors = (ReadJsonFileError ⋃ GetRepoError ⋃ Ø)

program :: Async ProgramErrors Repository
program = do
  config <- readConfig "./config.json"
  config # (\(Config c) -> getRepo c.githubToken c.organization c.repository)


main :: Effect Unit
main = runAsync program resultCb where
  resultCb (Left err)     = log $ "Buu: " <> explain err
  resultCb (Right result) = log $ "Yeay: " <> show result

