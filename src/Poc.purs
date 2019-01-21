module Poc where

import Prelude

import Control.Async (Async, runAsync)
import Control.File (ReadJsonFileError )
import Control.File as File
import Data.Either (Either(..))
import Data.Explain (explain)
import Simple.JSON (class ReadForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)
import Github.Api.Api (AccessToken)
import Github.Api.Repository (Repository, GetRepoError, getRepo)
import Github.Entities (OrgName, RepoName)
import Type.Row (RowApply)

-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃

-- Empty Set (unicode option+00D8)
type Ø = ()

newtype Config = Config
  { githubToken  :: Maybe AccessToken
  , organization :: OrgName
  , repository   :: RepoName
  }

derive instance newtypeConfig :: Newtype Config _
derive newtype instance readForeignConfig :: ReadForeign Config
derive newtype instance showConfig :: Show Config

readConfig :: ∀ e. String -> Async (ReadJsonFileError e) Config
readConfig = File.readJsonFile


type ProgramErrors = (ReadJsonFileError ⋃ GetRepoError ⋃ Ø)

program :: Async ProgramErrors Repository
program = do
  Config c <- readConfig "./poc-config.json"
  getRepo c.githubToken c.organization c.repository



main :: Effect Unit
main = runAsync program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Buu: " <> explain err
      Right result -> log $ "Yeay: " <> show result
  )
