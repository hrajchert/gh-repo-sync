module Mvp where

import Prelude

import Control.Async (Async, runAsync)

import Data.Either (Either(..))
import Data.Explain (explain)
import Simple.JSON (class ReadForeign)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Github.Api.Api (AccessToken)
import Github.Api.Repository (Repository, GetRepoError, getRepo)
import Github.Settings.BranchProtection (BranchProtectionSettings, syncBranchProtectionSettings, SyncBPSettingsErrors)
import Github.Entities (BranchObject)
import Config (ReadConfigError, readConfig)
-- import Github.Settings.BranchProtection (BranchProtectionSettings)
import Type.Row as R

-- Used to join error types together (unicode option+22C3)
infixr 0 type R.RowApply as ⋃

-- Empty Set (unicode option+00D8)
type Ø = ()


newtype Config = Config
  { githubToken  :: AccessToken
  , from         :: BranchObject
  , to           :: BranchObject
  }


-- Derive ReadForeign to automatically tell how to parse the json
derive newtype instance readForeignConfig :: ReadForeign Config


readConfig' :: ∀ e. String -> Async (ReadConfigError e) Config
readConfig' = readConfig


-------------------------------------------------------------------------------

type ProgramErrors = (ReadConfigError ⋃ SyncBPSettingsErrors ⋃ Ø)

program :: Async ProgramErrors BranchProtectionSettings
program = do
  Config c <- readConfig' "./mvp-config.json"
  syncBranchProtectionSettings c.githubToken c.from c.to


main :: Effect Unit
main = runAsync program resultCb where
  resultCb (Left err)     = log $ "Buu: " <> explain err
  resultCb (Right result) = log $ "Yeay: " <> explain result

