module Main where

import Prelude

import Control.Async (Async, runAsync, mapExceptT')
import Control.File (JsonParseErrorImpl(..), ReadFileErrorImpl(..), ReadJsonFileError, _readFileError, _readFileJsonParseError)
import Control.File as File
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Simple.JSON (class ReadForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (SProxy(..), Variant, default, inj, onMatch)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
import Github.Api.Api (AccessToken)
import Github.Api.Repository (Repository, GetRepoError, getRepo)
import Github.Entities (OrgName, RepoName)
import Github.Settings.BranchProtection (BranchProtectionSettings)
import Type.Row as R

-- Used to join error types together (unicode option+22C3)
infixr 0 type R.RowApply as ⋃

-- Empty Set (unicode option+00D8)
type Ø = ()


newtype Config = Config
  { githubToken  :: Maybe AccessToken
  , organization :: OrgName
  , repository   :: RepoName
  , branchProtection :: BranchProtectionSettings
  }

derive instance newtypeConfig :: Newtype Config _

derive newtype instance readForeignConfig :: ReadForeign Config

--
type ReadConfigError ρ = (readConfigError ∷ ReadConfigErrorImpl ρ | ρ)

_readConfigError :: SProxy "readConfigError"
_readConfigError = SProxy

readConfigError :: ∀ ρ. Variant (ReadJsonFileError ρ) -> Variant (ReadConfigError ρ)
readConfigError = inj _readConfigError <<< wrap

newtype ReadConfigErrorImpl e
  = ReadConfigErrorImpl (Variant (ReadJsonFileError e))

derive instance newtypeReadConfigErrorImpl :: Newtype (ReadConfigErrorImpl e) _

instance explainReadConfigError :: Explain (ReadConfigErrorImpl e) where
  explain err
      = default "Unknown problem"
      # onMatch
        { readFileError: \(ReadFileErrorImpl {error})
            -> "Can't read the config file: " <> message error
        , readFileJsonParseError: \(JsonParseErrorImpl {path, error})
            -> "There was a problem parsing the config file '" <> path <>"':"<> explain error
        }
      $ unwrap err



groupByReadConfigError :: ∀ e. Variant (ReadJsonFileError ⋃ ReadConfigError ⋃ e) -> Variant (ReadConfigError ⋃ e)
groupByReadConfigError = onMatch
  { readFileError: readConfigError <<< inj _readFileError
  , readFileJsonParseError: readConfigError <<< inj _readFileJsonParseError
  } identity
--


readConfig :: ∀ e. String -> Async (ReadConfigError e) Config
readConfig path = File.readJsonFile path `mapExceptT'` groupByReadConfigError

-------------------------------------------------------------------------------

type ProgramErrors = (ReadConfigError ⋃ GetRepoError ⋃ Ø)

program :: Async ProgramErrors Repository
program = do
  Config c <- readConfig "./config.json"
  getRepo c.githubToken c.organization c.repository


main :: Effect Unit
main = runAsync program resultCb where
  resultCb (Left err)     = log $ "Buu: " <> explain err
  resultCb (Right result) = log $ "Yeay: " <> show result

