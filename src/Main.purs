module Main where

import Prelude

import Control.Async (Async, runAsync, mapExceptT')
import Control.File (JsonParseErrorImpl(..), ReadFileErrorImpl(..), ReadJsonFileError, _readFileError, _readFileJsonParseError)
import Control.File as File
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.JSON.ParseForeign (class ParseForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), Variant, default, inj, onMatch)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message)
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

type ReadConfigError ρ = (readConfigError ∷ ReadConfigErrorImpl ρ | ρ)

newtype ReadConfigErrorImpl e
  = ReadConfigErrorImpl (Variant (ReadJsonFileError e))

instance explainReadConfigError :: Explain (ReadConfigErrorImpl e) where
  explain (ReadConfigErrorImpl err) = explain' err where
    explain'
      = default "Unknown problem"
      # onMatch
        { readFileError: \(ReadFileErrorImpl {error})
            -> "Can't read the config file: " <> message error
        , readFileJsonParseError: \(JsonParseErrorImpl {path, error})
            -> "There was a problem parsing the config file '" <> path <>"':"<> explain error
        }


readConfigError :: ∀ ρ. Variant (ReadJsonFileError ρ) -> Variant (ReadConfigError ρ)
readConfigError error = inj (SProxy :: SProxy "readConfigError") (ReadConfigErrorImpl error)

groupByReadConfigError :: forall e. Variant (ReadJsonFileError ⋃ ReadConfigError e) -> Variant (ReadConfigError e)
groupByReadConfigError = onMatch
  { readFileError: readConfigError <<< inj _readFileError
  , readFileJsonParseError: readConfigError <<< inj _readFileJsonParseError
  } identity

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

