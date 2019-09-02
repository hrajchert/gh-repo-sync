module Config
  ( ReadConfigError
  , groupByReadConfigError
  , ReadConfigErrorImpl
  , readConfig
  )
  where

import Prelude
import Control.Async (Async, mapExceptT')
import Control.File (JsonParseErrorImpl(..), ReadFileErrorImpl(..), ReadJsonFileError, _readFileError, _readFileJsonParseError)
import Control.File as File
import Simple.JSON (class ReadForeign)
import Data.Explain (class Explain, explain)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (SProxy(..), Variant, default, inj, onMatch)
import Effect.Exception (message)
import Type.Row as R

-- Used to join error types together (unicode option+22C3)
infixr 0 type R.RowApply as ⋃

-- Empty Set (unicode option+00D8)
type Ø = ()

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

readConfig :: ∀ e c. ReadForeign c => String -> Async (ReadConfigError e) c
readConfig path = File.readJsonFile path `mapExceptT'` groupByReadConfigError
