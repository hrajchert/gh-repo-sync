module Control.File
  ( readJsonFile
  , readFile
  , readTextFile
  , _readFileError
  , _readFileJsonParseError
  , ReadJsonFileError
  , ReadFileError
  , ReadFileErrorImpl (..)
  , JsonParseError
  , JsonParseErrorImpl (..)
  )
where

import Prelude

import Control.Async (Async, mapExceptT')
import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Except (ExceptT(..), except)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Explain (class Explain, explain)
import Data.JSON.ParseForeign (class ParseForeign, readJSON)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant, inj)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import Foreign (ForeignError)
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Async as FS
import Node.Path (FilePath)
import Type.Data.Boolean (kind Boolean)
import Type.Row (RowApply)
infixr 0 type RowApply as ⋃

-------------------------------------------------------------------------------

readFile ::
  ∀  ρ
  .  String
  -> Async (ReadFileError ρ) Buffer
readFile path = (ExceptT $ ContT $ FS.readFile path) `mapExceptT'` readFileError path

readTextFile ::
  ∀  ρ
  .  String
  -> Async (ReadFileError ρ) String
readTextFile path = (readFile path) >>= toString' where
  toString' :: ∀ e. Buffer -> Async e String
  toString' buff = liftEffect $ toString UTF8 buff



-- -- TODO: Thinking of putting this into a ForeignHelper
-- showForeignErrors :: NonEmptyList ForeignError -> String
-- showForeignErrors errors = foldl showError "" errors where
--   showError :: String -> ForeignError -> String
--   showError "" error = renderForeignError error
--   showError accu error = accu <> ", " <> renderForeignError error



type ReadJsonFileError ρ = (ReadFileError ⋃ JsonParseError ⋃ ρ)

readJsonFile ::
  ∀  ρ a
  .  ParseForeign a
  => String
  -> Async (ReadJsonFileError ρ) a
readJsonFile path = do
  -- Read the file as a string
  str <- readTextFile path

  -- Interpret as JSON object `a` and convert the error
  -- to variant
  let
    jsonFile :: ∀ e . Either (Variant (JsonParseError e)) a
    jsonFile = readJSON str # lmap (jsonParseError path)

  -- Lift it to Async a.k.a ExceptV ContT
  except jsonFile

-------------------------------------------------------------------------------

-- | Thrown when a readFile was unsuccesful
type ReadFileError ρ = (readFileError ∷ ReadFileErrorImpl | ρ)

_readFileError :: SProxy "readFileError"
_readFileError = SProxy

newtype ReadFileErrorImpl
  = ReadFileErrorImpl
    { path  :: String
    , error :: Error
    }

instance explainReadFileError :: Explain ReadFileErrorImpl where
  explain (ReadFileErrorImpl {path, error}) = "Could not read file \"" <> path <> "\"" <> message error

readFileError :: ∀ ρ. String -> Error -> Variant (ReadFileError ρ)
readFileError path error = inj _readFileError (ReadFileErrorImpl { path, error })

---------------------------------------

-- | Thrown when parsing a file as JSON
type JsonParseError ρ = (readFileJsonParseError ∷ JsonParseErrorImpl | ρ)

-- TODO: unify into either jsonParseError or readFileJsonParseError
_readFileJsonParseError :: SProxy "readFileJsonParseError"
_readFileJsonParseError = SProxy

newtype JsonParseErrorImpl
  = JsonParseErrorImpl
    { path  :: FilePath
    , error :: NonEmptyList ForeignError
    }

instance explainJsonParseError :: Explain JsonParseErrorImpl where
  explain (JsonParseErrorImpl {path, error}) = "There was a problem parsing the json file " <> show path <> ":" <> explain error

jsonParseError :: ∀ ρ. FilePath -> NonEmptyList ForeignError -> Variant (JsonParseError ρ)
jsonParseError path error = inj _readFileJsonParseError (JsonParseErrorImpl { path, error })

