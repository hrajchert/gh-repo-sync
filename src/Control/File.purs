module Control.File
  ( readJsonFile
  , readTextFile
  , parseConfig -- Should not be here
  , ReadJsonError
  )
   where


import Control.Async (Async)
import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff.Exception (Error, message)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Explain (class Explain, explain)
import Data.Foldable (foldl)
import Data.Foreign (ForeignError, renderForeignError)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse)
import Node.Buffer (Buffer, BUFFER, toString)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Async (readFile)
import Prelude (class Show, bind, pure, show, (#), ($), (<#>), (<>), (<$>))
import Simple.JSON (class ReadForeign, readJSON)


readFileCont
  :: forall eff
   . String
  -> Async (fs :: FS, buffer :: BUFFER | eff) (Either Error Buffer)
readFileCont path = ContT $ readFile path


readTextFile
  :: forall eff
  . String
  -> Async (fs :: FS, buffer :: BUFFER | eff) (Either Error String)
readTextFile path = do
  maybeBuffer <- readFileCont path
  -- Async eff == ContT (cb -> Eff eff Unit)
  --        cb :: (Either Error String) -> Eff eff Unit
  ContT (\cb -> do
                -- traverse :: (Buffer -> Eff eff String) -> Either Error Buffer -> Eff eff (Either Error String)
                -- maybeText :: Either Error String
                maybeText <- traverse (toString UTF8) maybeBuffer
                cb maybeText
  )

-- TODO: Shouldn't have config stuff in a File utils
parseConfig :: forall c a. ReadForeign a => Newtype c a => String -> Either (NonEmptyList ForeignError) c
parseConfig str = wrap <$> maybeObj where
  maybeObj :: Either (NonEmptyList ForeignError) a
  maybeObj = readJSON str


data ReadJsonError
  = ReadFileError String String
  | JsonParseError String (NonEmptyList ForeignError)

instance readJsonErrorShow :: Show ReadJsonError where
  show (ReadFileError path err) = "(ReadFileError file:" <> show path <> ", err: "<> err <> ")"
  show (JsonParseError path err) = "(JsonParseError file: " <> show path <> ", err: " <> show (showForeignErrors err) <> ")"

instance explainReadJsonError :: Explain ReadJsonError where
  explain :: ReadJsonError -> String
  explain (ReadFileError path err)  = "There was a problem reading the json file " <> show path <> ": " <> err
  explain (JsonParseError path err) = "There was a problem parsing the json file " <> show path <> ":" <> explain err


-- TODO: Thinking of putting this into a ForeignHelper
showForeignErrors :: NonEmptyList ForeignError -> String
showForeignErrors errors = foldl showError "" errors where
  showError :: String -> ForeignError -> String
  showError "" error = renderForeignError error
  showError accu error = accu <> ", " <> renderForeignError error



mapReadFileError :: String -> Error -> ReadJsonError
mapReadFileError path err = ReadFileError path (message err)

mapJsonParseError :: String -> NonEmptyList ForeignError -> ReadJsonError
mapJsonParseError path errors = JsonParseError path errors

readJsonFile
  :: forall eff t a
  .  ReadForeign a => Newtype t a
  => String
  -> Async (fs :: FS, buffer :: BUFFER | eff) (Either ReadJsonError t)
readJsonFile path = do
  -- maybeString :: Either ReadJsonError String
  maybeString <-
              -- Read the file
                readTextFile path
                          -- flip map the Async and left map the Either to convert the error type
                          <#> lmap (mapReadFileError path)
  -- Insert in Async
  pure $ do
    str  <- maybeString
    -- Parse the string into a json
    parseConfig str
      -- Map the Either to convert the error type
      # lmap (mapJsonParseError path)





