module Data.JSON.ParseForeign
  ( readJSON
  , readJSON'
  , read
  , read'
  , class ParseForeign
  , parseForeign
  , class ParseForeignFields
  , getFields
  )
 where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..),runExcept, withExcept)
-- import Control.Monad.Except  runExcept, runExceptT, withExcept)
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, isNull, isUndefined, tagOf, unsafeFromForeign)
import Foreign.Index (readProp)
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.List.NonEmpty (NonEmptyList, toUnfoldable, appendFoldable)
import Data.Nullable (Nullable, toNullable)
import Record.Builder (Builder)
import Record.Builder as Builder
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Variant (Variant, inj)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Prelude (RLProxy(..))

-- | Read a JSON string to a type `a` while returning a `MultipleErrors` if the
-- | parsing failed.
readJSON :: forall a
  .  ParseForeign a
  => String
  -> Either MultipleErrors a
readJSON = runExcept <<< readJSON'

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON' :: forall a
  .  ParseForeign a
  => String
  -> F a
readJSON' = parseForeign <=< parseJSON

read :: forall a
   . ParseForeign a
  => Foreign
  -> Either MultipleErrors a
read = runExcept <<< parseForeign

read' :: forall a
   . ParseForeign a
  => Foreign
  -> F a
read' = parseForeign

foreign import _parseJSON :: EU.EffectFn1 String Foreign

parseJSON :: String -> F Foreign
parseJSON
    = ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< EU.runEffectFn1 _parseJSON
  where
    -- Nate Faubion: "It uses unsafePerformEffect because that’s the only way to catch exceptions and still use the builtin json decoder"
    runPure = unsafePerformEffect

class ParseForeign a where
  parseForeign :: Foreign -> F a

instance parseForeignImpl :: ParseForeign Foreign where
  parseForeign = pure

instance parseChar :: ParseForeign Char where
  parseForeign = readChar

instance parseNumber :: ParseForeign Number where
  parseForeign = readNumber

instance parseInt :: ParseForeign Int where
  parseForeign = readInt

instance parseString :: ParseForeign String where
  parseForeign = readString

instance parseBoolean :: ParseForeign Boolean where
  parseForeign :: Foreign -> F Boolean
  parseForeign = readBoolean

-- Recursively parse the Array of a and get all errors
instance parseArray :: ParseForeign a => ParseForeign (Array a) where
  parseForeign
    :: forall a. ParseForeign a
    => Foreign
    -> F (Array a)
  -- Parse the foreign as an Array of foreigns and then iterate over each one
  parseForeign foreignArray =  parseForeignArray =<< readArray foreignArray where
    -- Iterate over the array to parse each foreign of type a and collect all errors
    parseForeignArray :: Array Foreign -> F (Array a)
    parseForeignArray = foldlWithIndex reducer initial where
      -- Acummulate each result or error
      reducer :: Int -> F (Array a) -> Foreign -> F (Array a)
      reducer i faccu f = do
        -- Put accu into scope and if the computation already fail keep on parsing to get all errors
        -- accu :: Array a
        accu <- withExcept (continueParsingAfterErrror i f) faccu
        -- Parse the inner foreign and append it to accumulated result
        parseAndAppendInnerForeign i f accu

      -- Parse the inner foreign and append it to accumulated result
      parseAndAppendInnerForeign :: Int -> Foreign -> Array a -> F (Array a)
      parseAndAppendInnerForeign i f accu = append accu <$> pure <$> parseInnerForeign i f

      -- Parse inner Foreign to the correct type and if it fail add the array index for better context
      parseInnerForeign :: Int -> Foreign -> F a
      parseInnerForeign i f = withExcept (wrapErrorWithIndex i) $ parseForeign f

      -- Wrap ForeignError with an ErrorAtIndex constructor
      wrapErrorWithIndex :: Int -> NonEmptyList ForeignError -> NonEmptyList ForeignError
      wrapErrorWithIndex i err = ErrorAtIndex i <$> err

      -- Keep parsing discarding results, and
      continueParsingAfterErrror :: Int -> Foreign -> NonEmptyList ForeignError -> NonEmptyList ForeignError
      continueParsingAfterErrror i f err = appendFoldable err (parseAsError i f)

      -- Parse inner foreign as an error
      parseAsError :: Int -> Foreign -> Array ForeignError
      parseAsError i f = bifoldMap foldErr foldOk (runExcept (parseInnerForeign i f)) where
        -- If it fail convert NonEmptyList to Array
        foldErr :: NonEmptyList ForeignError -> Array ForeignError
        foldErr = toUnfoldable

        -- If it works discard and don't accumulate a new error
        foldOk :: a -> Array ForeignError
        foldOk _ = []

      initial = pure []

instance parseMaybe :: ParseForeign a => ParseForeign (Maybe a) where
  parseForeign = readNullOrUndefined parseForeign
    where
      readNullOrUndefined _ value | isNull value || isUndefined value = pure Nothing
      readNullOrUndefined f value = Just <$> f value

instance parseNullable :: ParseForeign a => ParseForeign (Nullable a) where
  parseForeign o = withExcept (map reformat) $
    map toNullable <$> traverse parseForeign =<< readNull o
    where
      reformat error = case error of
        TypeMismatch inner other -> TypeMismatch ("Nullable " <> inner) other
        _ -> error

instance parseObject :: ParseForeign a => ParseForeign (Object.Object a) where
  parseForeign = sequence <<< Object.mapWithKey (const parseForeign) <=< readObject'
    where
      readObject' :: Foreign -> F (Object Foreign)
      readObject' value
        | tagOf value == "Object" = pure $ unsafeFromForeign value
        | otherwise = fail $ TypeMismatch "Object" (tagOf value)


instance parseRecord ::
  ( RowToList fields fieldList
  , ParseForeignFields fieldList () fields
  ) => ParseForeign (Record fields) where
  parseForeign
    :: Foreign -> F (Record fields)
  parseForeign o = do
    -- steps :: Builder (Record from) (Record to)
    steps <- getFields fieldListP o
    -- steps <- withExcept (continueParsingAfterErrror o) $ getFields fieldListP o
    pure $ Builder.build steps {}
    where
      fieldListP :: RLProxy fieldList
      fieldListP = RLProxy

-- | A class for reading foreign values from properties
class ParseForeignFields (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  getFields
    :: RLProxy xs
    -> Foreign
    -> F (Builder (Record from) (Record to))

instance parseForeignFieldsCons ::
  ( IsSymbol fieldName
  , ParseForeign fieldType
  , ParseForeignFields tail from from'
  , Row.Lacks fieldName from'
  , Row.Cons fieldName fieldType from' to
  ) => ParseForeignFields (Cons fieldName fieldType tail) from to where
  getFields _ obj = do
    value :: fieldType <- withExcept' $ parseForeign =<< readProp name obj
    rest <- getFields tailP obj
    let
      first :: Builder (Record from') (Record to)
      first = Builder.insert nameP value
    pure $ first <<< rest
    where
      nameP = SProxy :: SProxy fieldName
      tailP = RLProxy :: RLProxy tail
      name :: String
      name = reflectSymbol nameP

      withExcept' :: forall p. F p -> F p
      withExcept' = withExcept $ continueParsingAfterErrror tailP obj <<< wrapErrorWithProperty

      -- Wrap ForeignError with an ErrorAtProperty constructor
      wrapErrorWithProperty :: NonEmptyList ForeignError -> NonEmptyList ForeignError
      wrapErrorWithProperty = map $ ErrorAtProperty name

      -- Keep parsing discarding results, and
      -- continueParsingAfterErrror :: Foreign -> NonEmptyList ForeignError -> NonEmptyList ForeignError
      continueParsingAfterErrror tail o err = appendFoldable err (parseAsError tail o)

      -- Parse inner foreign as an error
      -- parseAsError :: Foreign -> Array ForeignError
      parseAsError tail o = bifoldMap foldErr foldOk (runExcept (getFields tail o)) where
        -- If it fail convert NonEmptyList to Array
        foldErr :: NonEmptyList ForeignError -> Array ForeignError
        foldErr = toUnfoldable

        -- If it works discard and don't accumulate a new error
        foldOk :: forall a. a -> Array ForeignError
        foldOk _ = []

instance parseForeignFieldsNil ::
  ParseForeignFields Nil () () where
  getFields _ _ =
    pure identity

instance parseForeignVariant ::
  ( RowToList variants rl
  , ParseForeignVariant rl variants
  ) => ParseForeign (Variant variants) where
  parseForeign o = parseVariantImpl (RLProxy :: RLProxy rl) o

class ParseForeignVariant (xs :: RowList) (row :: # Type)
  | xs -> row where
  parseVariantImpl :: RLProxy xs
    -> Foreign
    -> F (Variant row)

instance parseVariantNil ::
  ParseForeignVariant Nil trash where
  parseVariantImpl _ _ = fail $ ForeignError "Unable to match any variant member."

instance parseVariantCons ::
  ( IsSymbol name
  , ParseForeign ty
  , Row.Cons name ty trash row
  , ParseForeignVariant tail row
  ) => ParseForeignVariant (Cons name ty tail) row where
  parseVariantImpl _ o = do
    obj :: { type :: String, value :: Foreign } <- parseForeign o
    if obj.type == name
      then do
        value :: ty <- parseForeign obj.value
        pure $ inj namep value
      else
        (fail <<< ForeignError $ "Did not match variant tag " <> name)
    <|> parseVariantImpl (RLProxy :: RLProxy tail) o
    where
      namep = SProxy :: SProxy name
      name = reflectSymbol namep


