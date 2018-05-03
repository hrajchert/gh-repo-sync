module Data.Explain
  ( class Explain
  , explain
  ) where


import Data.Explain (class Explain, explain)
import Data.Foldable (foldl)
import Data.Foreign (ForeignError(TypeMismatch, JSONError, ErrorAtProperty, ErrorAtIndex, ForeignError))
import Data.List.Types (NonEmptyList)
import Prelude (show, (<>), (==))
import Utils.String (capitalize)

class Explain a where
  explain :: a -> String


instance explainString :: Explain String where
  explain str = str

instance explainNonEmptyList :: Explain a => Explain (NonEmptyList a) where
  explain :: NonEmptyList a -> String
  explain list = foldl explainItem "" list where
    explainItem :: String -> a -> String
    explainItem accu a = accu <> "\n    * " <> capitalize (explain a)

instance explainForeignError :: Explain ForeignError where
  explain :: ForeignError -> String
  explain e
    = case e of
      (ForeignError msg)       -> msg
      (ErrorAtIndex _ _)        -> "at position " <> showProperty true e <> " " <> showError e
      (ErrorAtProperty _ _) -> "property " <> show (showProperty true e) <> " " <> showError e
      (JSONError s)            -> "invalid json: " <> s
      (TypeMismatch exp act)   -> "i was expecting " <> show exp <> ", but got " <> show act
    where
      showProperty :: Boolean -> ForeignError -> String
      showProperty initial err = case err of
        (ErrorAtIndex i e') -> "[" <> show i <> "]" <> showProperty false e'
        (ErrorAtProperty prop e') ->
                                      let
                                        prefix = if initial then "" else "."
                                      in
                                        prefix <> prop <> showProperty false e'
        _ -> ""

      showError :: ForeignError -> String
      showError err = case err of
        (ErrorAtIndex _ e') -> showError e'
        (ErrorAtProperty _ e') -> showError e'
        (TypeMismatch exp "Undefined")   -> "is undefined"
        (TypeMismatch exp act)   -> "was expected to be " <> show exp <> ", but is " <> show act
        e' -> explain e'

