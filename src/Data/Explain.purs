module Data.Explain
  ( class Explain
  , explain
  ) where


import Data.Explain (class Explain, explain)
import Data.Foldable (foldl)
import Data.Foreign (ForeignError(TypeMismatch, JSONError, ErrorAtProperty, ErrorAtIndex, ForeignError))
import Data.List.Types (NonEmptyList)
import Prelude (show, (<>))
import Utils.String (capitalize)

class Explain a where
  explain :: a -> String


-- TODO: Make this generic for any a that is Explainable
instance explainReadJsonError :: Explain (NonEmptyList ForeignError) where
  explain :: NonEmptyList ForeignError -> String
  explain errors = foldl renderError "" errors where
    renderError :: String -> ForeignError -> String
    renderError accu error = accu <> "\n    * " <> capitalize (explain error)

instance explainForeignError :: Explain ForeignError where
  explain :: ForeignError -> String
  explain (ForeignError msg) = msg
  explain (ErrorAtIndex i e) = "at index " <> show i <> ", " <> explain e
  explain (ErrorAtProperty prop e) = "on property " <> show prop <> ", " <> explain e
  explain (JSONError s) = "there was a json error: " <> s
  explain (TypeMismatch exp act) = "i was expecting " <> show exp <> ", but got " <> show act

