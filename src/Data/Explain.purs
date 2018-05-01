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


instance explainNonEmptyList :: Explain a => Explain (NonEmptyList a) where
  explain :: NonEmptyList a -> String
  explain list = foldl explainItem "" list where
    explainItem :: String -> a -> String
    explainItem accu a = accu <> "\n    * " <> capitalize (explain a)

instance explainForeignError :: Explain ForeignError where
  explain :: ForeignError -> String
  explain (ForeignError msg) = msg
  explain (ErrorAtIndex i e) = "at index " <> show i <> ", " <> explain e
  explain (ErrorAtProperty prop e) = "on property " <> show prop <> ", " <> explain e
  explain (JSONError s) = "there was a json error: " <> s
  explain (TypeMismatch exp act) = "i was expecting " <> show exp <> ", but got " <> show act

