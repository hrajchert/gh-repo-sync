module Data.Explain
  ( class Explain
  , explain
  , class VariantExplains
  , variantExplains
  )
where


import Data.Foldable (foldl, class Foldable)
import Data.List as L
import Foreign (ForeignError(..))
import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Data.Variant.Internal (class VariantTags, RLProxy(..), VariantCase, VariantRep(..), lookup, variantTags)
import Prelude (show, (<>))
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

class Explain a where
  explain :: a -> String

-------------------------------------------------------------------------------
instance explainString :: Explain String where
  explain str = str

-------------------------------------------------------------------------------
explainFoldable :: forall f a. Foldable f => Explain a => f a -> String
explainFoldable list = foldl explainItem "" list where
  explainItem :: String -> a -> String
  explainItem accu a = accu <> "\n    * " <> explain a -- capitalize (explain a)

instance explainNonEmptyList :: Explain a => Explain (NonEmptyList a) where
  explain = explainFoldable

instance explainArray :: Explain a => Explain (Array a) where
  explain = explainFoldable

-------------------------------------------------------------------------------
class VariantExplains (rl ∷ R.RowList) where
  variantExplains ∷ RLProxy rl → L.List (VariantCase → String)

instance showVariantNil ∷ VariantExplains R.Nil where
  variantExplains _ = L.Nil

instance explainVariantCons ∷ (VariantExplains rs, Explain a) ⇒ VariantExplains (R.Cons sym a rs) where
  variantExplains _ =
    L.Cons (coerceExplain explain) (variantExplains (RLProxy ∷ RLProxy rs))
    where
    coerceExplain ∷ (a → String) → VariantCase → String
    coerceExplain = unsafeCoerce

instance explainVariant ∷ (R.RowToList r rl, VariantTags rl, VariantExplains rl) ⇒ Explain (Variant r) where
  explain :: (Variant r) -> String
  explain v1 =
    let
      VariantRep v = unsafeCoerce v1 ∷ VariantRep VariantCase
      tags = variantTags (RLProxy ∷ RLProxy rl)
      explains = variantExplains (RLProxy ∷ RLProxy rl)
      body = lookup "explain" v.type tags explains v.value
    in
      body

-------------------------------------------------------------------------------
instance explainForeignError :: Explain ForeignError where
  explain :: ForeignError -> String
  explain e
    = case e of
      (ForeignError msg)       -> msg
      (ErrorAtIndex _ _)        -> "at position " <> showProperty true e <> " " <> showError e
      (ErrorAtProperty _ _) -> "property " <> show (showProperty true e) <> " " <> showError e
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

