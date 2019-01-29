module Data.Rules
  ( Rules
  , boolRule
  , maybeRules
  , rule
  ) where

import Data.Maybe (Maybe(..))

type Rules = Array String
-- TODO: maybe move somewhere else?
boolRule :: String -> Boolean -> Rules
boolRule _   false = []
boolRule str true  = [str]

maybeRules :: ∀ a. (a -> Rules) -> Maybe a -> Rules
maybeRules _ (Nothing) = []
maybeRules f (Just a) = f a

rule :: String -> Rules
rule str = [str]
