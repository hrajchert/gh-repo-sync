module Utils.String
  ( capitalize
  ) where

import Prelude
import Data.String (singleton, toUpper, uncons)
import Data.Maybe (Maybe(..))

capitalize :: String -> String
capitalize str =
  uncons str
    # ( \maybeString -> case maybeString of
          Just { head: h, tail: t } -> toUpper (singleton h) <> t
          Nothing -> ""
      )
