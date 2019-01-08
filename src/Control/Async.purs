module Control.Async
  ( Async
  , ifItWorked
  , withError
  )
   where

import Prelude

import Effect (Effect)
import Control.Monad.Cont.Trans (ContT)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)

-- Async eff a = (Either Error a -> Eff eff Unit) -> Eff eff Unit
type Async = ContT Unit Effect


-- Helper functions to work with Async (Either e a)

-- Helps with do notation, runs the async f if the previous result was successful
-- kind like then from promises
ifItWorked :: forall a b e
  .  Either e a
  -> (a -> Async (Either e b))
  -> Async (Either e b)
ifItWorked maybeA f =
  case maybeA of
    Left err -> pure $ Left err
    Right c  -> f c


withError :: forall e e' a
  .  Async (Either e a)
  -> (e -> e')
  -> Async (Either e' a)
withError a fe = lmap fe <$> a
