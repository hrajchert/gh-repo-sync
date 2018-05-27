module Control.Async
  ( Async
  , ifItWorked
  , withError
  )
   where

import Prelude 

import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)

-- Async eff a = (Either Error a -> Eff eff Unit) -> Eff eff Unit
type Async eff = ContT Unit (Eff eff)


-- Helper functions to work with Async (Either e a)

-- Helps with do notation, runs the async f if the previous result was successful
-- kind like then from promises
ifItWorked :: forall a b e eff
  .  Either e a 
  -> (a -> Async eff (Either e b)) 
  -> Async eff (Either e b)
ifItWorked maybeA f = 
  case maybeA of
    Left err -> pure $ Left err
    Right c  -> f c


withError :: forall eff e e' a 
  .  Async eff (Either e a) 
  -> (e -> e') 
  -> Async eff (Either e' a)
withError a fe = lmap fe <$> a 
