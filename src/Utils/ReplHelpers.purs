module Utils.ReplHelpers
  ( try
  , try'
  )
where

import Prelude
import Control.Async (Async)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)

try
  :: forall eff err ok. Explain err => Show ok
  => Async (console :: CONSOLE | eff) (Either err ok)
  -> Eff (console :: CONSOLE | eff) Unit
try program = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Fail: " <> explain err
      Right result -> log $ "Ok: "   <> show result
  )

try'
  :: forall eff err ok. Explain err
  => Async (console :: CONSOLE | eff) (Either err String)
  -> Eff (console :: CONSOLE | eff) Unit
try' program = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Fail: " <> explain err
      Right result -> log $ "Ok: "   <> result
  )
