module Utils.ReplHelpers
  ( try
  , try'
  )
where

import Prelude
import Control.Async (Async)
import Control.Monad.Cont.Trans (runContT)
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)

try
  :: forall err ok. Explain err => Show ok
  => Async (Either err ok)
  -> Effect Unit
try program = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Fail: " <> explain err
      Right result -> log $ "Ok: "   <> show result
  )

try'
  :: forall err. Explain err
  => Async (Either err String)
  -> Effect Unit
try' program = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Fail: " <> explain err
      Right result -> log $ "Ok: "   <> result
  )
