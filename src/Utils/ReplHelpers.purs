module Utils.ReplHelpers
  ( try
  , try'
  ) where

import Prelude
import Control.Async (Async, runAsync)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Console (log)

try ::
  ∀ e a.
  Show a =>
  Explain (Variant e) =>
  Async e a ->
  Effect Unit
try program = runAsync program resultCb
  where
  resultCb =
    ( \m -> case m of
        Left err -> log $ "Fail': " <> explain err
        Right result -> log $ "Ok': " <> show result
    )

try' ::
  ∀ e.
  Explain (Variant e) =>
  Async e String ->
  Effect Unit
try' program = runAsync program resultCb
  where
  resultCb =
    ( \m -> case m of
        Left err -> log $ "Fail: " <> explain err
        Right result -> log $ "Ok: " <> result
    )
