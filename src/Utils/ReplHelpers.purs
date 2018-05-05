module Utils.ReplHelpers
  ( try
  )
where

import Prelude
import Control.Async (Async)
import Control.File as File
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Control.Github.Api (GetRepoErrors, getRepo)
import Control.Monad.Eff (Eff)

try
  :: forall eff err ok. Explain err => Show ok
  => Async (console :: CONSOLE | eff) (Either err ok)
  -> Eff (console :: CONSOLE | eff) Unit
try program = runContT program resultCb where
  resultCb = (\m -> case m of
      Left err     -> log $ "Fail: " <> explain err
      Right result -> log $ "Ok: "   <> show result
  )
