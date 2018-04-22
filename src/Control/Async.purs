module Control.Async
  ( Async
  )
   where


import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Eff (Eff)
import Prelude (Unit)

-- Async eff a = (Either Error a -> Eff eff Unit) -> Eff eff Unit
type Async eff = ContT Unit (Eff eff)
