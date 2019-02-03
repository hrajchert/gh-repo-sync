module Control.Async
  ( Async
  , mapExceptT'
  , throwErrorV
  , runAsync
  , exception
  , Exception
  , errorMessage
  , ErrorMessage
  )
where

import Prelude

import Control.Monad.Cont.Trans (ContT, runContT)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except as ExceptT
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Variant (SProxy(..), Variant, inj)
import Effect (Effect)
import Effect.Exception (Error)
import Type.Row (RowApply)

-- | Represents an asynchronous computation that can fail with an error defined in a `Variant ρ`
-- | or succeed yielding a value of type `a`.
-- |
-- | The variable `ρ` (Greek letter Rho) is a [Row Type](https://github.com/purescript/documentation/blob/master/language/Types.md#rows),
-- | and it is used with ExceptV, from the package [checked-exceptions](https://github.com/natefaubion/purescript-checked-exceptions),
-- | to model Extensible errors.
-- |
-- | Instead of defining errors with data which is a closed Set (it can't be extended)
-- | ```purescript
-- | data ApiError
-- |   = RequestError Error
-- |   | InvalidCredentials
-- |   | InvalidResponse MultipleErrors
-- | ```
-- |
-- | We can define our errors as an extensible Set, so we can use our computation to generate other results
-- | and in the process add, or resolve errors from the Set.
-- | ```purescript
-- | type ApiError ρ = (RequestError ⋃ InvalidCredentials ⋃ InvalidResponse ⋃ ρ)
-- | ```
type Async ρ a =  ExceptV ρ (ContT Unit Effect) a

-- | Used to join error types together, for example (ErrorMessage ⋃ Exception ⋃ ρ)
-- |
-- | **unicode** `option+22C3`
infixr 0 type RowApply as ⋃

-- | Empty Set
-- |
-- | **unicode** `option+00D8`
type Ø = ()
-------------------------------------------------------------------------------

-- | Runs an asynchronous computation
runAsync :: ∀ a e. Async e a -> (Either (Variant e) a -> Effect Unit) -> Effect Unit
runAsync =  runContT <<< runExceptT

-----

-- | Throws an error, restricting the error type to a Variant
throwErrorV :: ∀ a m e. MonadThrow (Variant e) m => Variant e -> m a
throwErrorV = ExceptT.throwError

-----

-- | Maps the error of a Monad Stack that includes the ExceptT Transformer.
-- | It's a helper around mapExceptT, to only concentrate about mapping the error.
-- | I'm not sure if this function should reside here.
mapExceptT' :: ∀ e e' a m
  . Functor m
  =>  ExceptT e m a
  -> (e -> e')
  -> ExceptT e' m a
mapExceptT' stack fe = ExceptT.mapExceptT map' stack where
  map' :: (m (Either e a) -> m (Either e' a))
  map' m = lmap fe <$> m


-------------------------------------------------------------------------------
-- Common Errors

-- | An ErrorMessage is a simple string error.
-- | It should be used whenever you want to fail with a simple reason explaining
-- | the failure.
type ErrorMessage ρ = (message ∷ String | ρ)

-- | Error constructor
errorMessage :: ∀ ρ. String -> Variant (ErrorMessage ρ)
errorMessage message = inj (SProxy :: SProxy "message") message

-----

-- | An Exception is a JavaScript Error object, which you can then message or see the Runtime stack
type Exception ρ = (exception ∷ Error | ρ)

-- | Error constructor
exception :: ∀ ρ. Error -> Variant (Exception ρ)
exception e = inj (SProxy :: SProxy "exception") e

