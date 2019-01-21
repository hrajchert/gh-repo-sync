module Github.Api.Api
  ( request
  , AccessToken(..)
  , authHeader
  , acceptHeader
  , addAccessTokenIfPresent
  , getStatusCode
  , parseResponse
  , api
  , site
  , RequestError
  , RequestErrorImpl
  , requestInternalError
  , InvalidResponse
  , InvalidResponseImpl
  , invalidResponse
  , InvalidCredentials
  , InvalidCredentialsImpl
  , invalidCredentials
  )
   where

import Prelude

import Affjax (Request, Response)
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Async (Async, mapExceptT', throwErrorV)
import Control.Monad.Cont (ContT(ContT))
import Control.Monad.Except (ExceptT(..), except)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, inj, SProxy(..))
import Effect (Effect)
import Effect.Aff (runAff_, Aff)
import Effect.Exception (Error, message)
import Foreign (MultipleErrors)
import Simple.JSON (class ReadForeign, readJSON)
import Type.Row (RowApply)

-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃


request
  :: ∀ a e
  . Request a
  -> Async (RequestError ⋃ e) (Response a)
request req = asyncRequestV >>= interpretResponse
    where
      -- Note that Affjax has two places for error handling:
      --   * The first is the implicit one from Aff. This error happens when there is no internet,
      --     or there is a problem with the DNS, etc.
      --   * The second error is explicit in the response, and it happens when the response can't
      --     be formated acording to the type `a`, described in the Request

      -- Asyncronous computation of our Response
      aff :: Aff (Response (Either ResponseFormatError a))
      aff = Affjax.request req

      -- Convert the aff computation into a Monad Transformer closer to Async
      asyncRequestT :: ExceptT Error (ContT Unit Effect) (Response (Either ResponseFormatError a))
      asyncRequestT = ExceptT $ ContT $ flip runAff_ $ aff

      -- Convert the computation to Async by interpreting (Aff Error) into the (Variant RequestError)
      asyncRequestV :: Async (RequestError ⋃ e) (Response (Either ResponseFormatError a))
      asyncRequestV = asyncRequestT `mapExceptT'` requestInternalError req

      -- Once we have the response, interpret the (Either ResponseFormatError) as a (Variant RequestError)
      interpretResponse :: Response (Either ResponseFormatError a) -> Async (RequestError ⋃ e) (Response a)
      interpretResponse res =
        case res.body of
          Left formatError -> throwErrorV $ requestIncorrectFormatError req formatError
                           -- If the value is correct, remove the Either from the Response
          Right a          -> lift $ pure $ res {body = a}

-------------------------------------------------------------------------------
-- HEADERS

-- Some methods are restricted and needs an  Access Token
newtype AccessToken = AccessToken String

-- Instances for parsing with SimpleJSON
derive instance newtypeAccessToken :: Newtype AccessToken _
derive newtype instance readForeignAccessToken :: ReadForeign AccessToken

instance showAccessToken :: Show AccessToken where
  show _ = "<access-token>"

authHeader :: AccessToken -> RequestHeader
authHeader (AccessToken token) = RequestHeader "Authorization" ("Bearer " <> token)

acceptHeader :: String -> RequestHeader
acceptHeader negotiation = RequestHeader "Accept" negotiation

addAccessTokenIfPresent :: Maybe AccessToken -> Array RequestHeader  -> Array RequestHeader
addAccessTokenIfPresent Nothing            headers = headers
addAccessTokenIfPresent (Just accessToken) headers = headers <> [authHeader accessToken]

getStatusCode :: StatusCode -> Int
getStatusCode (StatusCode n) = n

-------------------------------------------------------------------------------
-- URLS

api :: String -> String
api url = "https://api.github.com/" <> url

site :: String -> String
site url = "https://www.github.com/" <> url

-- TODO: Try to add conditional request
-- https://developer.github.com/v3/#conditional-requests


-------------------------------------------------------------------------------
-- ERRORS

-- | Error thrown when a affjax request cant be resolved
type RequestError ρ = (requestError ∷ RequestErrorImpl | ρ)

-- There are two reasons why the request can fail. An internal error normally represents no internet, DNS
-- problem, etc. IncorrectFormat is when parsing the response, if it can't be formated as the request intended
data RequestErrorImpl
  = IncorrectFormat ResponseFormatError
  | InternalError Error


instance explainRequestError :: Explain RequestErrorImpl where
  explain (IncorrectFormat formatError) = "Incorrect format"
  explain (InternalError error) = "Internal error: " <> message error

-- | Error constructors for the Variant RequestError
requestIncorrectFormatError :: ∀ a ρ. Request a -> ResponseFormatError -> Variant (RequestError ⋃ ρ)
requestIncorrectFormatError req formatError = inj (SProxy :: SProxy "requestError") (IncorrectFormat formatError)

requestInternalError :: ∀ a ρ. Request a -> Error -> Variant (RequestError ⋃ ρ)
requestInternalError req error = inj (SProxy :: SProxy "requestError") (InternalError error)

---------------------------------------

type InvalidResponse ρ = (invalidResponse ∷ InvalidResponseImpl | ρ)

data InvalidResponseImpl
  = InvalidResponseImpl MultipleErrors

instance explainInvalidResponse :: Explain InvalidResponseImpl where
  explain (InvalidResponseImpl err)  = "Github response doesn't match what we expected: " <> explain err

-- | Error constructors for the Variant InvalidResponse
invalidResponse :: ∀ ρ. MultipleErrors -> Variant (InvalidResponse ⋃ ρ)
invalidResponse err = inj (SProxy :: SProxy "invalidResponse") (InvalidResponseImpl err)

parseResponse :: ∀ a ρ. ReadForeign a => String -> Async (InvalidResponse ρ) a
parseResponse str = except parsedJSON where
    -- Parse the JSON and convert the error
    parsedJSON = readJSON str # lmap invalidResponse

---------------------------------------

-- | This error is usually tied to error 401
type InvalidCredentials ρ = (invalidCredentials ∷ InvalidCredentialsImpl | ρ)

data InvalidCredentialsImpl = InvalidCredentialsImpl

instance explainInvalidCredentials :: Explain InvalidCredentialsImpl where
  explain InvalidCredentialsImpl  = "The access token you provided is invalid or cancelled"

-- | Error constructors for the Variant InvalidCredentials
invalidCredentials :: ∀ ρ. Variant (InvalidCredentials ⋃ ρ)
invalidCredentials = inj (SProxy :: SProxy "invalidCredentials") InvalidCredentialsImpl



