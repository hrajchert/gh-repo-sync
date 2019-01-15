module Github.Api.Api
  -- ( getRepo
  -- , GetRepoErrors(..)
  -- , getBranchProtection
  -- , GetBranchProtectionErrors(..)
  -- , getBranchProtectionSettings -- TODO: Move to a Settings
  -- )
   where

import Prelude

import Affjax (Request, Response)
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.StatusCode (StatusCode(..))
import Control.Async (Async)
import Control.Monad.Cont (ContT(ContT))
import Data.Either (Either(..))
import Data.JSON.ParseForeign (class ParseForeign)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Exception (Error, error)
import Data.Show (class Show)
---------------------------
-- REQUEST
---------------------------
request
  :: forall a
  . Request a
  -> (Either Error (Response a) -> Effect Unit)
  -> Effect Unit
-- Run the Aff and ignore the resulting fiber
request req outercb = runAff innercb (Affjax.request req) *> pure unit where
  -- Simplify the outercb signature by treating ResponseFormatError as a normal Error
  innercb :: (Either Error (Response (Either ResponseFormatError a))) -> Effect Unit
  innercb (Left err) = outercb (Left err)
  innercb (Right {body: (Left err)}) = outercb (Left (error "Incorrect format"))
  innercb
    (Right
      { body: (Right a)
      , status
      , statusText
      , headers
      }
    ) = outercb (Right
              { body: a
              , status
              , statusText
              , headers
              }
           )


requestCont
  :: forall a
  . Request a
  -> Async (Either Error (Response a))
requestCont req = ContT (\cb -> request req cb)

-- Some methods are restricted and needs an  Access Token
newtype AccessToken = AccessToken String

-- Instances for parsing with SimpleJSON
derive instance newtypeAccessToken :: Newtype AccessToken _
derive newtype instance parseForeignAccessToken :: ParseForeign AccessToken

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

---------------------------
-- API
---------------------------
api :: String -> String
api url = "https://api.github.com/" <> url

site :: String -> String
site url = "https://www.github.com/" <> url

-- TODO: Try to add conditional request
-- https://developer.github.com/v3/#conditional-requests






