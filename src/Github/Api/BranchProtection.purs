module Github.Api.BranchProtection
  ( BranchProtection(..)
  , getBranchProtection
  , GetBranchProtectionErrors(..)
  , RequiredPullRequestReviews
  , DismissalRestrictions
  , RequiredStatusCheck
  , EnforceAdmins
  , RequiredSignatures
  , Restrictions
  , User
  , Team
  )
where

import Prelude

import Affjax (Request, defaultRequest, Response)
import Affjax.ResponseFormat as ResponseFormat
import Control.Async (Async)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.HTTP.Method (Method(..))
import Data.JSON.ParseForeign (class ParseForeign, parseForeign, readJSON)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, message)
import Foreign (F, MultipleErrors, Foreign)
import Github.Api.Api (acceptHeader, api, authHeader, getStatusCode, requestCont)

---------------------------
-- get Branch protection
---------------------------
-- Githubs documentation: https://developer.github.com/v3/repos/branches/#get-branch-protection

getBranchProtection
  :: String -- Access token
  -> String -- Organization name
  -> String -- Repository name
  -> String -- Branch name
  -> Async (Either GetBranchProtectionErrors BranchProtection)
getBranchProtection accessToken org repo branch =
  -- Request the url and transform both the error and the result
  transformResponse <$> requestCont req
    where
      req :: Request String
      req = defaultRequest  { url = (endpointUrl org repo branch)
                            , headers =
                              [ authHeader accessToken
                              -- This header is here to get required_approving_review_count info
                              -- as stated in a warning here: https://developer.github.com/v3/repos/branches/#get-branch-protection
                              , acceptHeader "application/vnd.github.luke-cage-preview+json"
                              -- This header is here to get required_signatures info
                              -- as stated here: https://developer.github.com/v3/repos/branches/#get-required-signatures-of-protected-branch
                              , acceptHeader "application/vnd.github.zzzax-preview+json"
                              ]
                            , method = Left GET
                            , responseFormat = ResponseFormat.string
                            }

      endpointUrl :: String -> String -> String -> String
      endpointUrl owner repo' branch' = api $ "repos/" <> owner <> "/" <> repo' <> "/branches/" <> branch' <> "/protection"

      transformResponse :: Either Error (Response String) -> Either GetBranchProtectionErrors BranchProtection
      transformResponse (Left err)  = Left (InternalError' $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse (readJSON val.body)
        401 -> Left InvalidCredentials'
        404 -> interpret404Response (readJSON val.body)
        n   -> Left (InternalError' $ "Unexpected status code " <> show n)

      interpretParsedResponse :: Either MultipleErrors BranchProtection -> Either GetBranchProtectionErrors BranchProtection
      interpretParsedResponse (Left err) = Left (InvalidResponse' err)
      interpretParsedResponse (Right bp) = Right bp

      interpret404Response :: Either MultipleErrors ApiError -> Either GetBranchProtectionErrors BranchProtection
      interpret404Response (Left err) = Left (InvalidResponse' err)
      interpret404Response (Right (ApiError r)) = case r.message of
          "Branch not protected" -> Left GetBranchNotProtected
          "Branch not found" -> Left (BranchNotFound org repo branch)
          _ -> Left (InternalError' "invalid response")



data GetBranchProtectionErrors
  = InternalError' String
  | BranchNotFound String String String
  | GetBranchNotProtected
  | InvalidResponse' MultipleErrors
  | InvalidCredentials'

instance explainGetBranchProtectionErrors :: Explain GetBranchProtectionErrors where
  explain :: GetBranchProtectionErrors -> String
  explain (InternalError' str)    = "There was an internal error: " <> str
  explain (BranchNotFound owner repo branch) = "The branch @" <> owner <> "/" <> repo <> "#" <> branch <> " is not found"
  explain (GetBranchNotProtected) = "The branch is not protected"
  explain (InvalidResponse' err)  = "Github response doesn't match what we expected: " <> explain err
  explain InvalidCredentials'     = "The access token you provided is invalid or cancelled"

newtype BranchProtection = BranchProtection
  { url                           :: String
  , required_status_checks        :: Maybe RequiredStatusCheck
  , enforce_admins                :: EnforceAdmins
  , required_signatures           :: RequiredSignatures
  , required_pull_request_reviews :: Maybe RequiredPullRequestReviews
  , restrictions                  :: Maybe Restrictions
  }

derive instance newtypeBranchProtection :: Newtype BranchProtection _

derive newtype instance parseForeignBranchProtection :: ParseForeign BranchProtection

instance showBranchProtection :: Show BranchProtection  where
  show (BranchProtection c) = "(BranchProtection " <> c.url <> ")"


type EnforceAdmins =
  { url     :: String
  , enabled :: Boolean
  }

type RequiredSignatures =
  { url     :: String
  , enabled :: Boolean
  }

type RequiredStatusCheck =
  { url           :: String
  , strict        :: Boolean
  , contexts      :: Array String
  , contexts_url  :: String
  }

type RequiredPullRequestReviews =
  { url                             :: String
  , dismissal_restrictions          :: Maybe DismissalRestrictions
  , dismiss_stale_reviews           :: Boolean
  , require_code_owner_reviews      :: Boolean
  , required_approving_review_count :: Int
  }

type DismissalRestrictions =
  { url       :: String
  , users_url :: String
  , teams_url :: String
  , users     :: Array User
  , teams     :: Array Team
  }

type User =
  { login               :: String
  , id                  :: Number
  , avatar_url          :: String
  , gravatar_id         :: String
  , url                 :: String
  , html_url            :: String
  , followers_url       :: String
  , following_url       :: String
  , gists_url           :: String
  , starred_url         :: String
  , subscriptions_url   :: String
  , organizations_url   :: String
  , repos_url           :: String
  , events_url          :: String
  , received_events_url :: String
  , type                :: String
  , site_admin          :: Boolean
  }

type Team =
  { id                :: Number
  , url               :: String
  , name              :: String
  , slug              :: String
  , description       :: String
  , privacy           :: String
  , permission        :: String
  , members_url       :: String
  , repositories_url  :: String
  -- , parent            :: Maybe Team -- This hangs the compiler :(
  }

type Restrictions =
  { url :: String
  , users_url :: String
  , teams_url :: String
  , users :: Array User
  , teams :: Array Team
  }

-- ************
-- * ApiError *
-- ************

type ApiErrorData =
  { message           :: String
  , documentation_url :: String
  }

parseApiErrorData :: Foreign -> F ApiErrorData
parseApiErrorData = parseForeign

instance parseForeignApiError :: ParseForeign ApiError where
  parseForeign f = ApiError <$> parseApiErrorData f

newtype ApiError = ApiError ApiErrorData

derive instance newtypeApiError :: Newtype ApiError _

instance showApiError :: Show ApiError  where
  show (ApiError e) = "(ApiError " <> e.message <> ")"

