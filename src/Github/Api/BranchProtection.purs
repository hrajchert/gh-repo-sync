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
  , BranchNotFoundImpl
  , BranchNotProtectedImpl
  , BranchNotFound
  , BranchNotProtected
  )
where

-- Rename the module to Branches

import Prelude

import Affjax (Request, defaultRequest, Response)
import Affjax.ResponseFormat as ResponseFormat
import Control.Async (Async, throwErrorV)
import Data.Either (Either(..))
import Data.Explain (class Explain)
import Data.HTTP.Method (Method(..))
import Data.JSON.ParseForeign (class ParseForeign, parseForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect.Exception (error)
import Foreign (F, Foreign)
import Github.Api.Api (AccessToken, InvalidCredentials, InvalidResponse, RequestError, acceptHeader, api, authHeader, getStatusCode, invalidCredentials, parseResponse, request, requestInternalError)
import Github.Entities (BranchName, OrgName, RepoName)
import Type.Row (RowApply)
import Data.Variant (SProxy(..), Variant, inj)

-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃


type GetBranchProtectionErrors e =
  ( RequestError
  ⋃ InvalidResponse
  ⋃ InvalidCredentials
  ⋃ BranchNotFound
  ⋃ BranchNotProtected
  ⋃ e
  )

-- Github documentation: https://developer.github.com/v3/repos/branches/#get-branch-protection
getBranchProtection
  :: ∀ e
  .  AccessToken
  -> OrgName
  -> RepoName
  -> BranchName
  -> Async (GetBranchProtectionErrors e) BranchProtection
getBranchProtection accessToken org repo branch =
  -- Make the request and interpret the response
  request req >>= transformResponse
    where
      endpointUrl :: String
      endpointUrl = api $ "repos/" <> unwrap org <> "/" <> unwrap repo <> "/branches/" <> unwrap branch <> "/protection"

      req :: Request String
      req = defaultRequest  { url = endpointUrl
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

      transformResponse :: Response String -> Async (GetBranchProtectionErrors e) BranchProtection
      transformResponse res = case getStatusCode res.status of
        200 -> parseResponse res.body
        401 -> throwErrorV invalidCredentials
        404 -> parseResponse res.body >>= interpret404Response
        n   -> throwErrorV $ requestInternalError req $ error $ "Unexpected status code " <> show n

      interpret404Response :: ApiError -> Async (GetBranchProtectionErrors e) BranchProtection
      interpret404Response (ApiError r) = case r.message of
          "Branch not protected" -> throwErrorV branchNotProtected
          "Branch not found" -> throwErrorV $ branchNotFound org repo branch
          msg -> throwErrorV $ requestInternalError req $ error $ "invalid error message: " <> show msg


-- TODO: Rename to BranchProtectionResponse
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

-------------------------------------------------------------------------------
-- ERRORS

type BranchNotProtected ρ = (branchNotProtected ∷ BranchNotProtectedImpl | ρ)

data BranchNotProtectedImpl = BranchNotProtectedImpl


-- | Error constructors for the Variant BranchNotProtected
branchNotProtected :: ∀ ρ. Variant (BranchNotProtected ⋃ ρ)
branchNotProtected = inj (SProxy :: SProxy "branchNotProtected") (BranchNotProtectedImpl)
-------------------------------------------------------------------------------

type BranchNotFound ρ = (branchNotFound ∷ BranchNotFoundImpl | ρ)

data BranchNotFoundImpl = BranchNotFoundImpl OrgName RepoName BranchName

branchURI :: OrgName -> RepoName -> BranchName -> String
branchURI owner repo' branch' = "@" <> unwrap owner <> "/" <> unwrap repo' <> "#" <> unwrap branch'

instance explainBranchNotFoundImpl :: Explain BranchNotFoundImpl where
  explain (BranchNotFoundImpl org repo branch) = "The branch " <> branchURI org repo branch <> " is not found"

-- | Error constructors for the Variant BranchNotFound
branchNotFound :: ∀ ρ. OrgName -> RepoName -> BranchName -> Variant (BranchNotFound ⋃ ρ)
branchNotFound org repo branch = inj (SProxy :: SProxy "branchNotFound") (BranchNotFoundImpl org repo branch)
-------------------------------------------------------------------------------

-- ************
-- * ApiError *
-- ************
-- TODO: Refactor and put in Api
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

