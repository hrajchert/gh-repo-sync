module Control.Github.Api
  -- ( getRepo
  -- , GetRepoErrors(..)
  -- , getBranchProtection
  -- , GetBranchProtectionErrors(..)
  -- , getBranchProtectionSettings -- TODO: Move to a Settings
  -- )
   where

import Prelude
import Control.Async (Async)
import Effect.Aff (runAff)
import Control.Monad.Cont (ContT(ContT))
import Effect (Effect)
import Effect.Exception (Error, message, error)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository, parseRepository)
import Data.Github.Api.BranchProtection (ApiError(..), BranchProtection(..), RequiredPullRequestReviews, RequiredStatusCheck, parseBranchProtection, parseApiError)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Affjax (Request, defaultRequest, Response)
import Affjax as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat (ResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Foreign (MultipleErrors)
import Data.Github.Settings.BranchProtection (BranchProtectionSettings(..), PullRequestReviewSettings, StatusChecksSettings)

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

authHeader :: String -> RequestHeader
authHeader token = RequestHeader "Authorization" ("Bearer " <> token)

acceptHeader :: String -> RequestHeader
acceptHeader negotiation = RequestHeader "Accept" negotiation


addAccessTokenIfPresent :: Maybe String -> Array RequestHeader  -> Array RequestHeader
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


---------------------------
-- get Repository
---------------------------
-- Githubs documentation https://developer.github.com/v3/repos/#get

getRepo
  :: Maybe String -- Access token
  -> String       -- Organization name
  -> String       -- Repository name
  -> Async (Either GetRepoErrors Repository)
getRepo maybeAccessToken org repo =
  -- Request the url and transform both the error and the result
  transformResponse <$> requestCont req
    where
      req :: Request String
      req = defaultRequest  { url = (apiRepoUrl org repo)
                            , headers = addAccessTokenIfPresent maybeAccessToken []
                            , method = Left GET
                            , responseFormat = ResponseFormat.string
                            }

      transformResponse :: Either Error (Response String) -> Either GetRepoErrors Repository
      transformResponse (Left err)  = Left (InternalError $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse (parseRepository val.body)
        401 -> Left InvalidCredentials
        404 -> Left (RepoNotFound $ siteRepoUrl org repo)
        n   -> Left (InternalError $ "Unexpected status code " <> show n)

      interpretParsedResponse :: Either MultipleErrors Repository -> Either GetRepoErrors Repository
      interpretParsedResponse (Left err) = Left (InvalidResponse err)
      interpretParsedResponse (Right r) = Right r

      apiRepoUrl :: String -> String -> String
      apiRepoUrl owner repo' = api $ "repos/" <> owner <> "/" <> repo'

      siteRepoUrl :: String -> String -> String
      siteRepoUrl owner repo' = site $ owner <> "/" <> repo'

data GetRepoErrors
  = InternalError String
  | RepoNotFound String
  | InvalidResponse MultipleErrors
  | InvalidCredentials

instance explainGetRepoErrors :: Explain GetRepoErrors where
  explain :: GetRepoErrors -> String
  explain (InternalError str)    = "There was an internal error: " <> str
  explain (RepoNotFound repoUrl) = "The repository (" <> repoUrl <> ") is not found, maybe it's private?"
  explain (InvalidResponse err)  = "Github response doesn't match what we expected: " <> explain err
  explain InvalidCredentials     = "The access token you provided is invalid or cancelled"

instance showGetRepoErrors :: Show GetRepoErrors  where
  show (InternalError e) = "(InternalError " <> e <> ")"
  show (RepoNotFound e) = "(RepoNotFound " <> e <> ")"
  show (InvalidResponse e) = "(InvalidResponse " <> show e <> ")"
  show InvalidCredentials = "InvalidCredentials"

getBranchProtectionSettings
  :: String -- Access token
  -> String -- Organization name
  -> String -- Repository name
  -> String -- Branch name
  -> Async (Either GetBranchProtectionErrors BranchProtectionSettings)
getBranchProtectionSettings accessToken org repo branch =
  -- Make the API call and interpret the response
  interpretResponse <$> getBranchProtection accessToken org repo branch
    where
      interpretResponse :: Either GetBranchProtectionErrors BranchProtection -> Either GetBranchProtectionErrors BranchProtectionSettings
      interpretResponse (Left GetBranchNotProtected) = pure BranchNotProtected
      interpretResponse (Left error) = Left error
      interpretResponse (Right (BranchProtection
                                  { required_pull_request_reviews: maybePR
                                  , required_status_checks: maybeSC
                                  , required_signatures
                                  , enforce_admins
                                  }
                                )
                        ) = pure $ ProtectedBranch
                                    { pullRequestReview    : interpretPullRequestReview maybePR
                                    , statusChecks         : interpretStatusCheck maybeSC
                                    , requireSignedCommits : required_signatures.enabled
                                    , includeAdministrators: enforce_admins.enabled
                                    }


      interpretPullRequestReview :: Maybe RequiredPullRequestReviews -> Maybe PullRequestReviewSettings
      interpretPullRequestReview Nothing = Nothing
      interpretPullRequestReview
          (Just
            { required_approving_review_count: requiredApprovingReviews
            , dismiss_stale_reviews: dismissStale
            , require_code_owner_reviews: requireReviewFromOwner
            }
          ) = Just
                { requiredApprovingReviews
                , dismissStale
                , requireReviewFromOwner
                }

      interpretStatusCheck :: Maybe RequiredStatusCheck -> Maybe StatusChecksSettings
      interpretStatusCheck Nothing = Nothing
      interpretStatusCheck
        (Just
          { strict: requireUpToDate
          , contexts: checks
          }
        ) = Just
              { requireUpToDate
              , checks
              }


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

      transformResponse :: Either Error (Response String) -> Either GetBranchProtectionErrors BranchProtection
      transformResponse (Left err)  = Left (InternalError' $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse (parseBranchProtection val.body)
        401 -> Left InvalidCredentials'
        404 -> interpret404Response (parseApiError val.body)
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

      endpointUrl :: String -> String -> String -> String
      endpointUrl owner repo' branch' = api $ "repos/" <> owner <> "/" <> repo' <> "/branches/" <> branch' <> "/protection"


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

