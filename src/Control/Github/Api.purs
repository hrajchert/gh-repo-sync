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
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Cont (ContT(ContT))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, message)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.Github.Repository (Repository, RepositoryParse(..))
import Data.Github.Api.BranchProtection (BranchProtectionParse(..), BranchProtectionBodyParse(..), ApiError(..), BranchProtection(..), RequiredPullRequestReviews, RequiredStatusCheck)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (affjax, defaultRequest, AffjaxRequest, AffjaxResponse, AJAX)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Foreign (MultipleErrors)
import Data.Github.Settings.BranchProtection (BranchProtectionSettings(..), PullRequestReviewSettings, StatusChecksSettings)

---------------------------
-- REQUEST
---------------------------
request
  :: forall eff a b. Requestable a => Respondable b
  =>  AffjaxRequest a
  -> (Either Error (AffjaxResponse b) -> Eff (ajax :: AJAX | eff) Unit)
  -> Eff (ajax :: AJAX | eff) Unit
request req cb = runAff cb aff *> pure unit where
  -- innerCb :: (Either Error (AffjaxResponse b) → Eff (ajax :: AJAX | eff) Unit)
  -- innerCb (Left err)  = cb $ Left err
  -- innerCb (Right val) = cb $ Right val

  aff :: Aff (ajax :: AJAX | eff) (AffjaxResponse b)
  aff = affjax req

requestCont
  :: forall eff a b. Requestable a => Respondable b
  => AffjaxRequest a
  -> Async (ajax :: AJAX | eff) (Either Error (AffjaxResponse b))
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
  :: forall eff
  .  Maybe String -- Access token
  -> String       -- Organization name
  -> String       -- Repository name
  -> Async (ajax :: AJAX | eff) (Either GetRepoErrors Repository)
getRepo maybeAccessToken org repo =
  -- Request the url and transform both the error and the result
  transformResponse <$> requestCont req
    where
      req = defaultRequest  { url = (apiRepoUrl org repo)
                            , headers = addAccessTokenIfPresent maybeAccessToken []
                            , method = Left GET
                            }

      transformResponse :: Either Error (AffjaxResponse RepositoryParse) -> Either GetRepoErrors Repository
      transformResponse (Left err)  = Left (InternalError $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse val.response
        401 -> Left InvalidCredentials
        404 -> Left (RepoNotFound $ siteRepoUrl org repo)
        n   -> Left (InternalError $ "Unexpected status code " <> show n)

      interpretParsedResponse :: RepositoryParse -> Either GetRepoErrors Repository
      interpretParsedResponse (RepositoryParse (Left err)) = Left (InvalidResponse err)
      interpretParsedResponse (RepositoryParse (Right r)) = Right r

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
  :: forall eff
  .  String -- Access token
  -> String -- Organization name
  -> String -- Repository name
  -> String -- Branch name
  -> Async (ajax :: AJAX | eff) (Either GetBranchProtectionErrors BranchProtectionSettings)
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
  :: forall eff
  .  String -- Access token
  -> String -- Organization name
  -> String -- Repository name
  -> String -- Branch name
  -> Async (ajax :: AJAX | eff) (Either GetBranchProtectionErrors BranchProtection)
getBranchProtection accessToken org repo branch =
  -- Request the url and transform both the error and the result
  transformResponse <$> requestCont req
    where
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
                            }

      transformResponse :: Either Error (AffjaxResponse BranchProtectionParse) -> Either GetBranchProtectionErrors BranchProtection
      transformResponse (Left err)  = Left (InternalError' $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse val.response
        401 -> Left InvalidCredentials'
        404 -> interpret404Response val.response
        n   -> Left (InternalError' $ "Unexpected status code " <> show n)

      interpretParsedResponse :: BranchProtectionParse -> Either GetBranchProtectionErrors BranchProtection
      interpretParsedResponse (BranchProtectionParse (Left err)) = Left (InvalidResponse' err)
      interpretParsedResponse (BranchProtectionParse (Right (Error r))) = Left (InternalError' "invalid response") -- TODO: change to invalidResponse
      interpretParsedResponse (BranchProtectionParse (Right (Branch r))) = Right r

      interpret404Response :: BranchProtectionParse -> Either GetBranchProtectionErrors BranchProtection
      -- TODO: right now this error is here to allow it to compile, but it should check wheter the message is
      -- "Branch not protected" or "Branch not found"
      interpret404Response (BranchProtectionParse (Left err)) = Left (InvalidResponse' err)
      interpret404Response (BranchProtectionParse (Right (Branch r))) = Left (InternalError' "shoudnt parse a response if 404")  -- TODO: change to invalidResponse
      interpret404Response (BranchProtectionParse (Right (Error (ApiError r)))) = case r.message of
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

