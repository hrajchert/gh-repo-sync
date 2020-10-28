module Github.Settings.BranchProtection
  ( BranchProtectionSettings(..)
  , ProtectedBranchSettings
  , PullRequestReviewSettings
  , StatusChecksSettings
  , getBranchProtectionSettings
  , setBranchProtectionSettings
  , GetBPSettingsErrors
  , SyncBPSettingsErrors
  , syncBranchProtectionSettings
  ) where

import Prelude
import Control.Async (Async)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Control.Monad.Except.Checked (handleError)
import Simple.JSON (class ReadForeign, readImpl)
import Data.Maybe (Maybe(..))
import Data.Rules (Rules, boolRule, maybeRules, rule)
import Foreign (F, Foreign)
import Github.Api.BranchProtection (BranchNotFound, BranchNotProtectedImpl, BranchProtection(..), RequiredPullRequestReviews, RequiredStatusCheck, getBranchProtection, UpdateBranchProtectionData(..), updateBranchProtection, removeBranchProtection)
import Github.Entities (BranchName, OrgName, RepoName, BranchObject)
import Github.Api.Api (AccessToken, InvalidCredentials, InvalidResponse, RequestError)
import Type.Row (RowApply)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (liftA1)
import Data.Nullable (Nullable, null, notNull, toNullable)

-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃

-- | Data structure that models the settings in this page
-- | https://github.com/<owner>/<repo>/settings/branches/<branch>
data BranchProtectionSettings
  = BranchNotProtected
  | ProtectedBranch ProtectedBranchSettings

type ProtectedBranchSettings
  = { pullRequestReview :: Maybe PullRequestReviewSettings
    , statusChecks :: Maybe StatusChecksSettings
    , requireSignedCommits :: Boolean
    , includeAdministrators :: Boolean
    }

type PullRequestReviewSettings
  = { requiredApprovingReviews :: Int -- TODO: see if we can restrict 1 to 6
    , dismissStale :: Boolean
    , requireReviewFromOwner :: Boolean
    }

type StatusChecksSettings
  = { requireUpToDate :: Boolean
    , checks :: Array String
    }

instance readForeignBranchSettings :: ReadForeign BranchProtectionSettings where
  readImpl :: Foreign -> F BranchProtectionSettings
  readImpl f = interpret <$> parsedSettings
    where
    parsedSettings :: F (Maybe ProtectedBranchSettings)
    parsedSettings = readImpl f

    interpret :: Maybe ProtectedBranchSettings -> BranchProtectionSettings
    interpret (Just settings) = ProtectedBranch settings

    interpret Nothing = BranchNotProtected

instance explainBranchSettings :: Explain BranchProtectionSettings where
  explain :: BranchProtectionSettings -> String
  explain BranchNotProtected = "The branch is not protected"
  explain (ProtectedBranch settings) = "The branch is protected in the following way: " <> (explain $ protectedBranchRules settings)

-------------------------------------------------------------------------------
-- TODO: probably groupBy into a specific error with a closed set
type GetBPSettingsErrors e
  = ( RequestError
        ⋃ InvalidResponse
        ⋃ InvalidCredentials
        ⋃ BranchNotFound
        ⋃ e
    )

getBranchProtectionSettings ::
  ∀ e.
  AccessToken ->
  OrgName ->
  RepoName ->
  BranchName ->
  Async (GetBPSettingsErrors e) BranchProtectionSettings
getBranchProtectionSettings accessToken org repo branch = -- Make the API call and interpret the response
  asyncWithError >>= interpretResponse
  where
  asyncWithError :: Async (GetBPSettingsErrors e) (Either BranchNotProtectedImpl BranchProtection)
  asyncWithError =
    getBranchProtection accessToken org repo branch
      -- If it succeeded, treat it as a right value
      
      <#> Right
      -- If it failed with "branchNotProtected", treat it as a left
      
      # handleError { branchNotProtected: (lift <<< pure <<< Left) }

  interpretResponse :: Either BranchNotProtectedImpl BranchProtection -> Async (GetBPSettingsErrors e) BranchProtectionSettings
  interpretResponse (Left _) = lift $ pure $ BranchNotProtected

  interpretResponse (Right bp) = lift $ pure $ fromBranchProtectionResponse bp

-------------------------------------------------------------------------------
fromBranchProtectionResponse :: BranchProtection -> BranchProtectionSettings
fromBranchProtectionResponse ( BranchProtection
    { required_pull_request_reviews: maybePR
  , required_status_checks: maybeSC
  , required_signatures
  , enforce_admins
  }
) =
  ProtectedBranch
    { pullRequestReview: liftA1 interpretPullRequestReview $ maybePR
    , statusChecks: liftA1 interpretStatusCheck $ maybeSC
    , requireSignedCommits: required_signatures.enabled
    , includeAdministrators: enforce_admins.enabled
    }

interpretPullRequestReview :: RequiredPullRequestReviews -> PullRequestReviewSettings
interpretPullRequestReview { required_approving_review_count: requiredApprovingReviews
, dismiss_stale_reviews: dismissStale
, require_code_owner_reviews: requireReviewFromOwner
} =
  { requiredApprovingReviews
  , dismissStale
  , requireReviewFromOwner
  }

interpretStatusCheck :: RequiredStatusCheck -> StatusChecksSettings
interpretStatusCheck { strict: requireUpToDate
, contexts: checks
} =
  { requireUpToDate
  , checks
  }

-------------------------------------------------------------------------------
type SetBPSettingsErrors e
  = ( RequestError
        ⋃ InvalidResponse
        ⋃ InvalidCredentials
        ⋃ BranchNotFound
        ⋃ e
    )

setBranchProtectionSettings ::
  ∀ e.
  AccessToken ->
  OrgName ->
  RepoName ->
  BranchName ->
  BranchProtectionSettings ->
  Async (SetBPSettingsErrors e) Unit
-- TODO: Maybe return the same settings to improve composability?
-- -> Async (SetBPSettingsErrors e) BranchProtectionSettings
setBranchProtectionSettings accessToken owner repo branch settings = case settings of
  BranchNotProtected -> removeBranchProtection accessToken owner repo branch
  (ProtectedBranch branchSettings) -> do
    _ <- updateBranchProtection accessToken owner repo branch (toUpdateBranchProtectionData branchSettings)
    pure unit

toUpdateBranchProtectionData :: ProtectedBranchSettings -> UpdateBranchProtectionData
toUpdateBranchProtectionData { pullRequestReview -- :: Maybe PullRequestReviewSettings
, statusChecks -- :: Maybe StatusChecksSettings
, requireSignedCommits -- :: Boolean
, includeAdministrators -- :: Boolean
} =
  UpdateBranchProtectionData
    { enforce_admins: includeAdministrators
    , required_signatures: requireSignedCommits
    , restrictions: null
    , required_status_checks: toNullable (convertStatusChecks <$> statusChecks)
    , required_pull_request_reviews: toNullable (convertPullRequestReviews <$> pullRequestReview)
    }
  where
  convertPullRequestReviews settings =
    { dismiss_stale_reviews: settings.dismissStale
    , require_code_owner_reviews: settings.requireReviewFromOwner
    , required_approving_review_count: settings.requiredApprovingReviews
    }

  convertStatusChecks settings =
    { strict: settings.requireUpToDate
    , contexts: settings.checks
    }

-------------------------------------------------------------------------------
-- TODO: Probably move to it's own Sync module
type SyncBPSettingsErrors e
  = ( RequestError
        ⋃ InvalidResponse
        ⋃ InvalidCredentials
        ⋃ BranchNotFound
        ⋃ e
    )

syncBranchProtectionSettings ::
  ∀ e.
  AccessToken ->
  BranchObject ->
  BranchObject ->
  Async (SetBPSettingsErrors e) BranchProtectionSettings
syncBranchProtectionSettings accessToken src dst = do
  settings <- getBranchProtectionSettings accessToken src.owner src.repository src.branch
  setBranchProtectionSettings accessToken dst.owner dst.repository dst.branch settings
  pure settings

-------------------------------------------------------------------------------
-- RULES
protectedBranchRules :: ProtectedBranchSettings -> Rules
protectedBranchRules { pullRequestReview -- :: Maybe PullRequestReviewSettings
, statusChecks -- :: Maybe StatusChecksSettings
, requireSignedCommits -- :: Boolean
, includeAdministrators -- :: Boolean
} =
  rule "Force pushes are disabled"
    <> rule "The branch can't be deleted."
    <> maybeRules pullRequestReviewsRules pullRequestReview
    <> maybeRules statusChecksRules statusChecks
    <> boolRule "Commits pushed to this branch must have verified signatures." requireSignedCommits
    <> boolRule "All configured restrictions are enforced even to the administrators." includeAdministrators

pullRequestReviewsRules :: PullRequestReviewSettings -> Rules
pullRequestReviewsRules { requiredApprovingReviews -- :: Int -- TODO: see if we can restrict 1 to 6
, dismissStale -- :: Boolean
, requireReviewFromOwner -- :: Boolean
} =
  rule ("All commits must be made to a non-protected branch and submitted via a pull request with " <> show requiredApprovingReviews <> " approving code reviews.")
    <> rule ("If a code review requires changes the PR cannot be merged. Even if it has " <> show requiredApprovingReviews <> " approved reviews.")
    <> boolRule "If a new commit is pushed to the branch, previous approvals will be dismissed." dismissStale
    <> boolRule "There must be an approved review in the PR from one of the designated code owner." requireReviewFromOwner

statusChecksRules :: StatusChecksSettings -> Rules
statusChecksRules { requireUpToDate -- :: Boolean
, checks -- :: Array String
} =
  boolRule "The branch needs to be tested with the latest code" requireUpToDate
    <> checks
    <#> (\check -> "The status code check " <> show check <> " must be valid before merging the PR.")
