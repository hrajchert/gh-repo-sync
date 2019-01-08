module Data.Github.Settings.BranchProtection
  ( BranchProtectionSettings(..)
  , ProtectedBranchSettings
  , PullRequestReviewSettings
  , StatusChecksSettings
  )
where

import Prelude
import Data.JSON.ParseForeign (class ParseForeign, parseForeign)
import Foreign (F, Foreign)
import Data.Maybe (Maybe(..))
import Data.Explain (class Explain, explain)

-- | Data structure that models the settings in this page
-- | https://github.com/<owner>/<repo>/settings/branches/<branch>

data BranchProtectionSettings
  = BranchNotProtected
  | ProtectedBranch ProtectedBranchSettings

type ProtectedBranchSettings =
  { pullRequestReview         :: Maybe PullRequestReviewSettings
  , statusChecks              :: Maybe StatusChecksSettings
  , requireSignedCommits      :: Boolean
  , includeAdministrators     :: Boolean
  }

type PullRequestReviewSettings =
  { requiredApprovingReviews :: Int -- TODO: see if we can restrict 1 to 6
  , dismissStale             :: Boolean
  , requireReviewFromOwner   :: Boolean
  }

type StatusChecksSettings =
  { requireUpToDate  :: Boolean
  , checks           :: Array String
  }

instance showBranchSettings :: Show BranchProtectionSettings where
  show BranchNotProtected  = "(BranchNotProtected)"
  show (ProtectedBranch s) = "(ProtectedBranch admin = " <> show s.includeAdministrators <> ")"

instance parseForeignBranchSettings :: ParseForeign BranchProtectionSettings where
  parseForeign :: Foreign -> F BranchProtectionSettings
  parseForeign f =
    interpret <$> parsedSettings
      where
        parsedSettings :: F (Maybe ProtectedBranchSettings)
        parsedSettings = parseForeign f

        interpret :: Maybe ProtectedBranchSettings -> BranchProtectionSettings
        interpret (Just settings) = ProtectedBranch settings
        interpret Nothing         = BranchNotProtected


instance explainBranchSettings :: Explain BranchProtectionSettings where
  explain :: BranchProtectionSettings -> String
  explain BranchNotProtected = "The branch is not protected"
  explain (ProtectedBranch settings) = "The branch is protected in the following way: " <> (explain $ protectedBranchRules settings)

protectedBranchRules :: ProtectedBranchSettings -> Array String
protectedBranchRules { pullRequestReview         -- :: Maybe PullRequestReviewSettings
                     , statusChecks              -- :: Maybe StatusChecksSettings
                     , requireSignedCommits      -- :: Boolean
                     , includeAdministrators     -- :: Boolean
                     } = rule "Force pushes are disabled"
                      <> rule "The branch can't be deleted."
                      <> maybeRules pullRequestReviewsRules pullRequestReview
                      <> maybeRules statusChecksRules statusChecks
                      <> boolRule "Commits pushed to this branch must have verified signatures." requireSignedCommits
                      <> boolRule "All configured restrictions are enforced even to the administrators." includeAdministrators

pullRequestReviewsRules :: PullRequestReviewSettings -> Array String
pullRequestReviewsRules
  { requiredApprovingReviews -- :: Int -- TODO: see if we can restrict 1 to 6
  , dismissStale             -- :: Boolean
  , requireReviewFromOwner   -- :: Boolean
  } = rule ("All commits must be made to a non-protected branch and submitted via a pull request with " <> show requiredApprovingReviews <> " approving code reviews.")
   <> rule ("If a code review requires changes the PR cannot be merged. Even if it has " <> show requiredApprovingReviews <> " approved reviews.")
   <> boolRule "If a new commit is pushed to the branch, previous approvals will be dismissed." dismissStale
   <> boolRule "There must be an approved review in the PR from one of the designated code owner." requireReviewFromOwner

statusChecksRules :: StatusChecksSettings -> Array String
statusChecksRules
  { requireUpToDate   -- :: Boolean
  , checks            -- :: Array String
  } = boolRule "The branch needs to be tested with the latest code" requireUpToDate
   <> checks <#> (\check -> "The status code check " <> show check <> " must be valid before merging the PR.")




boolRule :: String -> Boolean -> Array String
boolRule _   false = []
boolRule str true  = [str]

maybeRules :: forall a. (a -> Array String) -> Maybe a -> Array String
maybeRules _ (Nothing) = []
maybeRules f (Just a) = f a

rule :: String -> Array String
rule str = [str]
