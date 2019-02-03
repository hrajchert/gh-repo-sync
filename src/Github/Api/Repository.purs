module Github.Api.Repository
  ( Repository
  , Owner
  , RepositoryPermissions
  , Organization
  , getRepo
  , GetRepoError (..)
  , RepoNotFound
  , RepoNotFoundImpl
  )
where

import Prelude

import Affjax (Request, defaultRequest, Response)
import Affjax.ResponseFormat as ResponseFormat
import Control.Async (Async, throwErrorV)
import Data.Either (Either(..))
import Data.Explain (class Explain)
import Data.HTTP.Method (Method(..))
import Data.JSON.ParseForeign (class ParseForeign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Github.Api.Api (AccessToken, InvalidCredentials, InvalidResponse, RequestError, addAccessTokenIfPresent, api, getStatusCode, invalidCredentials, parseResponse, request, requestInternalError, site)
import Effect.Exception (error)
import Github.Entities (OrgName(..), RepoName(..))
import Type.Row (RowApply)
import Data.Variant (Variant, inj, SProxy(..))

-- Used to join error types together (unicode option+22C3)
infixr 0 type RowApply as ⋃

-------------------------------------------------------------------------------
-- Githubs documentation https://developer.github.com/v3/repos/#get

-- TODO: Maybe groupBy into a context object
type GetRepoError e =
  ( RequestError
  ⋃ InvalidResponse
  ⋃ InvalidCredentials
  ⋃ RepoNotFound
  ⋃ e
  )

getRepo
  :: ∀ e
  .  Maybe AccessToken
  -> OrgName
  -> RepoName
  -> Async (GetRepoError e) Repository
getRepo maybeAccessToken org repo =
  -- Make the request and interpret the response
  request req >>= transformResponse
    where
      endpointUrl :: String
      endpointUrl = api $ "repos/" <> unwrap org <> "/" <> unwrap repo

      req :: Request String
      req = defaultRequest  { url = endpointUrl
                            , headers = addAccessTokenIfPresent maybeAccessToken []
                            , method = Left GET
                            , responseFormat = ResponseFormat.string
                            }

      transformResponse :: Response String -> Async (GetRepoError e) Repository
      transformResponse res = case getStatusCode res.status of
        200 -> parseResponse res.body
        401 -> throwErrorV invalidCredentials
        404 -> throwErrorV $ repoNotFound org repo
        n   -> throwErrorV $ requestInternalError req $ error $ "Unexpected status code " <> show n


newtype Repository = Repository
  { id                  :: Int
  , name                :: String
  , full_name           :: String
  , owner               :: Owner
  , private             :: Boolean
  , html_url            :: String
  , description         :: String
  , fork                :: Boolean
  , url                 :: String
  , forks_url           :: String
  , keys_url            :: String
  , collaborators_url   :: String
  , teams_url           :: String
  , hooks_url           :: String
  , issue_events_url    :: String
  , events_url          :: String
  , assignees_url       :: String
  , branches_url        :: String
  , tags_url            :: String
  , blobs_url           :: String
  , git_tags_url        :: String
  , git_refs_url        :: String
  , trees_url           :: String
  , statuses_url        :: String
  , languages_url       :: String
  , stargazers_url      :: String
  , contributors_url    :: String
  , subscribers_url     :: String
  , subscription_url    :: String
  , commits_url         :: String
  , git_commits_url     :: String
  , comments_url        :: String
  , issue_comment_url   :: String
  , contents_url        :: String
  , compare_url         :: String
  , merges_url          :: String
  , archive_url         :: String
  , downloads_url       :: String
  , issues_url          :: String
  , pulls_url           :: String
  , milestones_url      :: String
  , notifications_url   :: String
  , labels_url          :: String
  , releases_url        :: String
  , deployments_url     :: String
  , created_at          :: String -- TODO: convert to date
  , updated_at          :: String -- TODO: convert to date
  , pushed_at           :: String -- TODO: convert to date
  , git_url             :: String
  , ssh_url             :: String
  , clone_url           :: String
  , svn_url             :: String
  , homepage            :: Maybe String
  , size                :: Int
  , stargazers_count    :: Int
  , watchers_count      :: Int
  , language            :: String
  , has_issues          :: Boolean
  , has_projects        :: Boolean
  , has_downloads       :: Boolean
  , has_wiki            :: Boolean
  , has_pages           :: Boolean
  , forks_count         :: Int
  , mirror_url          :: Maybe String
  , archived            :: Boolean
  , open_issues_count   :: Int
  , license             :: Maybe RepositoryLicense
  , forks               :: Int
  , open_issues         :: Int
  , watchers            :: Int
  , default_branch      :: String
  , permissions         :: Maybe RepositoryPermissions
  , allow_squash_merge  :: Maybe Boolean
  , allow_merge_commit  :: Maybe Boolean
  , allow_rebase_merge  :: Maybe Boolean
  , organization        :: Maybe Organization
  , network_count       :: Int
  , subscribers_count   :: Int
  }

derive instance newtypeRepository :: Newtype Repository _

derive newtype instance parseForeignRepository :: ParseForeign Repository

instance showRepository :: Show Repository  where
  show (Repository c) = "(Repository " <> c.full_name <> ")"

type Owner =
  { login               :: String
  , id                  :: Int
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

type RepositoryPermissions =
  { admin  :: Boolean
  , push   :: Boolean
  , pull   :: Boolean
  }

type RepositoryLicense =
  { key     :: String
  , name    :: String
  , spdx_id :: String
  , url     :: String
  }

type Organization =
  { login               :: String
  , id                  :: Int
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

-------------------------------------------------------------------------------
-- ERRORS

type RepoNotFound ρ = (repoNotFound ∷ RepoNotFoundImpl | ρ)

data RepoNotFoundImpl = RepoNotFoundImpl OrgName RepoName

siteRepoUrl :: OrgName -> RepoName -> String
siteRepoUrl (OrgName org) (RepoName repo') = site $ org <> "/" <> repo'

instance explainRepoNotFoundImpl :: Explain RepoNotFoundImpl where
  explain (RepoNotFoundImpl org repo) = "The repository (" <> siteRepoUrl org repo <> ") is not found, maybe it's private?"

-- | Error constructors for the Variant RepoNotFound
repoNotFound :: ∀ ρ. OrgName -> RepoName -> Variant (RepoNotFound ⋃ ρ)
repoNotFound org repo = inj (SProxy :: SProxy "repoNotFound") (RepoNotFoundImpl org repo)