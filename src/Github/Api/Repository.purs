module Github.Api.Repository
  ( Repository
  , Owner
  , RepositoryPermissions
  , Organization
  , getRepo
  , GetRepoErrors (..)
  )
where

import Prelude

import Affjax (Request, defaultRequest, Response)
import Affjax.ResponseFormat as ResponseFormat
import Control.Async (Async)
import Data.Either (Either(..))
import Data.Explain (class Explain, explain)
import Data.HTTP.Method (Method(..))
import Data.JSON.ParseForeign (class ParseForeign, readJSON)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Exception (Error, message)
import Foreign (MultipleErrors)
import Github.Api.Api (addAccessTokenIfPresent, api, getStatusCode, requestCont, site)

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
      apiRepoUrl :: String -> String -> String
      apiRepoUrl owner repo' = api $ "repos/" <> owner <> "/" <> repo'

      transformResponse :: Either Error (Response String) -> Either GetRepoErrors Repository
      transformResponse (Left err)  = Left (InternalError $ message err)
      transformResponse (Right val) = case getStatusCode val.status of
        200 -> interpretParsedResponse (readJSON val.body)
        401 -> Left InvalidCredentials
        404 -> Left (RepoNotFound $ siteRepoUrl org repo)
        n   -> Left (InternalError $ "Unexpected status code " <> show n)

      interpretParsedResponse :: Either MultipleErrors Repository -> Either GetRepoErrors Repository
      interpretParsedResponse (Left err) = Left (InvalidResponse err)
      interpretParsedResponse (Right r) = Right r

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