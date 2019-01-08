module Data.Github.Repository
  ( Repository
  , parseRepository
  , Owner
  , RepositoryPermissions
  , Organization
  )
where

import Prelude

import Data.Either (Either)
import Foreign (F, MultipleErrors, Foreign)
import Data.JSON.ParseForeign (parseForeign, class ParseForeign, readJSON)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

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

newtype Repository = Repository RepositoryData

type RepositoryData =
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
-- derive instance readForeignRepository :: ReadForeign Repository _

instance showRepository :: Show Repository  where
  show (Repository c) = "(Repository " <> c.full_name <> ")"


parseRepositoryData :: Foreign -> F RepositoryData
parseRepositoryData = parseForeign

instance parseForeignRepository :: ParseForeign Repository where
  parseForeign f
    =  Repository <$> parseRepositoryData f


parseRepository :: String -> Either MultipleErrors Repository
parseRepository = readJSON
