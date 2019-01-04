module Data.Github.Api.BranchProtection
--   ( Repository
--   , RepositoryParse(..)
--   , Owner
--   , RepositoryPermissions
--   , Organization
--   )
where


import Prelude

import Data.Either (Either)
import Data.Foreign (F, MultipleErrors)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Data.Newtype (class Newtype, wrap)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseContent, ResponseType(JSONResponse))
import Control.Alt ((<|>))
import Data.Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString)
-- import Simple.JSON (read, read')
import Data.JSON.ParseForeign (read, read')
import Data.JSON.ParseForeign (class ParseForeign, readJSON, parseForeign)

-- ********************
-- * BranchProtection *
-- ********************

type BranchProtectionData =
  { url                           :: String
  , required_status_checks        :: Maybe RequiredStatusCheck
  , enforce_admins                :: EnforceAdmins
  , required_signatures           :: RequiredSignatures
  , required_pull_request_reviews :: Maybe RequiredPullRequestReviews
  , restrictions                  :: Maybe Restrictions
  }

parseBranchProtectionData :: Foreign -> F BranchProtectionData
parseBranchProtectionData = parseForeign

newtype BranchProtection = BranchProtection BranchProtectionData


derive instance newtypeBranchProtection :: Newtype BranchProtection _
-- derive instance parseForeignBranchProtection :: ParseForeign BranchProtection _

instance parseForeignBranchProtection :: ParseForeign BranchProtection where
  parseForeign f = BranchProtection <$> parseBranchProtectionData f

instance showBranchProtection :: Show BranchProtection  where
  show (BranchProtection c) = "(BranchProtection " <> c.url <> ")"

instance respondableBranchProtection :: Respondable BranchProtection where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse :: ResponseContent -> F BranchProtection
  fromResponse foreignData = wrap <$> read' foreignData


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



-- *************************
-- * BranchProtectionParse *
-- *************************

-- Class to parse the API body
data BranchProtectionBodyParse
  = Branch BranchProtection
  | Error ApiError

parseBranchProtection :: Foreign -> F BranchProtection
parseBranchProtection = parseForeign

parseApiError :: Foreign -> F ApiError
parseApiError = parseForeign

-- Try to parse it as a BranchProtection or an ApiError
instance parseForeignBPBP :: ParseForeign BranchProtectionBodyParse where
  parseForeign f
    =  Branch <$> parseBranchProtection f
   <|> Error <$> parseApiError f

newtype BranchProtectionParse = BranchProtectionParse (Either MultipleErrors BranchProtectionBodyParse)

instance respondableMaybeBranchProtection :: Respondable BranchProtectionParse  where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse :: ResponseContent -> F BranchProtectionParse
  fromResponse foreignData = pure $ BranchProtectionParse (parseForeign foreignData)
    where
        parseForeign :: ResponseContent -> Either MultipleErrors BranchProtectionBodyParse
        parseForeign foreignD = read foreignD

-- ************
-- * ApiError *
-- ************

newtype ApiErrorParse = ApiErrorParse (Either MultipleErrors ApiError)


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

instance respondableApiErrorParse :: Respondable ApiErrorParse  where
  responseType = Tuple (Just applicationJSON) JSONResponse
  fromResponse :: ResponseContent -> F ApiErrorParse
  fromResponse foreignData = pure $ ApiErrorParse (parseForeign foreignData)
    where
        parseForeign :: ResponseContent -> Either MultipleErrors ApiError
        parseForeign foreignD = wrap <$> read foreignD

