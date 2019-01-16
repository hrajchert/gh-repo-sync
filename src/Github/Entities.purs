module Github.Entities
--  (
--  )
    where

import Data.JSON.ParseForeign (class ParseForeign)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

-- | The name of an organization
newtype OrgName = OrgName String

-- For JSON parsing
derive instance newtypeOrgName :: Newtype OrgName _
derive newtype instance parseForeignOrgName :: ParseForeign OrgName
-- For Showing
derive newtype instance showOrgName :: Show OrgName


-- | The name of a repository
newtype RepoName = RepoName String

-- For JSON parsing
derive instance newtypeRepoName :: Newtype RepoName _
derive newtype instance parseForeignRepoName :: ParseForeign RepoName
-- For Showing
derive newtype instance showRepoName :: Show RepoName


-- | The name of a Branch
newtype BranchName = BranchName String

-- For JSON parsing
derive instance newtypeBranchName :: Newtype BranchName _
derive newtype instance parseForeignBranchName :: ParseForeign BranchName
-- For Showing
derive newtype instance showBranchName :: Show BranchName