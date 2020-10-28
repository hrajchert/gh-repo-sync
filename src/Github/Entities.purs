module Github.Entities --  (
 --  )
 where

import Simple.JSON (class ReadForeign)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

-- | The name of an organization
-- TODO: rename to owner to match the documentation
newtype OrgName
  = OrgName String

-- For JSON parsing
derive instance newtypeOrgName :: Newtype OrgName _

derive newtype instance readForeignOrgName :: ReadForeign OrgName

-- For Showing
derive newtype instance showOrgName :: Show OrgName

-- | The name of a repository
newtype RepoName
  = RepoName String

-- For JSON parsing
derive instance newtypeRepoName :: Newtype RepoName _

derive newtype instance readForeignRepoName :: ReadForeign RepoName

-- For Showing
derive newtype instance showRepoName :: Show RepoName

-- | The name of a Branch
newtype BranchName
  = BranchName String

-- For JSON parsing
derive instance newtypeBranchName :: Newtype BranchName _

derive newtype instance readForeignBranchName :: ReadForeign BranchName

-- For Showing
derive newtype instance showBranchName :: Show BranchName

--
-- TODO: Rename to BranchCanonical
type BranchObject
  = { owner :: OrgName
    , repository :: RepoName
    , branch :: BranchName
    }
