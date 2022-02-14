module Policy (
  Policy (..)
, RoleID
) where

import Types (RoleID)
import Matcher (AMatcher, RMatcher, Hierarchy)

data Policy = Policy { roleId   :: RoleID
                     , action   :: Hierarchy AMatcher
                     , resource :: Hierarchy RMatcher }
  deriving (Show)
