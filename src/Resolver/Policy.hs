module Resolver.Policy (
  Policy (..)
) where

import Resolver.Types (RoleID)
import Resolver.Matcher (AMatcher, RMatcher, Hierarchy)

data Policy = Policy { roleId   :: RoleID
                     , action   :: Hierarchy AMatcher
                     , resource :: Hierarchy RMatcher }
  deriving (Show)
