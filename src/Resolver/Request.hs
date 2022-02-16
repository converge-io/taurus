module Resolver.Request (
  Request (..)
) where

import Resolver.Types (RoleID)
import Resolver.Matcher (AMatcher, RMatcher, Hierarchy)

data Request = Request { roles :: [RoleID]
                       , action :: Hierarchy AMatcher
                       , resource :: Hierarchy RMatcher }
  deriving (Show)
