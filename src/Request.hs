module Request (
  Request (..)
) where

import Types (RoleID)
import Matcher (AMatcher, RMatcher, Hierarchy)

data Request = Request { roles :: [RoleID]
                       , action :: Hierarchy AMatcher
                       , resource :: Hierarchy RMatcher }
  deriving (Show)
