module Resolver.Auth (
  authorised
) where

import Resolver.Matcher (Matchable (matches))
import qualified Resolver.Policy as P
import qualified Resolver.Request as R

-- | Checks if a policy matches against a given request. Only checks the
-- action and resource, not the role.
reqMatch :: P.Policy -> R.Request -> Bool
reqMatch p r = P.action p `matches` R.action r && P.resource p `matches` R.resource r

-- | Checks if a given request is matched by any policy in a given list.
-- (i.e if the policy list authorises the request)                     .
authorised :: R.Request -> [P.Policy] -> Bool
authorised r = any (`reqMatch` r)
