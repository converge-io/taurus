module Resolver.Types (
  RoleID
, ResourceID
, ResourceType
, ActionType
) where

import Data.Text (Text)

type RoleID = Integer
type ResourceID = Text
type ResourceType = Text
type ActionType = Text
