module Resolver.Types (
  RoleID
, ActorID
, ResourceID
, ResourceType
, ActionType
) where

import Data.Text (Text)

type RoleID = Integer
type ActorID = Integer
type ResourceID = Text
type ResourceType = Text
type ActionType = Text
