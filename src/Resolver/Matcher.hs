{-# LANGUAGE GADTs #-}

module Resolver.Matcher (
-- * Basic types to represent match objects
  Action
, Resource

-- * Matchers
, RMatcher (..)
, AMatcher (..)
, Hierarchy (..)

-- * Typeclasses
, Matchable (..)
) where

import Data.Text (Text)
import Resolver.Types (ResourceID, ResourceType, ActionType)

-- | Resource matchers
data RMatcher = RSpecific ResourceType ResourceID -- ^ match a specified resource (e.g. "org/42")
              | RAny ResourceType                 -- ^ match an unspecified resource (e.g. "org/*")
              | RWildcard                         -- ^ match a wildcard resource ("*")
  deriving (Eq, Show)

-- | Action matchers
data AMatcher = ASpecific ActionType -- ^ match a specified action (e.g. "write")
              | AWildcard            -- ^ match a wildcard action ("*")
  deriving (Eq, Show)

-- | A linear hierarchy of matchable things
data Hierarchy a = (Matchable a) => Node { matcher :: a
                                         , child   :: Hierarchy a }
                 | EndNode -- ^ used to terminate folds

-- | Shorthand for Hierarchy AMatcher
type Action = Hierarchy AMatcher

-- | Shorthand for Hierarchy RMatcher
type Resource = Hierarchy RMatcher

instance (Eq a) => Eq (Hierarchy a) where
  EndNode == EndNode = True
  Node x1 y1 == Node x2 y2 = x1 == x2 && y1 == y2
  _ == _ = False

instance (Show a) => Show (Hierarchy a) where
  show EndNode = "EndNode"
  show (Node x y) = "Node (" ++ show x ++ "; " ++ show y ++ ")"

class Matchable a where
  -- | matches is a non-commutative, fuzzy equality in that the order of
  -- the arguments should matter. This is because more general matchers
  -- should match more specific ones, but not vice versa.
  matches :: a -> a -> Bool

instance (Matchable a) => Matchable (Hierarchy a) where
  matches EndNode EndNode = True
  matches (Node x1 y1) (Node x2 y2) = x1 `matches` x2
                                    && y1 `matches` y2
  matches _ _ = False

instance Matchable RMatcher where
  matches (RSpecific x1 y1) (RSpecific x2 y2) = x1 == x2 && y1 == y2
  matches (RAny x1) (RAny x2) = x1 == x2
  matches (RAny x1) (RSpecific x2 _) = x1 == x2
  matches RWildcard _ = True
  matches _ _ = False

instance Matchable AMatcher where
  matches (ASpecific x) (ASpecific y) = x == y
  matches AWildcard _ = True
  matches _ _ = False
