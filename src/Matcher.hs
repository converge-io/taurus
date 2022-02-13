{-# LANGUAGE GADTs #-}

module Matcher (
  ID
, ResourceType
, RMatcher (..)
, AMatcher (..)
, Hierarchy (..)
, Matchable (..)
) where

import Data.Text (Text)

type ID = Text
type ResourceType = Text
type ActionType = Text

data RMatcher = RSpecific ResourceType ID
              | RAny ResourceType
              | RWildcard

data AMatcher = ASpecific ActionType
              | AWildcard

data Hierarchy a = (Matchable a) => Node { matcher :: a
                                         , child   :: Maybe (Hierarchy a) }

class Matchable a where
  matches :: a -> a -> Bool

instance (Matchable a) => Matchable (Hierarchy a) where
  matches a b = matcher a `matches` matcher b
                && child a `matches` child b

instance (Matchable a) => Matchable (Maybe a) where
  matches Nothing Nothing = True
  matches (Just _) Nothing = False
  matches (Just x) (Just y) = x `matches` y
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
