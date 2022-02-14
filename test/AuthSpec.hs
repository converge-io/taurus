{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import Test.Hspec
import Auth
import Matcher (AMatcher (..), RMatcher (..), Hierarchy (..))
import qualified Request as R
import qualified Policy as P
import Data.Text

spec :: Spec
spec = do
  describe "Auth.authorise" $
    it "returns True if any Policy in a list matches the given request" $ do
      let r = R.Request { R.roles = [42]
                        , R.action = Node (ASpecific "org") (Just (Node (ASpecific "CreateUser") Nothing))
                        , R.resource = Node (RSpecific "org" "27") Nothing }
          -- a1 allows r
          a1 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Just (Node AWildcard Nothing))
                        , P.resource = Node (RSpecific "org" "27") Nothing }
          -- a2 does not allow r on the basis of the resource
          a2 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Just (Node AWildcard Nothing))
                        , P.resource = Node (RSpecific "org" "19") Nothing }
          -- a3 does not allow r on the basis of the action
          a3 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Just (Node (ASpecific "ListUsers") Nothing))
                        , P.resource = Node (RSpecific "org" "27") Nothing }
      r `authorised` [a1, a2, a3] `shouldBe` True
      r `authorised` [a2, a3] `shouldBe` False
