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
                        , R.action = Node (ASpecific "org") (Node (ASpecific "CreateUser") EndNode)
                        , R.resource = Node (RSpecific "org" "27") EndNode }
          -- a1 allows r
          a1 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Node AWildcard EndNode)
                        , P.resource = Node (RSpecific "org" "27") EndNode }
          -- a2 does not allow r on the basis of the resource
          a2 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Node AWildcard EndNode)
                        , P.resource = Node (RSpecific "org" "19") EndNode }
          -- a3 does not allow r on the basis of the action
          a3 = P.Policy { P.roleId = 42
                        , P.action = Node (ASpecific "org") (Node (ASpecific "ListUsers") EndNode)
                        , P.resource = Node (RSpecific "org" "27") EndNode }
      r `authorised` [a1, a2, a3] `shouldBe` True
      r `authorised` [a2, a3] `shouldBe` False
