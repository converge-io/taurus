{-# LANGUAGE OverloadedStrings #-}

module MatcherSpec (spec) where

import Test.Hspec
import Matcher
import Data.Text

spec :: Spec
spec = do
  describe "Resource Matchers" $ do
    context "with RSpecific" $
      it "can match other RSpecific where all parameters match" $ do
        let a = RSpecific "org" "42"
            b = RSpecific "org" "42"
            c = RSpecific "org" "24"
            d = RSpecific "gro" "42"
            e = RSpecific "gro" "24"
        a `matches` b `shouldBe` True
        a `matches` c `shouldBe` False
        a `matches` d `shouldBe` False
        a `matches` e `shouldBe` False

    context "with RAny" $ do
      it "can match other RAny where the ResourceType matches" $ do
        let a = RAny "org"
            b = RAny "org"
            c = RAny "gro"
        a `matches` b `shouldBe` True
        a `matches` c `shouldBe` False

      it "can match against RSpecific where the ResourceType matches" $ do
        let a = RAny "org"
            b = RSpecific "org" "42"
            c = RSpecific "gro" "27"
        a `matches` b `shouldBe` True
        a `matches` c `shouldBe` False

    context "with RWildcard" $ do
      it "matches other RWildcard" $
        RWildcard `matches` RWildcard `shouldBe` True

      it "matches RAny" $
        RWildcard `matches` RAny "org" `shouldBe` True

      it "matches RSpecific" $
        RWildcard `matches` RSpecific "org" "42" `shouldBe` True

  describe "Action Matchers" $ do
    context "with ASpecific" $
      it "matches other ASpecific where the ActionType matches" $ do
        let a = ASpecific "write"
            b = ASpecific "write"
            c = ASpecific "read"
        a `matches` b `shouldBe` True
        a `matches` c `shouldBe` False

    context "with AWildcard" $ do
      it "matches other AWildcard" $
        AWildcard `matches` AWildcard `shouldBe` True

      it "matches any ASpecific" $
        AWildcard `matches` ASpecific "write" `shouldBe` True

  describe "Hierarchy" $ do
    it "can match hierarchies where no children exist" $ do
      let a = Node (RAny "org") EndNode
          b = Node (RSpecific "org" "42") EndNode
          c = Node RWildcard EndNode
      a `matches` b `shouldBe` True
      a `matches` c `shouldBe` False

    it "can match nested hierarchies" $ do
      let a = Node { matcher=RSpecific "org" "42"
                   , child=Node { matcher=RAny "user"
                                , child=Node { matcher=RWildcard
                                             , child=EndNode }}}
          b = Node { matcher=RSpecific "org" "42"
                   , child=Node { matcher=RSpecific "user" "27"
                                , child=Node { matcher=RAny "contact"
                                             , child=EndNode }}}
          c = Node { matcher=RSpecific "org" "42"
                   , child=Node { matcher=RAny "user"
                                , child=EndNode }}
      a `matches` b `shouldBe` True
      a `matches` c `shouldBe` False
