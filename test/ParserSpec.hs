{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Parser
import Matcher (AMatcher (..), RMatcher (..), Hierarchy (..))

spec :: Spec
spec = do
  describe "pAction" $ do
    it "parses a single action" $
      parse pAction "" "write" `shouldParse` Node (ASpecific "write") EndNode

    it "parses a set of actions" $ do
      shouldParse
        (parse pAction "" "org:write")
        (Node (ASpecific "org")
              (Node (ASpecific "write")
                    EndNode))
      shouldParse
        (parse pAction "" "org:users:create")
        (Node (ASpecific "org")
              (Node (ASpecific "users")
                    (Node (ASpecific "create")
                                EndNode)))

    it "parses actions with wildcards" $
      shouldParse
        (parse pAction "" "org:*:create")
        (Node (ASpecific "org")
              (Node AWildcard
                    (Node (ASpecific "create")
                          EndNode)))

  describe "pSpecificAction" $ do
    it "only parses fully-specified actions" $ do
      shouldParse
        (parse pSpecificAction "" "org:write")
        (Node (ASpecific "org") (Node (ASpecific "write") EndNode))
      parse pSpecificAction "" `shouldFailOn` "org:*"

  describe "pResource" $ do
    it "parses a single resource with specifiers and wildcards" $ do
      shouldParse
        (parse pResource "" "org/42")
        (Node (RSpecific "org" "42") EndNode)
      shouldParse
        (parse pResource "" "org/*")
        (Node (RAny "org") EndNode)
      shouldParse
        (parse pResource "" "*")
        (Node RWildcard EndNode)

    it "parses a set of resources with specifiers and wildcards" $ do
      shouldParse
        (parse pResource "" "org/42:user/*:*")
        (Node (RSpecific "org" "42")
              (Node (RAny "user")
                          (Node RWildcard EndNode)))

  describe "pSpecificResource" $ do
    it "parses only fully-specified resources" $ do
      shouldParse
        (parse pSpecificResource "" "org/42:user/27")
        (Node (RSpecific "org" "42")
              (Node (RSpecific "user" "27") EndNode))
      parse pSpecificResource "" `shouldFailOn` "org/42:*"
      parse pSpecificResource "" `shouldFailOn` "org/42:user/*"
