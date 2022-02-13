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
      parse pAction "" "write" `shouldParse` Node (ASpecific "write") Nothing

    it "parses a set of actions" $ do
      shouldParse
        (parse pAction "" "org:write")
        (Node (ASpecific "org")
              (Just (Node (ASpecific "write")
                    Nothing)))
      shouldParse
        (parse pAction "" "org:users:create")
        (Node (ASpecific "org")
              (Just (Node (ASpecific "users")
                    (Just (Node (ASpecific "create")
                                Nothing)))))

    it "parses actions with wildcards" $
      shouldParse
        (parse pAction "" "org:*:create")
        (Node (ASpecific "org")
              (Just (Node AWildcard
                          (Just (Node (ASpecific "create")
                                      Nothing)))))

  describe "pResource" $ do
    it "parses a single resource with specifiers and wildcards" $ do
      shouldParse
        (parse pResource "" "org/42")
        (Node (RSpecific "org" "42") Nothing)
      shouldParse
        (parse pResource "" "org/*")
        (Node (RAny "org") Nothing)
      shouldParse
        (parse pResource "" "*")
        (Node RWildcard Nothing)

    it "parses a set of resources with specifiers and wildcards" $ do
      shouldParse
        (parse pResource "" "org/42:user/*:*")
        (Node (RSpecific "org" "42")
              (Just (Node (RAny "user")
                          (Just (Node RWildcard Nothing)))))
