{-# LANGUAGE OverloadedStrings #-}

module Resolver.Parser (
-- * Parsing Actions
  pAction
, pSpecificAction

-- * Parsing Resources
, pResource
, pSpecificResource

-- * Helper Types
, Parser
) where

import Resolver.Matcher (
    Matchable
  , Action
  , Resource
  , AMatcher (..)
  , RMatcher (..)
  , Hierarchy (..)
  )
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text, pack)
import Data.Void
import Data.Functor ((<&>))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

sep :: Parser Text
sep = string ":"
wld :: Parser Text
wld = string "*"
spc :: Parser Text
spc = string "/"

pName :: Parser Text
pName = pack <$> some (alphaNumChar <|> char '-' <|> char '_') <?> "name"
pSpecific = (,) <$> pName <*> (spc *> pName)
pAny = pName <* spc <* wld

parser :: (Matchable a) => Parser a -> Parser (Hierarchy a)
parser p = foldr makeHierarchy EndNode <$> parseMatchers p
  where parseMatchers :: (Matchable a) => Parser a -> Parser [a]
        parseMatchers p = (:) <$> p <*> many (sep *> p)

        makeHierarchy :: (Matchable a) => a -> Hierarchy a -> Hierarchy a
        makeHierarchy x c = Node { matcher=x, child=c }

-- | Parses an Action hierarchy out of a string
pAction :: Parser Action
pAction = parser $  ASpecific <$> pName
                <|> AWildcard <$  wld

-- | Parses a specific Action hierarchy (i.e. no wildcards)
pSpecificAction :: Parser Action
pSpecificAction = parser $ ASpecific <$> pName

-- | Parses a Resource hierarchy out of a string
pResource :: Parser Resource
pResource = parser $  try (uncurry RSpecific <$> pSpecific)
                  <|> RAny <$> pAny
                  <|> RWildcard <$ wld

-- | Parses a specific Resource hierarchy (i.e. no wildcards allows)
pSpecificResource :: Parser Resource
pSpecificResource = parser $ uncurry RSpecific <$> pSpecific
