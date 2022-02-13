{-# LANGUAGE OverloadedStrings #-}

module Parser (
  pAction
, pSpecificAction
, pResource
, pSpecificResource
) where

import Matcher (Matchable, AMatcher (..), RMatcher (..), Hierarchy (..))
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
        makeHierarchy x EndNode = Node { matcher=x, child=Nothing }
        makeHierarchy x c       = Node { matcher=x, child=Just c }

pAction :: Parser (Hierarchy AMatcher)
pAction = parser $  ASpecific <$> pName
                <|> AWildcard <$  wld

pSpecificAction :: Parser (Hierarchy AMatcher)
pSpecificAction = parser $ ASpecific <$> pName

pResource :: Parser (Hierarchy RMatcher)
pResource = parser $  try (uncurry RSpecific <$> pSpecific)
                  <|> RAny <$> pAny
                  <|> RWildcard <$ wld

pSpecificResource :: Parser (Hierarchy RMatcher)
pSpecificResource = parser $ uncurry RSpecific <$> pSpecific
