{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}

module Source.Policy (
  getPoliciesForRoles
) where

import Resolver.Types (RoleID)
import Resolver.Policy (Policy (..))
import Resolver.Parser (pAction, pResource)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (ParseErrorBundle)
import Hasql.Statement
import Hasql.Session
import Hasql.TH
import Data.Profunctor (dimap)
import Data.Vector (Vector, toList, fromList)
import Data.Text (Text)
import Data.Int (Int32)
import Data.Either.Combinators

newtype PolicyParsingError = PolicyParsingError String
  deriving (Show)

makePolicy :: (Int32, Text, Text) -> Either PolicyParsingError Policy
makePolicy (_roleId, _action, _resource) = do
  let roleId = toInteger _roleId
  action <- mapLeft (\_ -> PolicyParsingError "error parsing action") . runParser pAction "query result" $ _action
  resource <- mapLeft (\_ -> PolicyParsingError "error parsing resource") . runParser pResource "query result" $ _resource
  return Policy {..}

sPoliciesForRoles :: Statement [RoleID] (Either PolicyParsingError [Policy])
sPoliciesForRoles = dimap
                      (fromList . map fromInteger) -- convert [Integer] to Vector Int32
                      (mapM makePolicy . toList)
                      [vectorStatement|
                        select
                          role_id :: int4
                        , action :: text
                        , resource :: text
                        from policies
                        where role_id = ANY($1 :: int4[])
                       |]

getPoliciesForRoles :: [RoleID] -> Session (Either PolicyParsingError [Policy])
getPoliciesForRoles ids = statement ids sPoliciesForRoles
