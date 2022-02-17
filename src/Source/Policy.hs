{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Source.Policy (
  getPoliciesForRoles
, PolicyParsingError (..)
) where

import Resolver.Types (RoleID)
import Resolver.Policy (Policy (..))
import Resolver.Parser (pAction, pResource, Parser)
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
import Error

newtype PolicyParsingError = PolicyParsingError Text
  deriving (Show)

instance ToProgrammeError PolicyParsingError where
  toProgrammeError (PolicyParsingError s) =
    SourceQueryError $ "Unable to parse policy: " <> s

makePolicy :: (Int32, Text, Text) -> Either PolicyParsingError Policy
makePolicy (_roleId, _action, _resource) =
  do
    let roleId = toInteger _roleId
    action <- mkParse "action" pAction _action
    resource <- mkParse "resource" pResource _resource
    return Policy {..}
  where mkParse l p = mapLeft (const . PolicyParsingError $ "error parsing " <> l) . runParser p "query result"

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
