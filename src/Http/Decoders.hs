{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Http.Decoders () where

import Web.HttpApiData (FromHttpApiData (..))
import Resolver.Matcher (Action, Resource)
import Resolver.Parser (pSpecificAction, pSpecificResource)
import Text.Megaparsec (runParser)
import Data.Either.Combinators (mapLeft)
import Data.Aeson
import Resolver.Request

instance FromHttpApiData Action where
  parseUrlPiece = mapLeft (const "invalid action string")
                          . runParser pSpecificAction "url piece"

instance FromHttpApiData Resource where
  parseUrlPiece = mapLeft (const "invalid resource string")
                          . runParser pSpecificResource "url piece"

instance FromJSON Request where
  parseJSON = withObject "Request"
                (\v -> Request <$> (v .: "roles")
                               <*> (v .: "action")
                               <*> (v .: "resource"))

instance FromJSON Action where
  parseJSON = withText "Action" (\t ->
                case runParser pSpecificAction "json" t of
                  Left _ -> fail "unable to parse action"
                  Right a -> pure a
              )

instance FromJSON Resource where
  parseJSON = withText "Resource" (\t ->
                case runParser pSpecificResource "json" t of
                  Left _ -> fail "unable to parse resource"
                  Right r -> pure r
              )
