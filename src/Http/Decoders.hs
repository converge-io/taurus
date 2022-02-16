{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Http.Decoders () where

import Web.HttpApiData (FromHttpApiData (..))
import Resolver.Matcher (Action, Resource)
import Resolver.Parser (pSpecificAction, pSpecificResource)
import Text.Megaparsec (runParser)
import Data.Either.Combinators (mapLeft)

instance FromHttpApiData Action where
  parseUrlPiece = mapLeft (const "invalid action string")
                          . runParser pSpecificAction "url piece"

instance FromHttpApiData Resource where
  parseUrlPiece = mapLeft (const "invalid resource string")
                          . runParser pSpecificResource "url piece"
