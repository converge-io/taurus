{-# LANGUAGE DataKinds, TypeOperators #-}

module Http.Server (
  app
) where

import Servant
import Data.Text
import Resolver.Matcher (Hierarchy, AMatcher, RMatcher)
import Resolver.Types (ActorID)
import Http.Types
import Http.Decoders
import Http.Encoders

type AuthRequestAPI = "request" :> Capture "actorId" ActorID
                                :> Capture "action" (Hierarchy AMatcher)
                                :> Capture "resource" (Hierarchy RMatcher)
                                :> Get '[PlainText, JSON] AuthResponse

authRespond :: ActorID
        -> Hierarchy AMatcher
        -> Hierarchy RMatcher
        -> Handler AuthResponse
authRespond id action resource = return Allow

server :: Server AuthRequestAPI
server = authRespond

-- Boilerplate for type guidance
authRequestAPI :: Proxy AuthRequestAPI
authRequestAPI = Proxy

app :: Application
app = serve authRequestAPI server
