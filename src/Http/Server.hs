{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

module Http.Server (
  app
) where

import Servant
import Data.Text
import Resolver.Matcher (Action, Resource)
import Resolver.Types (RoleID)
import Http.Types
import Http.Decoders
import Http.Encoders
import Control.Monad.IO.Class (liftIO)
import Hasql.Pool (Pool, use, UsageError (..))
import Source.Policy (getPoliciesForRoles, PolicyParsingError (..))
import Hasql.Session (run)
import Resolver.Auth (authorised)
import Resolver.Request
import Servant.Server.Internal.ServerError (responseServerError)

type AuthRequestAPI = "request" :> ReqBody '[JSON] Request
                                :> Post '[PlainText, JSON] AuthResponse

server :: Pool -> Server AuthRequestAPI
server cs = authRespond
  where authRespond :: Request -> Handler AuthResponse
        authRespond req = do
          queryResult <- liftIO . use cs $ getPoliciesForRoles (roles req)
          -- TODO: lift all these Eithers into ExceptT and process monadically
          case queryResult of
            Left _          -> throwError $ err500 { errBody = "unable to query database" }
            Right policies  -> case policies of
              Left _               -> throwError $ err500 { errBody = "unable to parse policies" }
              Right parsedPolicies -> return $ if authorised req parsedPolicies then Allow else Deny


-- Boilerplate for type guidance
authRequestAPI :: Proxy AuthRequestAPI
authRequestAPI = Proxy

app :: Pool -> Application
app = serve authRequestAPI . server
