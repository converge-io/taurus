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
import Control.Monad.Trans.Except
import Control.Monad.Except

type AuthRequestAPI = "request" :> Capture "roleId" RoleID
                                :> Capture "action" Action
                                :> Capture "resource" Resource
                                :> Get '[PlainText, JSON] AuthResponse

authRespond id action resource = do
  liftIO . putStrLn $ "Incoming request: "
                     <> show id ++ " wants to "
                     <> show action ++ " on "
                     <> show resource
  return Allow

server :: Pool -> Server AuthRequestAPI
server cs = authRespond
  where authRespond :: RoleID -> Action -> Resource -> Handler AuthResponse
        authRespond i a r = do
          let req = Request [i] a r
          queryResult <- liftIO . use cs $ getPoliciesForRoles [i]
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
