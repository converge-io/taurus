{-# LANGUAGE OverloadedStrings #-}

module Main where

import Http.Server (app)
import Network.Wai.Handler.Warp (run)
import qualified Hasql.Pool as HP
import qualified Hasql.Connection as HC

connSettings = HC.settings "172.31.0.2" 5432 "pg" "pg" "policies"

main :: IO ()
main = do
  putStrLn "Running Taurus on :1337"
  pool <- HP.acquire (10, 10, connSettings)
  run 1337 (app pool)
