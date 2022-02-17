{-# LANGUAGE OverloadedStrings #-}

module Main where

import Http.Server (app)
import Network.Wai.Handler.Warp (run)
import qualified Hasql.Pool as HP
import qualified Hasql.Connection as HC
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)

getSettingsFromEnv :: IO HC.Settings
getSettingsFromEnv = do
  host <- getEnv "TAURUS_SOURCE_ADDR"
  port <- getEnv "TAURUS_SOURCE_PORT"
  user <- getEnv "TAURUS_SOURCE_USER"
  pass <- getEnv "TAURUS_SOURCE_PASS"
  db   <- getEnv "TAURUS_SOURCE_DB"
  return $ HC.settings (pack host)
                       (read port)
                       (pack user)
                       (pack pass)
                       (pack db)

main :: IO ()
main =
  putStrLn "Running Taurus on :1337"
  >> getSettingsFromEnv
  >>= HP.acquire . (,,) 10 10
  >>= run 1337 . app
