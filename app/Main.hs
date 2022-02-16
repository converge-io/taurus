module Main where

import Http.Server (app)
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  putStrLn "Running Taurus on :1337"
  run 1337 app
