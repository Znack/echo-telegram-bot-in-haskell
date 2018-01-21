module Main where

import Server (startServer)
import System.Environment

main :: IO ()
main = do
  token <- head <$> getArgs
  startServer token
