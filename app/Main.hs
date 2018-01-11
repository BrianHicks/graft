module Main where

import Ingest
import System.FilePath

main :: IO ()
main =
  ingest [ testIngester ] "."
