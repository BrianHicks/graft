module Main where

import Ingest
import System.FilePath
import Text.Regex
import System.FilePath.Glob (compile)
import Data.Text (pack)
import Flow

main :: IO ()
main =
  ingest [ testIngester ] "."

testIngester :: Ingester
testIngester =
  let interestingFiles = compile "**/*.hs"
      haskellModule = mkRegex "^module (.+) .*$"
      parse filepath = do
        contents <- readFile filepath
        pure <|
          case matchRegex haskellModule contents of
            Just [moduleName] -> (pack moduleName, [])
            _ -> (pack filepath, [])
  in ingester interestingFiles parse
