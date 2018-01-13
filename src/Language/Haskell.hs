module Language.Haskell
  ( haskell
  ) where

import Data.Attoparsec.Text -- can't be bothered to list imports...
import Data.Char (isSpace)
import Data.Either (lefts)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Flow
import Ingest (Ingester, ingester)
import Language
       (Edge(HasFiletype, Imports), Node(Filetype, Haskell))
import System.FilePath.Glob (compile)

haskell :: Ingester Node Edge
haskell =
  let interestingFiles = compile "**/*.hs"
      getInfo filepath = do
        contents <- T.readFile filepath
        let (name, edges) =
              case parseOnly (fileInfo filepath) contents of
                Right res -> res
                Left _ -> (Haskell <| pack filepath, [])
        return (name, (HasFiletype, Filetype "haskell") : edges)
  in ingester interestingFiles getInfo

fileInfo :: FilePath -> Parser (Node, [(Edge, Node)])
fileInfo filename = do
  name <- option (Haskell <| pack filename) haskellModule
  imports <- sepBy (eitherP haskellImport line) endOfLine
  return (name, lefts imports)

haskellModule :: Parser Node
haskellModule = do
  string "module "
  name <- takeTill isSpace
  line
  return <| Haskell name

haskellImport :: Parser (Edge, Node)
haskellImport = do
  string "import "
  name <- takeTill isSpace
  line
  return (Imports, Haskell name)

line :: Parser Text
line = do
  line <- takeTill isEndOfLine
  endOfLine
  return line

emptyLine :: Parser ()
emptyLine = endOfLine
