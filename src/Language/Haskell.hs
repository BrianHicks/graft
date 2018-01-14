module Language.Haskell
  ( haskell
  ) where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Data.Void
import Flow
import Ingest (Ingester, ingester)
import Language
       (Edge(HasFiletype, Imports), Node(Filetype, Haskell))
import System.FilePath (FilePath)
import System.FilePath.Glob (compile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)

haskell :: Ingester Node Edge
haskell =
  let interestingFiles = compile "**/*.hs"
      getInfo filepath = do
        contents <- T.readFile filepath
        case parse (parser filepath) filepath contents of
          Left err -> fail <| parseErrorPretty err
          Right (node, edges) ->
            return (node, (HasFiletype, Filetype "haskell") : edges)
  in ingester interestingFiles getInfo

type Parser = Parsec Void Text

parser :: FilePath -> Parser (Node, [(Edge, Node)])
parser filepath = do
  name <- haskellModule <|> (pure <| Haskell (pack filepath) [])
  edges <- haskellBody
  return (name, edges)

haskellModule :: Parser Node
haskellModule = do
  string "module"
  space1
  name <- takeWhile1P (Just "module name") (not . isSpace)
  space1
  exports <- haskellExports <|> pure []
  space
  string "where"
  return <| Haskell name exports

haskellExports :: Parser [Text]
haskellExports = do
  string "("
  exports <- some haskellExport
  string ")"
  return exports

haskellExport :: Parser Text
haskellExport = do
  space
  export <- some alphaNumChar <?> "export"
  string "(..)" <|> "" -- TODO: hack alert!
  space
  string "," <|> "" -- TODO: hack alert!
  space
  return <| pack export

haskellBody :: Parser [(Edge, Node)]
haskellBody = do
  space
  statements <- some haskellStatement
  pure <| catMaybes statements

haskellStatement :: Parser (Maybe (Edge, Node))
haskellStatement = choice [Just <$> haskellImport, Nothing <$ dontCare]

haskellImport :: Parser (Edge, Node)
haskellImport = do
  string "import"
  space1
  name <-
    some
      ((oneOf <| '.' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) <?>
       "import")
  -- TODO: HaskellModule vs HaskellIdentifier
  return (Imports, Haskell (pack name) [])

dontCare :: Parser ()
dontCare = do
  someTill anyChar eol
  pure ()
