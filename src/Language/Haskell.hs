module Language.Haskell
  ( haskell
  , Node
  , Edge
  ) where

import Data.Char (isSpace)
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes)
import Data.Text (Text, concat, pack, unpack)
import qualified Data.Text.IO as T
import Data.Void
import Flow
import GHC.Generics (Generic)
import Ingest (Ingester, ingester)
import Prelude (fail) -- TODO: but we should not be failing anyway
import Protolude
import System.FilePath (FilePath)
import System.FilePath.Glob (compile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)

data Node
  = Module Text
  | Identifier Text
  deriving (Show, Generic)

instance Hashable Node

data Edge
  = Imports
  | Exports
  deriving (Show, Generic)

instance Hashable Edge

haskell :: Ingester Node Edge
haskell =
  let interestingFiles = compile "**/*.hs"
      getInfo filepath = do
        contents <- T.readFile filepath
        case parse (parser filepath) filepath contents of
          Left err -> fail <| parseErrorPretty err
          Right success -> return success
  in ingester interestingFiles getInfo

type Parser = Parsec Void Text

parser :: FilePath -> Parser (Node, [(Edge, Node)])
parser filepath = do
  (name, exports) <- moduleStatement <|> (pure <| (Module (pack filepath), []))
  edges <- body
  return (name, exports ++ edges)

moduleStatement :: Parser (Node, [(Edge, Node)])
moduleStatement = do
  string "module"
  space1
  name <- takeWhile1P (Just "module name") (not . isSpace)
  space1
  rawExports <- exports <|> pure []
  space
  string "where"
  ---
  let exports =
        rawExports |> map (qualify name) |> map Identifier |> map ((,) Exports)
  return <| (Module name, exports)

exports :: Parser [Text]
exports = do
  string "("
  exports <- some export
  string ")"
  return exports

export :: Parser Text
export = do
  space
  export <- some alphaNumChar <?> "export"
  string "(..)" <|> ""
  space
  string "," <|> ""
  space
  return <| pack export

body :: Parser [(Edge, Node)]
body = do
  space
  statements <- some statement
  pure <| Protolude.concat (catMaybes statements)

statement :: Parser (Maybe [(Edge, Node)])
statement =
  choice [Just <$> Text.Megaparsec.try importStatement, Nothing <$ dontCare]

importStatement :: Parser [(Edge, Node)]
importStatement = do
  string "import"
  space1
  string "qualified" <|> ""
  space
  name <-
    some
      ((oneOf <| '.' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) <?>
       "import")
  dontCare -- adding imported identifiers created a lot of noise before
  return [(Imports, Module (pack name))]

dontCare :: Parser ()
dontCare = do
  someTill anyChar eol
  pure ()

stringUnit :: Text -> Parser ()
stringUnit s = do
  string s
  return ()

qualify :: Text -> Text -> Text
qualify moduleName identifier = Data.Text.concat [moduleName, ".", identifier]
