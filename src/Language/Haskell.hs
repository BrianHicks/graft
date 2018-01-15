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
  (name, exports) <- haskellModule <|> (pure <| (Module (pack filepath), []))
  edges <- haskellBody
  return (name, exports ++ edges)

haskellModule :: Parser (Node, [(Edge, Node)])
haskellModule = do
  string "module"
  space1
  name <- takeWhile1P (Just "module name") (not . isSpace)
  space1
  rawExports <- haskellExports <|> pure []
  space
  string "where"
  ---
  let exports =
        rawExports |> map (\e -> Data.Text.concat [name, ".", e]) |>
        map Identifier |>
        map ((,) Exports)
  return <| (Module name, exports)

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
  string "(..)" <|> ""
  space
  string "," <|> ""
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
  string "qualified" <|> ""
  space
  name <-
    some
      ((oneOf <| '.' : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']) <?>
       "import")
  -- TODO: HaskellModule vs HaskellIdentifier
  return (Imports, Module (pack name))

dontCare :: Parser ()
dontCare = do
  someTill anyChar eol
  pure ()
