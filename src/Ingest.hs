module Ingest
  ( Ingester
  , ingester
  , ingest
  ) where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text (Text, pack)
import Flow
import System.Directory.PathWalk (pathWalk)
import System.FilePath (FilePath, combine, takeExtension)
import System.FilePath.Glob (Pattern, match)

data Ingester = Ingester
  { forFiles :: Pattern
  , nodeName :: FilePath -> IO Text
  , relations :: FilePath -> IO [(Text, Text)]
  }

instance Show Ingester where
  show (Ingester forFiles _ _) = "Ingester for (" ++ show forFiles ++ ")"

ingester ::
     Pattern
  -> (FilePath -> IO Text)
  -> (FilePath -> IO [(Text, Text)])
  -> Ingester
ingester = Ingester

-- ingest :: [Ingester] -> FilePath -> IO (Gr Text Text)
ingest :: [Ingester] -> FilePath -> IO ()
ingest ingesters root = do
  pathWalk root <| \dir subdirs files -> do
    let fullPaths = map (combine dir) files
    matched <-
      ingesters |> mapM (ingestFilesWithIngester fullPaths) |> fmap concat
    case matched of
      [] -> pure ()
      _ -> putStrLn (show matched)
  -- 2a. getting the node names and relations for those ingesters whose `forFiles` match
  -- 3. convert that information to something I can make into a graph
  -- 4. return the graph, hooray!
  pure ()

ingestFilesWithIngester :: [FilePath] -> Ingester -> IO [(Text, Text, Text)]
ingestFilesWithIngester files ingester = do
  filter (match (forFiles ingester)) files |>
    mapM (ingestFileWithIngester ingester) |>
    fmap concat

ingestFileWithIngester :: Ingester -> FilePath -> IO [(Text, Text, Text)]
ingestFileWithIngester (Ingester _ getNodeName getRelations) filepath = do
  nodeName <- getNodeName filepath
  relations <- getRelations filepath
  let builtin =
        [ ("extension", filepath |> takeExtension |> drop 1 |> pack)
        , ("path", pack filepath)
        ]
  (relations ++ builtin) |> map (\(pred, obj) -> (nodeName, pred, obj)) |> pure
