module Ingest
  ( Ingester
  , ingester
  , ingest
  ) where

import Data.Graph.Inductive.Graph (LEdge, LNode)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Hashable (hash)
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
    matched <- ingesters |> mapM (ingestFiles fullPaths)
    case flattenNodesAndEdges matched of
      ([], []) -> pure ()
      _ -> putStrLn (show matched)
  -- 2a. getting the node names and relations for those ingesters whose `forFiles` match
  -- 3. convert that information to something I can make into a graph
  -- 4. return the graph, hooray!
  pure ()

toNode :: Text -> LNode Text
toNode label = (hash label, label)

toEdge :: Text -> Text -> Text -> LEdge Text
toEdge subject predicate object = (hash subject, hash object, predicate)

flattenNodesAndEdges ::
     [([LNode Text], [LEdge Text])] -> ([LNode Text], [LEdge Text])
flattenNodesAndEdges everything =
  foldr
    (\(newNodes, newEdges) (nodes, edges) ->
       (newNodes ++ nodes, newEdges ++ edges))
    ([], [])
    everything

ingestFiles :: [FilePath] -> Ingester -> IO ([LNode Text], [LEdge Text])
ingestFiles files ingester = do
  let interesting = filter (match (forFiles ingester)) files
  nodesAndEdges <- mapM (ingestFile ingester) interesting
  pure <| flattenNodesAndEdges nodesAndEdges

ingestFile :: Ingester -> FilePath -> IO ([LNode Text], [LEdge Text])
ingestFile (Ingester _ getNodeName getRelations) filepath = do
  subject <- getNodeName filepath
  relations <- getRelations filepath
  -- edges
  let out =
        relations ++
        [ ("extension", filepath |> takeExtension |> drop 1 |> pack)
        , ("path", pack filepath)
        ] -- TODO: treat these as a set and don't override if predicate already set
  let nodes = toNode subject : map (\(_, obj) -> toNode obj) out
  let edges = map (\(pred, obj) -> toEdge subject pred obj) out
  pure (nodes, edges)
