module Ingest
  ( Ingester
  , ingester
  , ingest
  ) where

import Data.Graph.Inductive.Graph
       (LEdge, LNode, empty, insEdges, insNodes, labEdges, labNodes,
        mkGraph, prettyPrint)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Hashable (hash)
import Data.Text (Text, pack)
import Flow
import System.Directory.PathWalk (pathWalkAccumulate)
import System.FilePath (FilePath, combine, takeExtension)
import System.FilePath.Glob (Pattern, match)

-- | this is also Object, but too many type aliases :S
type Subject = Text

type Predicate = Text

type FileInfo = (Subject, [(Predicate, Subject)])

data Ingester = Ingester
  { forFiles :: Pattern
  , parse :: FilePath -> IO FileInfo
  }

instance Show Ingester where
  show (Ingester forFiles _) = "Ingester for (" ++ show forFiles ++ ")"

-- I don't feel great about this, but it seems to be the best way to
-- get a Monoid instance for Graph... I don't think it can be in the
-- inductive graph library proper since this isn't valid for all kinds
-- of graphs, but for ours it is so... ok?
newtype Graph =
  Graph (Gr Subject Predicate)

instance Monoid (Graph) where
  mempty = Graph empty
  mappend (Graph x) (Graph y)
    -- note that the order matters here... nodes must come before
    -- edges since the graph library rightly refuses to add dangling
    -- eges.
   = x |> (insNodes <| labNodes y) |> (insEdges <| labEdges y) |> Graph

instance Show (Graph) where
  show (Graph g) = show g

unwrap :: Graph -> Gr Subject Predicate
unwrap (Graph g) = g

ingester :: Pattern -> (FilePath -> IO FileInfo) -> Ingester
ingester = Ingester

-- ingest :: [Ingester] -> FilePath -> IO (Gr Subject Predicate)
ingest :: [Ingester] -> FilePath -> IO ()
ingest ingesters root = do
  graph <- pathWalkAccumulate root <| ingestDirectory ingesters
  prettyPrint <| unwrap graph
  pure ()

-- this signature is weird because it's meant to be used by pathWalk
ingestDirectory ::
     [Ingester] -> FilePath -> [FilePath] -> [FilePath] -> IO (Graph)
ingestDirectory ingesters dir _ files = do
  let fullPaths = map (combine dir) files
  -- TODO: strict evaluation for FS operations
  matched <- mapM (ingestFiles fullPaths) ingesters
  pure <|
    case flatten matched of
      ([], []) -> mempty
      (nodes, edges) -> Graph <| mkGraph nodes edges

ingestFiles :: [FilePath] -> Ingester -> IO ([LNode Subject], [LEdge Predicate])
ingestFiles files ingester = do
  let interesting = filter (match (forFiles ingester)) files
  nodesAndEdges <- mapM (ingestFile ingester) interesting
  pure <| flatten nodesAndEdges

ingestFile :: Ingester -> FilePath -> IO ([LNode Subject], [LEdge Predicate])
ingestFile (Ingester _ getInfo) filepath = do
  (subject, relations) <- getInfo filepath
  -- edges
  let out =
        relations ++
        [ ("extension", filepath |> takeExtension |> drop 1 |> pack)
        , ("path", pack filepath)
        ] -- TODO: treat these as a set and don't override if predicate already set
  let nodes = toNode subject : map (\(_, obj) -> toNode obj) out
  let edges = map (\(pred, obj) -> toEdge subject pred obj) out
  pure (nodes, edges)

toNode :: Subject -> LNode Subject
toNode label = (hash label, label)

toEdge :: Subject -> Predicate -> Subject -> LEdge Predicate
toEdge subject predicate object = (hash subject, hash object, predicate)

flatten ::
     [([LNode Subject], [LEdge Predicate])]
  -> ([LNode Subject], [LEdge Predicate])
flatten everything =
  foldr
    (\(newNodes, newEdges) (nodes, edges) ->
       (newNodes ++ nodes, newEdges ++ edges))
    ([], [])
    everything
