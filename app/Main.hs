module Main where

import Data.Graph.Inductive.Graph (labEdges, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (intersperse)
import Data.Text (Text, pack)
import Flow
import Ingest
import System.FilePath
import System.FilePath.Glob (compile)
import Text.Regex (Regex, matchRegex, matchRegexAll, mkRegex)

main :: IO ()
main = do
  graph <- ingest [testIngester] "."
  -- until we get fancy, let's make a graphviz graph out of these.
  putStrLn $ digraph graph

digraph :: Gr Text Text -> String
digraph graph =
  let nodes =
        labNodes graph |>
        map (\(id, label) -> quoted id ++ "[label=" ++ show label ++ "];")
      edges =
        labEdges graph |>
        map
          (\(subj, obj, label) ->
             quoted subj ++
             " -> " ++ quoted obj ++ "[label=" ++ show label ++ "];")
  in "digraph {\n" ++ join "\n" nodes ++ "\n" ++ join "\n" edges ++ "\n}"

join :: String -> [String] -> String
join char items = items |> intersperse char |> concat

quoted :: Show a => a -> String
quoted a = "\"" ++ show a ++ "\""

testIngester :: Ingester
testIngester =
  let interestingFiles = compile "**/*.hs"
      haskellModule = mkRegex "^module ([a-zA-Z\\.]+).*$"
      haskellImport = mkRegex "^import ([a-zA-Z\\.]+)( \\(.+\\))?$"
      parse filepath = do
        contents <- readFile filepath
        let name =
              case matchRegex haskellModule contents of
                Just [moduleName] -> pack moduleName
                _ -> pack filepath
        let imports =
              matchAll haskellImport contents |>
              map
                (\items ->
                   case items of
                     i:_ -> [("imports", pack i)]
                     [] -> []) |>
              concat
        pure (name, ("filetype", "haskell") : imports)
  in ingester interestingFiles parse

matchAll :: Regex -> String -> [[String]]
matchAll = matchAllAcc []

matchAllAcc :: [[String]] -> Regex -> String -> [[String]]
matchAllAcc acc regex contents =
  case matchRegexAll regex contents of
    Just (_, _, next, match) -> matchAllAcc (match : acc) regex next
    Nothing -> reverse acc
