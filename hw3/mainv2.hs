{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (try, IOException)
import Data.List (intercalate)

-- types for clarity, MIDs and Names are just Text
type MID = T.Text
type Name = T.Text

-- graph is just a map of MIDs to their neighbor MIDs
type Graph = Map.Map MID [MID]

-- map of MID to the name it refers to
type MIDToName = Map.Map MID Name

-- structure to hold BFS state - queue and visited nodes
data BFSState = BFSState
    { queue :: [(MID, [MID])]  -- keeps MID and path taken so far
    , visited :: Set.Set MID
    } deriving (Show)

-- this loads the mid2name.tsv file into a map
loadMid2Name :: FilePath -> IO MIDToName
loadMid2Name filepath = do
    content <- TIO.readFile filepath
    let linesText = T.lines content -- split into lines
        validLines = filter (not . T.null) linesText -- remove empty lines
        -- try to parse each line into (mid, name)
        parseLine line = case T.splitOn "\t" line of
            [mid, name] -> Just (T.strip mid, T.strip name)
            _ -> Nothing
        pairs = map parseLine validLines
        validPairs = [p | Just p <- pairs] -- remove failed parses
    return $ foldr (\(mid, name) acc ->
        if Map.member mid acc then acc else Map.insert mid name acc)
        Map.empty (reverse validPairs) -- reverse to keep first occurrence

-- load the graph edges from freebase.tsv file
loadGraph :: FilePath -> IO Graph
loadGraph filepath = do
    content <- TIO.readFile filepath
    let linesText = T.lines content
        validLines = filter (not . T.null) linesText
        -- expect lines like source \t predicate \t target
        parseRelation line = case T.splitOn "\t" line of
            [source, _, target] -> Just (T.strip source, T.strip target)
            _ -> Nothing
        relations = [r | Just r <- map parseRelation validLines]
    return $ foldr addEdge Map.empty relations
  where
    -- add edges from source to target
    addEdge (from, to) = Map.insertWith (++) from [to]

-- performs BFS to find the shortest path
bfs :: Graph -> MID -> MID -> Maybe (Int, [MID])
bfs graph start end
    | start == end = Just (0, [start]) -- start and end are the same
    | otherwise = bfsHelper initialState
  where
    initialState = BFSState [(start, [start])] (Set.singleton start)
    
    bfsHelper :: BFSState -> Maybe (Int, [MID])
    bfsHelper (BFSState [] _) = Nothing  -- no more nodes to explore
    bfsHelper (BFSState ((current, path):rest) visitedSet)
        | current == end = Just (length path - 1, reverse path) -- found
        | otherwise = 
            let neighbors = Map.findWithDefault [] current graph
                unvisitedNeighbors = filter (\n -> not (Set.member n visitedSet)) neighbors
                -- extend path to each neighbor
                newPaths = [(neighbor, neighbor : path) | neighbor <- unvisitedNeighbors]
                -- mark neighbors as visited
                newVisited = foldr Set.insert visitedSet unvisitedNeighbors
                newQueue = rest ++ newPaths
            in bfsHelper (BFSState newQueue newVisited)

-- convert MID path to their actual names using the map
pathToNames :: MIDToName -> [MID] -> [Name]
pathToNames mid2name path = map (\mid -> Map.findWithDefault mid mid mid2name) path




main :: IO ()
main = do
    args <- getArgs
    case args of
        [sourceMID, targetMID] -> do
            -- load the MID to Name map and the graph
            mid2name <- loadMid2Name "mid2name.tsv"
            graph <- loadGraph "freebase.tsv"
            
            let source = T.pack sourceMID
                target = T.pack targetMID
            
            -- check if both MIDs are in the name map
            case (Map.lookup source mid2name, Map.lookup target mid2name) of
                (Just sourceName, Just targetName) -> do
                    -- run BFS to find the shortest path
                    case bfs graph source target of
                        Nothing -> do
                            putStrLn $ "No connection between " ++ sourceMID ++ " and " ++ targetMID
                        Just (distance, path) -> do
                            let pathNames = pathToNames mid2name path
                            putStrLn $ "Shortest distance: " ++ show distance
                            putStrLn $ "Full path: " ++ (intercalate " -> " (map T.unpack pathNames))
                -- either source or target wasn't found in mid2name
                (Nothing, _) -> do
                    putStrLn $ "No connection between " ++ sourceMID ++ " and " ++ targetMID
                (_, Nothing) -> do
                    putStrLn $ "No connection between " ++ sourceMID ++ " and " ++ targetMID
        -- wrong number of arguments
        _ -> do
            putStrLn "Usage: pathfinder <source_MID> <target_MID>"
            exitFailure
