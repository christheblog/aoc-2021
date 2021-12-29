module Day12(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Either as E
import qualified Data.Map as M
import Aoc


data Node = Big String | Small String deriving (Eq, Ord, Show)
type Graph = M.Map Node [Node]
type Path = [Node]


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = solve (validPath)


{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = solve (validPath2)


solve :: (Path -> Bool) -> IO Int
solve p = do
  graph <- readInput "./resources/day12.txt"
  let paths = allPaths p $ graph
  return paths

{-- Helpers --}

startNode = Small "start"
endNode   = Small "end"

-- Optimized DFS eliminating early duplicate and invalid paths
allPaths :: (Path -> Bool) -> Graph -> Int
allPaths valid g = dfs [[startNode]] 0
  where
    dfs [] acc = acc
    dfs (path:ps) acc = dfs newStack newAcc
      where
        newPaths = map (:path) . (filter (/=startNode)) . children (head path) $ g
        newAcc = if null finished then acc else acc + 1
        (finished, unfinished) = partition ((==endNode) . head) newPaths
        validUnfinished = filter (valid) unfinished
        newStack = if null validUnfinished then ps else nubOrd (validUnfinished ++ ps)

children :: Node -> Graph -> [Node]
children n = M.findWithDefault [] n

validPath :: Path -> Bool
validPath path = (length smalls) == (length . nub $ smalls)
  where smalls = smallCaves path

validPath2 :: Path -> Bool
validPath2 path =
    ((countSmall == countUniqueSmall) || (countSmall == countUniqueSmall + 1))
  where
    smalls = smallCaves path
    countSmall = length smalls
    countUniqueSmall = length . nubOrd $ smalls

smallCaves :: Path -> Path
smallCaves path = smalls
  where
    smalls = filter (isSmall) path
    isSmall (Small _) = True
    isSmall _         = False

readInput :: String -> IO Graph
readInput filename = do
    edges <- readData (readEdge) $ filename
    return (graphFrom edges)
  where
    readEdge = edgeFrom . splitOn '-'
    edgeFrom xs = (nodeFrom . head $ xs, nodeFrom . last $ xs)
    nodeFrom s = if isUpper . head $ s then Big s else Small s
    graphFrom = foldl' (\acc (a,b) -> addEdge a b . addEdge b a $ acc) M.empty
    addEdge s e = M.insertWith (++) s [e] . M.insertWith (++) e [s]