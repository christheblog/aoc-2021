{-# LANGUAGE OverloadedStrings #-}
module Day5(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T


data Point = Point { x:: Int, y::Int } deriving (Eq, Ord, Show)
data Line = Line Point Point deriving (Show)


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = solveFor (horizontalOrVertical)

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = solveFor (allLines)


solveFor :: (Line -> Bool) -> IO Int
solveFor lineFilter = do
  lines <- readData (parseLine) $ "./resources/day5.txt"
  let filtered = filter (lineFilter) lines
  let cover = computeCoverage (Map.empty) . concatMap coverage $ filtered
  return (countOverlapping cover)

{-- Helpers --}

countOverlapping :: Map.Map Point Int -> Int
countOverlapping = count (> 1) . Map.elems

computeCoverage :: Map.Map Point Int -> [Point] -> Map.Map Point Int
computeCoverage map = foldr (\x acc -> coverPoint acc x) (Map.empty)

coverPoint :: Map.Map Point Int -> Point -> Map.Map Point Int
coverPoint map p = let n = Map.findWithDefault 0 p map in
  Map.insert p (n+1) map

coverage :: Line -> [Point]
coverage l@(Line p1 p2) 
  | vertical l   = map (Point (x p1)) [min (y p1) (y p2) .. max (y p1) (y p2)]
  | horizontal l = map (\abs -> Point abs (y p1)) [min (x p1) (x p2) .. max (x p1) (x p2)]
  | otherwise    = diagonal p1 p2
  where diagonal p1 p2 = map (\(a,b) -> Point a b) (xs `zip` ys)
          where xs = [(x p1), if (x p1) > (x p2) then (x p1) - 1 else (x p1) + 1 .. (x p2)]
                ys = [(y p1), if (y p1) > (y p2) then (y p1) - 1 else (y p1) + 1 .. (y p2)]

horizontalOrVertical :: Line -> Bool
horizontalOrVertical = vertical `orf` horizontal

allLines :: Line -> Bool
allLines _ = True

vertical :: Line -> Bool
vertical (Line p1 p2) = (x p1) == (x p2)

horizontal :: Line -> Bool
horizontal (Line p1 p2) = (y p1) == (y p2)

readInput :: String -> IO [Line]
readInput filename = do
  content <- readFile filename
  let lns = map (parseLine) . lines $ content
  return lns

parseLine :: String -> Line
parseLine l = Line (head points) (last points)
  where
    points = map (parsePoint) . splitOn '|' . (replSubstr " -> " "|")  $ l
    parsePoint p = Point (head coords) (last coords)
      where coords = map (parseInt) . splitOn ',' $ p
    replSubstr needle rep haystack = T.unpack . T.replace (T.pack needle) (T.pack rep) $ (T.pack haystack)
