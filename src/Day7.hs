module Day7(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import qualified Data.Map as M


type HPosition = Int
type Positions = M.Map HPosition Int

{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = solveWith cstCost

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = solveWith arithmeticCost


solveWith :: (Int -> Positions -> Int) -> IO Int
solveWith cost = do
  hPositions <- readInput "./resources/day7.txt"
  let (l, u) = (minimum . M.keys $ hPositions, maximum . M.keys $ hPositions)
  let minCost = minimum . map (\pos -> cost pos hPositions) $ [l .. u]
  return minCost


{-- Helpers --}

cstCost :: Int -> Positions -> Int
cstCost target = sum . map (\(hpos,n) -> (abs (hpos - target)) * n) . M.toList

arithmeticCost :: Int -> Positions -> Int
arithmeticCost target = sum . map (\(hpos,n) -> (cost . abs $ hpos - target) * n) . M.toList
  where cost x = sum . take (abs x) $ [1 ..] -- could use sum of an arithmetic sequence of differece=1 here

readInput :: String -> IO Positions
readInput filename = do
  content <- readFile filename
  let hPositions = map (parseInt) . splitOn ',' $ content
  return (M.fromListWith (+) . map (\x -> (x,1)) $ hPositions)