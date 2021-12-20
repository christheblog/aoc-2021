module Day6(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import qualified Data.Map as M

type Timer = Int
type State = M.Map Timer Int

{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = solveFor 80

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = solveFor 256


solveFor :: Int -> IO Int
solveFor n = do
  timers <- readInput "./resources/day6.txt"
  let result = populationCount . simulate n $ timers
  return result

{-- Helpers --}

populationCount :: State -> Int
populationCount = sum . M.elems

simulate :: Int -> State -> State
simulate n timers | n <= 1    = simulateOne timers
simulate n timers | otherwise = simulate (n-1) (simulateOne timers)

simulateOne :: State -> State
simulateOne m = foldr (\(timer, count) acc -> M.unionWith (+) (nextGen timer count) acc) (M.empty) timers
  where timers = M.assocs m
        nextGen n count = if n==0 then M.fromList [(6,count), (8,count)] else M.singleton (n-1) count

readInput :: String -> IO State
readInput filename = do
  content <- readFile filename
  let timers = map (parseInt) . splitOn ',' $ content
  return (M.fromListWith (+) . map (\x -> (x,1)) $ timers)



