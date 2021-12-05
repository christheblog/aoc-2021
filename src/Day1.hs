module Day1(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import Data.List


{-- Part 1 --}

solvePart1:: IO Int
solvePart1 = do
  depths <- readData (\x -> read x :: Int) "./resources/day1.txt"
  let larger = filter (\(a,b) -> a > b) . zip (tail depths) $ depths
  return (length larger)


{-- Part 2 --}

solvePart2:: IO Int
solvePart2 = do
  depths <- readData (\x -> read x :: Int) "./resources/day1.txt"
  let slided = map (sum) . window 3 $ depths
  let larger = filter (\(a,b) -> a > b) . zip (tail slided) $ slided
  return (length larger)

{-- Helpers --}

-- sliding window of n elements
window:: Int -> [Int] -> [[Int]]
window n = foldr (zipWith (:)) (repeat []) . take n . tails


