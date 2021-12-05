module Day3(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import Data.List


data Bit = Zero | One deriving Eq
type BinaryNum = [Bit]


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = do
  numbers <- readData (parse) $ "./resources/day3.txt"
  let gammaBits = map (mostCommon) . transpose $ numbers
  let epsilon = toDec . map (flipBit) $ gammaBits
  return ((toDec gammaBits) * epsilon)

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = do
  numbers <- readData (parse) $ "./resources/day3.txt"
  let oxygen = toDec . keepOnlyOne mostCommon 0
  let co2    = toDec . keepOnlyOne leastCommon 0
  return (oxygen numbers * co2 numbers)

keepOnlyOne :: (BinaryNum -> Bit) -> Int -> [BinaryNum] -> BinaryNum
keepOnlyOne bitFilter n numbers = case kept of
  []  -> error "No number after filtering"
  [x] -> x
  _   -> keepOnlyOne bitFilter (n+1) kept
  where transposed = transpose numbers
        criteria = bitFilter (transposed !! n)
        kept = filter (\num -> num !! n == criteria) numbers

{-- Helpers --}

parse :: [Char] -> BinaryNum
parse = map (bit)
  where bit '0' = Zero
        bit _   = One

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

toDec :: BinaryNum -> Int
toDec = foldl' (\acc x -> acc * 2 + toBit x) 0
  where toBit Zero = 0
        toBit One  = 1

mostCommon :: BinaryNum -> Bit
mostCommon xs = let (zeros, ones) = partition (== Zero) xs in
  if length zeros > length ones then Zero else One

leastCommon :: BinaryNum -> Bit
leastCommon = flipBit . mostCommon