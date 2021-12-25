module Day11(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Either as E
import Aoc


type Position = (Int, Int)
type Width = Int
type Height = Int
type EnergyMap = [[Int]]


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = do
  energyLevels <- readInput "./resources/day11.txt"
  let xs = iterate step energyLevels
  let result = sum . map (countFlashed) . take 101 $ xs
  return result

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = do
  energyLevels <- readInput "./resources/day11.txt"
  let xs = iterate step energyLevels
  let result = head . dropWhile (not . isSynchronizedFlash . fst) $ (zip xs [0..])
  return (snd result)

{-- Helpers --}

step :: EnergyMap -> EnergyMap
step emap = loop (flashPoints bumped) bumped
  where
    bumped = bumpEnergyLevel emap
    loop []     emap = resetFlashed emap
    loop (x:xs) emap = let flashed = flash emap x
                           newFlashPoints = flashPoints flashed
                       in loop (nub (xs ++ newFlashPoints)) flashed

bumpEnergyLevel :: EnergyMap -> EnergyMap
bumpEnergyLevel = map (map (+1))

flashPoints :: EnergyMap -> [Position]
flashPoints emap = [ (x,y) | x <- [0 .. width-1], y <- [0 .. height-1], energyAt emap (x,y) == 10]
  where (width, height) = dim emap

flash :: EnergyMap -> Position -> EnergyMap
flash emap (x, y) = foldl' (bump) emap $ (x, y) : (neighbours emap (x, y))
  where
    bump :: EnergyMap -> Position -> EnergyMap
    bump emap (a, b) = let bumped = 1 + (energyAt emap (a, b))
                       in updateMap (a,b) bumped emap

resetFlashed :: EnergyMap -> EnergyMap
resetFlashed = map (map (\energy -> if energy > 9 then 0 else energy))

countFlashed :: EnergyMap -> Int
countFlashed = sum . map (\x -> if x==0 then 1 else 0) . concat

isSynchronizedFlash :: EnergyMap -> Bool
isSynchronizedFlash = (==0) . sum . concat

updateMap :: Position -> Int -> EnergyMap -> EnergyMap
updateMap (x,y) value emap = updateAt y (updateAt x value (emap !! y)) emap

neighbours :: EnergyMap -> Position -> [Position]
neighbours emap (x, y) = starPattern
  where
    (width, height) = dim emap
    valid (a, b) = a >= 0 && a < width && b >= 0 && b < height
    starPattern = [(a, b) | a <- [x-1 .. x+1], b <- [y-1 .. y+1], (a, b) /= (x, y), valid (a,b)]

energyAt :: EnergyMap -> Position -> Int
energyAt emap (x,y) = emap !! y !! x

dim :: EnergyMap -> (Width, Height)
dim emap = (length . head $ emap, length emap)

readInput :: String -> IO EnergyMap
readInput filename = do
  readData (map digitToInt) filename