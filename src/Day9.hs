module Day9(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Data.Char
import Data.List
import Aoc

type Coordinates = (Int, Int)
type Heatmap = [[Int]]


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = do
 heatmap <- readInput "./resources/day9.txt"
 let result = sum . map (riskLevel) . lowPointHeights $ heatmap
 return result

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = do
  heatmap <- readInput "./resources/day9.txt"
  let basinSizes = map (length) . map (basin heatmap) . lowPoints $ heatmap
  let result = product . take 3 . sortDesc $ basinSizes
  return result


{-- Helpers --}

riskLevel :: Int -> Int
riskLevel = (+1)

lowPointHeights :: Heatmap -> [Int]
lowPointHeights heatmap = map (heightAt heatmap) . lowPoints $ heatmap

lowPoints :: Heatmap -> [Coordinates]
lowPoints heatmap = [ (x,y) | x <- [0 .. width-1],
                              y <- [0 .. height-1],
                              isLowPoint heatmap (x,y)]
  where (width, height) = (fst . dim $ heatmap, snd . dim $ heatmap)

isLowPoint :: Heatmap -> Coordinates -> Bool
isLowPoint heatmap (x,y) = checkLowPoint (heightAt heatmap (x,y)) (neighbourHeights (x,y) heatmap)
  where checkLowPoint height ngbh = (> height) . minimum $ ngbh

neighbourHeights :: Coordinates -> Heatmap -> [Int]
neighbourHeights (x,y) heatmap = map (heightAt heatmap) . neighbours (x,y) $ heatmap

neighbours :: Coordinates -> Heatmap -> [Coordinates]
neighbours (x,y) heatmap = filter (valid) $ crossPattern
  where crossPattern = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        (width, height) = (fst . dim $ heatmap, snd . dim $ heatmap)
        valid (a,b) = a >= 0 && b >= 0 && a < width && b < height

heightAt :: Heatmap -> Coordinates -> Int
heightAt heatmap (x,y) = heatmap !! y !! x

basin :: Heatmap -> Coordinates -> [Coordinates]
basin heatmap lowPoint = collectPoints [lowPoint] []
  where
    collectPoints :: [Coordinates] -> [Coordinates] -> [Coordinates]
    collectPoints [] acc = nub acc
    collectPoints (x:xs) acc = collectPoints (xs ++ ngbhs) $ (x : acc)
      where ngbhs = filter ((< 9) . heightAt heatmap) . filter (`notElem` (x : acc)) . neighbours x $ heatmap

dim :: Heatmap -> Coordinates
dim heatmap = (length (head heatmap), length heatmap)

readInput :: String -> IO Heatmap
readInput filename = do
  readData (map digitToInt) filename

