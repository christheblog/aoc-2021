module Day4(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import Data.List


data Cell = Unmarked Int | Marked deriving (Eq, Ord, Show)
data Bingo = Board [[Cell]] deriving (Eq, Show)


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = findBoard firstWinner

solvePart2 :: IO Int
solvePart2 = findBoard lastWinner

findBoard :: ([Int] -> [Bingo] -> (Int, Bingo, [Bingo])) -> IO Int
findBoard f = do
  (rs, boards) <- readInput "./resources/day4.txt"
  let (called, win, rem) = f rs $ boards
  return (score called win)

{-- Helpers --}

firstWinner :: [Int] -> [Bingo] -> (Int, Bingo, [Bingo])
firstWinner [] boards     = error "No more number to draw"
firstWinner (n:ns) boards = case identifyWinner markedBoards of
  Just win -> (n, win, filter (not . winner) markedBoards)
  Nothing  -> firstWinner ns markedBoards
  where markedBoards = draw n boards
        identifyWinner = find (winner)

lastWinner :: [Int] -> [Bingo] -> (Int, Bingo, [Bingo])
lastWinner ns boards =
  case (firstWinner ns boards) of
    (called, winBoard, [])        -> (called, winBoard, [])
    (called, winBoard, remainder) -> lastWinner (tail ns) remainder

draw :: Int -> [Bingo] -> [Bingo]
draw n = map (mark n)

mark :: Int -> Bingo -> Bingo
mark n (Board board) = Board . map (replace (Unmarked n) Marked) $ board

score :: Int -> Bingo -> Int
score called (Board cells) = called * (unmarked cells)
  where unmarked = foldl' (step) 0 . concat
          where step acc (Unmarked n) = acc + n
                step acc _ = acc

winner :: Bingo -> Bool
winner (Board cells) = (any winningRow $ cells) || ((any winningRow) . transpose $ cells)
  where winningRow = all (isMarked)
        isMarked Marked = True
        isMarked _      = False

readInput :: String -> IO ([Int], [Bingo])
readInput filename = do
  content <- readFile filename
  let (x : xs) = filter (nonEmpty) . lines $ content
  return (readRandomSeq x, readBoards xs)
  where
    readRandomSeq = map (parseInt) . splitOn ','
    readBoards = map (readOne) . chunksOf 5
      where readOne = Board . map (map (Unmarked . parseInt) . words)