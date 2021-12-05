module Day2(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Aoc
import Data.List


data Command =
  Forward Int
  | Up Int
  | Down Int

data Position = Pos {
  horizontal:: Int,
  depth:: Int,
  aim:: Int
 } deriving (Show, Eq)

{-- Part 1 -}

solve:: (Position -> Command -> Position) -> IO Int
solve f = do
  commands <- readData (parseCommand) "./resources/day2.txt"
  let Pos { horizontal = h, depth = d, aim = _ } = foldl f initPos $ commands
  return (h * d)

solvePart1:: IO Int
solvePart1 = solve applyCommand1

applyCommand1:: Position -> Command -> Position
applyCommand1 pos (Forward n) = setHorizontal (n + (horizontal pos)) pos
applyCommand1 pos (Up n)      = setDepth ((depth pos) - n) pos
applyCommand1 pos (Down n)    = setDepth ((depth pos) + n) pos


{-- Part 2 -}

solvePart2:: IO Int
solvePart2 = solve applyCommand2

applyCommand2:: Position -> Command -> Position
applyCommand2 pos (Forward n) = setHorizontal ((horizontal pos) + n) . setDepth ((depth pos) + ((aim pos) * n)) $ pos
applyCommand2 pos (Up n)      = setAim ((aim pos) - n) pos
applyCommand2 pos (Down n)    = setAim ((aim pos) + n) pos


{-- Helpers --}

parseCommand:: String -> Command
parseCommand cmd = case words cmd of
  ["forward", n] -> Forward (read n:: Int)
  ["up", n]      -> Up (read n:: Int)
  ["down", n]    -> Down (read n:: Int)
  _              -> error ("Invalid command: " ++ cmd)

initPos:: Position
initPos = Pos { horizontal = 0, depth = 0, aim = 0}

setHorizontal:: Int -> Position -> Position
setHorizontal n Pos { depth = d, aim = a } = Pos { horizontal = n, depth = d, aim = a }

setDepth:: Int -> Position -> Position
setDepth n Pos { horizontal = h, aim = a } = Pos { horizontal = h, depth = n, aim = a }

setAim:: Int -> Position -> Position
setAim n Pos { horizontal = h, depth = d } = Pos { horizontal = h, depth = d, aim = n }
