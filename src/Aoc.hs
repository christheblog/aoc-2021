module Aoc(readData) where

import System.IO
import Control.Monad

-- Helper to read input data and apply a parsing function to the raw file input
readData :: (String -> a) -> String -> IO [a]
readData parseLn filename = do
  content <- readFile filename
  let fileLines = lines content
  let parsed = map (parseLn) fileLines
  return parsed

