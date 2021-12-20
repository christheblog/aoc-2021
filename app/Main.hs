module Main where

import System.Environment
import System.IO

import Day6

main :: IO ()
main = do
  res <- solvePart2
  _   <- putStrLn . show $ res
  return ()
