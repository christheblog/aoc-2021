module Main where

import System.Environment
import System.IO

import Day12

main :: IO ()
main = do
  res <- solvePart2
  _   <- putStrLn . show $ res
  return ()
