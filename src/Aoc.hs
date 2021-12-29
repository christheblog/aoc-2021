module Aoc(readData,
  parseInt,
  chunksOf,
  splitOn,
  nubOrd,
  count,
  replace,
  replaceSubOne,
  updateAt,
  mapAt,
  nonEmpty,
  orf,
  andf,
  sortDesc) where

import System.IO
import Control.Monad
import Data.Ord
import Data.Sort
import Data.List
import qualified Data.Map as M

{-- Parsing helpers --}

-- Helper to read input data and apply a parsing function to the raw file input
readData :: (String -> a) -> String -> IO [a]
readData parseLn filename = do
  content <- readFile filename
  let fileLines = lines content
  let parsed = map (parseLn) fileLines
  return parsed

parseInt :: String -> Int
parseInt = read


{-- Lists helpers --}

nonEmpty :: [a] -> Bool
nonEmpty = not . null

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunksOf n zs

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep xs = (takeWhile (/= sep) xs) : (splitOn sep (dropFirstIfSep . dropWhile (/= sep) $ xs))
  where dropFirstIfSep [] = []
        dropFirstIfSep (x:xs)
          | x == sep = xs
          | otherwise = x:xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\a -> if a==x then y else a)

replaceSubOne :: Eq a => [a] -> [a] -> [a] -> [a]
replaceSubOne _ _ [] = []
replaceSubOne [] rep xs = xs
replaceSubOne pat [] xs = xs
replaceSubOne pat repl ls = replaceSub [] [] pat ls
  where
    replaceSub rem matched [] xs = rem ++ repl ++ xs
    replaceSub rem matched _ [] = ls
    replaceSub rem matched (p:ps) (x:xs) = 
      let full = matched ++ (x:xs) in
      if p==x then replaceSub rem (matched ++ [p]) ps xs
      else replaceSub (rem ++ [head full]) [] pat (tail full)

updateAt :: Int -> a -> [a] -> [a]
updateAt index x xs =
  let (left, right) = splitAt index xs in left ++ [x] ++ (tail right)

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt index f xs =
  let (left, right) = splitAt index xs in left ++ [f . head $ right] ++ (tail right)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Down)

-- A faster nub for Ord elements (nub is O(n^2)
nubOrd:: Ord a => [a] -> [a]
nubOrd = reverse . snd . foldl' (step) (M.empty, [])
  where
    step (visited, res) x = if M.notMember x visited then (M.insert x () visited, x:res) else (visited, res)

{-- Function helpers --}

orf :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
orf f g = (\x -> f x || g x)

andf :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andf f g = (\x -> f x && g x)
