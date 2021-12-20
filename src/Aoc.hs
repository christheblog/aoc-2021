module Aoc(readData, parseInt, chunksOf, splitOn, count, replace, replaceSubOne, nonEmpty, orf, andf) where

import System.IO
import Control.Monad

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



{-- Function helpers --}

orf :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
orf f g = (\x -> f x || g x)

andf :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andf f g = (\x -> f x && g x)
