module Day10(solvePart1, solvePart2) where

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Either as E
import Aoc


{-- Part 1 --}

solvePart1 :: IO Int
solvePart1 = do
 navigationLines <- readData (id) "./resources/day10.txt"
 let errors = E.lefts . map (matchParens) $ navigationLines
 let result = sum . map (score) $ errors
 return result

{-- Part 2 --}

solvePart2 :: IO Int
solvePart2 = do
  navigationLines <- readData (id) "./resources/day10.txt"
  let incompletes = E.rights . map (matchParens) $ navigationLines
  let result = median . map (scoreCloseSeq) . map (map close) $ incompletes
  return result


{-- Helpers --}

brackets = [('(',')'), ('[',']'), ('{','}'), ('<','>')]
openingBrackets = map (fst) brackets
closingBrackets = map (fst) brackets

matchParens :: String -> Either Char [Char]
matchParens = foldl' (\q char -> case q of
    Left c                           -> Left c
    Right qs     | isOpening char    -> Right (char : qs)
    Right []     | isClosing char    -> Left char
    Right (q:qs) | isMatching q char -> Right qs
    Right _                          -> Left char
  ) (Right [])

matchClosingParens :: [Char] -> [Char]
matchClosingParens = map (close)

isCorrupted :: String -> Bool
isCorrupted = E.isLeft . matchParens

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score other  = error $ "Invalid '" ++ [other] ++ "' for scoring"

scoreCloseSeq :: [Char] -> Int
scoreCloseSeq = foldl' (\acc c -> (5 * acc) + scoreBracket c) 0
  where
    scoreBracket ')' = 1
    scoreBracket ']' = 2
    scoreBracket '}' = 3
    scoreBracket '>' = 4
    scoreBracket _ = error "Not a closing bracket"

median :: [Int] -> Int
median xs = (!! (length xs `div` 2)) . sort $ xs

isOpening :: Char -> Bool
isOpening = (`elem` openingBrackets)

isClosing :: Char -> Bool
isClosing = (`elem` closingBrackets)

isMatching :: Char -> Char -> Bool
isMatching o c = (o,c) `elem` brackets

close :: Char -> Char
close o = head . maybeToList . fmap (snd) . find ((==o) . fst) $ brackets


