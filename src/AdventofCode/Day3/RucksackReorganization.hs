module AdventofCode.Day3.RucksackReorganization (solution) where

import Utility

import Data.Char
import Data.List

result1 :: IO String
result1 = show <$> prioritySum <$> inputList

result2 :: IO String
result2 = show <$> prioritySum3 <$> inputList

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn

input :: IO String
input = readFile $ aocPath ++ "Day3/input.txt"

inputList = lines <$> input

prioritySum3 :: [String] -> Int
prioritySum3 [] = 0
prioritySum3 ls = (priority $ commonElem3 a b c) + prioritySum3 xs
  where (a:b:c:xs) = ls

prioritySum :: [String] -> Int
prioritySum [] = 0
prioritySum (x:xs) = (priority $ commonElem a b) + prioritySum xs
  where (a, b) = splitStr x

splitStr :: String -> (String, String)
splitStr str = (take i str, drop i str)
  where i = (length str) `div` 2

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- assume that there is a common element
commonElem :: String -> String -> Char
commonElem (x:xs) ys = if x `elem` ys
                       then x
                       else commonElem xs ys

commonElem3 :: String
            -> String
            -> String 
            -> Char
commonElem3 (x:xs) ys zs = 
  if x `elem` ys &&
     x `elem` zs
  then x
  else commonElem3 xs ys zs                        

priority :: Char -> Int
priority c
  | isLower c = (ord c) - 96
  | isUpper c = (ord c) - 38                    