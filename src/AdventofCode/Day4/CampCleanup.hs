module AdventofCode.Day4.CampCleanup where

import Utility (aocPath)

type Range = (Int, Int)
data Ranges = 
  Ranges Range Range deriving (Show)

result1 :: IO String
result1 = show . sumContainedRanges <$> inputList

result2 :: IO String
result2 = show . sumOverlap <$> inputList

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn


input :: IO String
input = readFile $ aocPath ++ "Day4/input.txt"

inputList :: IO [String]
inputList = lines <$> input

sumContainedRanges :: [String] -> Int
sumContainedRanges [] = 0
sumContainedRanges (x:xs) = sum + sumContainedRanges xs
  where sum = if compareRanges $ makeTuple x
              then 1
              else 0

sumOverlap :: [String] -> Int
sumOverlap [] = 0
sumOverlap (x:xs) = sum + sumOverlap xs
  where sum = if isOverlap $ makeTuple x
              then 1
              else 0            

-- "3-23,5-6" -> Ranges (3, 23) (5, 6)
makeTuple :: String -> Ranges
makeTuple str = Ranges (rngToTuple x) (rngToTuple y)
  where (x, y) = breakChar str ','

breakChar :: String -> Char -> (String, String)
breakChar str ch = (x, y)
  where (x, y') = break (== ch) str
        y = tail y'

-- "2-3" -> (2, 3)
rngToTuple :: String -> (Int, Int)
rngToTuple str = 
  let (x', y') = breakChar str '-'
      x = read x' :: Int
      y = read y' :: Int
  in (x, y)

compareRanges :: Ranges -> Bool
compareRanges (Ranges (a, b) (x, y))
  | a >= x && b <= y = True
  | x >= a && y <= b = True
  | otherwise        = False

isOverlap :: Ranges -> Bool
isOverlap (Ranges (a, b) (x, y))
  | b < x || a > y = False
  | otherwise      = True