{-

- get all groups of numbers
- add the numbers
- get the highest number

-}

module AdventofCode.Day1.CalorieCounting (solution) where

import Utility (aocPath)

grouped :: [String] -> [Integer]
grouped [] = []
grouped xs = y : grouped ys
  where (y, ys) = getNumsFromList xs

splitAtNull :: [String] -> ([String], [String])
splitAtNull = span (not . null)

-- assumes that the list of strings can be coerced to Integer
makeNumAndAdd :: [String] -> Integer
makeNumAndAdd []     = 0
makeNumAndAdd (x:xs) = (read x :: Integer) + makeNumAndAdd xs

removeBeginningNull :: [String] -> [String]
removeBeginningNull [] = []
removeBeginningNull l@(x:xs)
  | null x    = xs
  | otherwise = l

getNumsFromList :: [String] -> (Integer, [String])
getNumsFromList [] = (0, [])
getNumsFromList ls = (makeNumAndAdd nums, removeBeginningNull xs)
  where (nums, xs) = splitAtNull ls

topNums :: Int -> [Integer] -> Integer
topNums 0 ls = 0
topNums n ls = m + (topNums (n - 1) $ removeOneNumFromList m ls)
  where m = maximum ls

removeOneNumFromList :: (Num a, Eq a) => a -> [a] -> [a]
removeOneNumFromList _ [] = []
removeOneNumFromList n (x:xs)
  | n == x    = xs
  | otherwise = x : removeOneNumFromList n xs

input :: IO [String]
input = lines <$> readFile inputPath

inputPath :: FilePath
inputPath = aocPath ++ "/Day1/input.txt"

numList :: IO [Integer]
numList = grouped <$> input

result1 :: IO String
result1 = show <$> maximum <$> numList

result2 :: IO String
result2 = show <$> topNums 3 <$> numList

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn