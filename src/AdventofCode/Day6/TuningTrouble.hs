module AdventofCode.Day6.TuningTrouble where

import Utility (aocPath)

result1 :: IO String
result1 = show . (findNumDiffChars 4) <$> input

result2 :: IO String
result2 = show . (findNumDiffChars 14) <$> input

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn

input :: IO String
input = readFile $ aocPath ++ "Day6/input.txt"

allDiffChars :: String -> Bool
allDiffChars []  = True
allDiffChars str = diff (length str) str
  where diff n l
          | n < 0     = False
          | n <= 1    = True
          | otherwise = 
            (l !! n') `notElem` (removeElem n l) && (diff n' l )
            where n' = n - 1

-- i is 1-indexed 
removeElem :: Int -> String -> String
removeElem i str =
  (take (i - 1) str) ++ (drop i str)      

findNumDiffChars :: Int -> String -> Int
findNumDiffChars _ []  = 0
findNumDiffChars i str@(x:xs) =
  if allDiffChars (take i str)
    then i
    else 1 + findNumDiffChars i xs
