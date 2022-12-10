module AdventofCode.Day2.RockPaperScissors (solution) where

import Utility (aocPath)

type Round = (String, String)

result1 :: IO String
result1 = show <$> score <$> inputList

result2 :: IO String
result2 = show <$> correctScore <$> inputList

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn

input :: IO String
input = readFile $ aocPath ++ "/Day2/input.txt"

inputList :: IO [String]
inputList = lines <$> input

-- input for this is the list of lines'd input
score :: [String] -> Integer
score [] = 0
score (x:xs) = (getRoundScore id (a, b)) + score xs
  where (y:ys:[]) = words x
        (a, b)    = (y, ys)

-- input for this is the list of lines'd input
correctScore :: [String] -> Integer
correctScore [] = 0
correctScore (x:xs) = 
  (getRoundScore getCorrectShapes (a, b)) + correctScore xs
  where (y:ys:[]) = words x
        (a, b)    = (y, ys)

getRoundScore :: (Round -> Round)
              -> Round 
              -> Integer
getRoundScore f rnd = (getMyScore rnd') + (getOtherScore rnd')
  where rnd' = f rnd

-- given a string of two characters (AKA the 
-- result of "words"-ing each round), return
-- the score for the second letter
getMyScore :: Round -> Integer
getMyScore (_, y) = case y of
  "X" -> 1
  "Y" -> 2
  "Z" -> 3
  _   -> 0

getOtherScore :: Round -> Integer
getOtherScore (x, y)
  | isWin (x, y)  = 6
  | isLoss (x, y) = 0
  | otherwise     = 3

isWin :: Round -> Bool
isWin (x, y)
  | x == "A" && y == "Y" = True
  | x == "B" && y == "Z" = True
  | x == "C" && y == "X" = True
  | otherwise            = False

isLoss :: Round -> Bool
isLoss (x, y)
  | x == "A" && y == "Z" = True
  | x == "B" && y == "X" = True
  | x == "C" && y == "Y" = True
  | otherwise            = False

getCorrectShapes :: Round -> Round
getCorrectShapes (x, y)
  | y == "X"  = (x, makeLoss x)
  | y == "Z"  = (x, makeWin x)
  | otherwise = (x, opponentToPlayer x)

opponentToPlayer :: String -> String
opponentToPlayer s = case s of
  "A" -> "X"
  "B" -> "Y"
  "C" -> "Z"

-- The two 'make' functions take as input
-- what piece the opponent will play, and
-- return the piece that you should play
makeWin :: String -> String
makeWin s = case s of
  "A" -> "Y"
  "B" -> "Z"
  "C" -> "X"

makeLoss :: String -> String
makeLoss s = case s of
  "A" -> "Z"
  "B" -> "X"
  "C" -> "Y"