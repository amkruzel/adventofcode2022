module AdventofCode.Day5.SupplyStacks where

import Utility (aocPath)

data Item = 
    A | B | C | D | E | F
  | G | H | I | J | K | L
  | M | N | O | P | Q | R
  | S | T | U | V | W | X
  | Y | Z
  deriving Show

type Stack  = [Item]
type Stacks = [Stack]

-- "move x from y to z"
-- (Int, Int, Int) = (num, from, to)
type Line = (Int, Int, Int)

s1 = [ Z, V, T, B, J, G, R ]
s2 = [ L, V, R, J ]
s3 = [ F, Q, S ]
s4 = [ G, Q, V, F, L, N, H, Z ]
s5 = [ W, M, S, C, J, T, Q, R ]
s6 = [ F, H, C, T, W, S ]
s7 = [ J, N, F, V, C, Z, D ]
s8 = [ Q, F, R, W, D, Z, G, L ]
s9 = [ P, V, W, B, J ]

startStacks = 
  [ s1, s2, s3
  , s4, s5, s6
  , s7, s8, s9 ]

result1 :: IO String
result1 = show . getTopOfStacks . (moveAllItems startStacks) <$> inputList

result2 :: IO String
result2 = show . getTopOfStacks . (moveAllItems' startStacks) <$> inputList

solution = do
  result1 >>= putStrLn
  result2 >>= putStrLn


input :: IO String
input = readFile $ aocPath ++ "Day5/input.txt"

inputList :: IO [String]
inputList = lines <$> input

{-
            [G] [W]         [Q]    
[Z]         [Q] [M]     [J] [F]    
[V]         [V] [S] [F] [N] [R]    
[T]         [F] [C] [H] [F] [W] [P]
[B] [L]     [L] [J] [C] [V] [D] [V]
[J] [V] [F] [N] [T] [T] [C] [Z] [W]
[G] [R] [Q] [H] [Q] [W] [Z] [G] [B]
[R] [J] [S] [Z] [R] [S] [D] [L] [J]
 1   2   3   4   5   6   7   8   9 
 
-}

moveAllItems :: Stacks -> [String] -> Stacks
moveAllItems st [] = st
moveAllItems st (x:xs) = moveAllItems st' xs
  where st' = moveItems x st

moveAllItems' :: Stacks -> [String] -> Stacks
moveAllItems' st [] = st
moveAllItems' st (x:xs) = moveAllItems' st' xs
  where st' = moveMultItems (parseLine x) st

-- String represents one line of commands
moveItems :: String 
          -> Stacks 
          -> Stacks
moveItems str st = mv num (from, to) st
  where (num, from, to) = parseLine str
        mv 0 _      s   = s 
        mv n (x, y) s   = mv (n - 1) (x, y) (moveOneItem (x, y) s)  

moveMultItems :: Line -> Stacks -> Stacks
moveMultItems (x, y, z) li = st'
  where oldFrom = li !! (y - 1)
        oldTo   = li !! (z - 1)
        newFrom = drop x oldFrom
        newTo   = (take x oldFrom) ++ oldTo
        st      = replaceStack li y newFrom
        st'     = replaceStack st z newTo

-- (Int, Int) = (from, to)
moveOneItem :: (Int, Int) -> Stacks -> Stacks
moveOneItem (x, y) li = st'
  where oldFrom = li !! (x - 1)
        oldTo   = li !! (y - 1)
        newFrom = tail oldFrom
        newTo   = (head oldFrom) : oldTo
        st      = replaceStack li x newFrom
        st'     = replaceStack st y newTo

-- Int is the 'from' value; in order to
-- be zero-indexed, you need to subtract 1
replaceStack :: Stacks
             -> Int
             -> Stack
             -> Stacks
replaceStack s i n = ret
  where ret = (take (i - 1) s) ++ [n] ++ (drop i s)                 

-- String will always be in this format:
-- "move x from y to z"
-- (Int, Int, Int) = (num, from, to)
parseLine :: String -> (Int, Int, Int)
parseLine str = (x, y, z)
  where t = drop 5 str
        l = drop 6 $ dropWhile (/= ' ') t
        x = read $ takeWhile (/= ' ') t
        y = read $ takeWhile (/= ' ') l
        z = read $ reverse $ takeWhile (/= ' ') $ reverse t

getTopOfStacks :: Stacks -> Stack
getTopOfStacks [] = []
getTopOfStacks (x:xs) = 
  head x : getTopOfStacks xs