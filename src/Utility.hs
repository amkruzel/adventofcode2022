module Utility where

day :: Int -> String
day n = makeBold $ day' n

incompleteDay :: Int -> String
incompleteDay n = makeBold $ day'' n

day' :: Int -> String
day' n = "\n~~~~~~~~~~\n\
         \✅ Day " ++ ns ++ "\n\
         \~~~~~~~~~~\n"
  where ns = show n

day'' :: Int -> String
day'' n = "\n~~~~~~~~~~\n\
         \❌ Day " ++ ns ++ "\n\
         \~~~~~~~~~~\n"
  where ns = show n

makeBold :: String -> String
makeBold str = "\x1b[1m" ++ str ++ "\x1b[0m"

aocPath :: FilePath
aocPath = 
  "/Users/alexanderkruzel/Documents\
  \/repos_personal/haskell/adventofcode2022\
  \/src/AdventofCode/"