module Main where

import Data.List (find)

main = do
  ns <- numbersFromFile "input"
  print (f ns)
  print (g ns)

f :: [Int] -> Int
f xs = case find (\(m,n) -> m + n == 2020) (pairs xs) of
  Just (m,n) -> m * n
  Nothing    -> error "pair that sums to 2020 not found"

g :: [Int] -> Int
g xs = case find (\(m,n,o) -> m + n + o == 2020) (triples xs) of
  Just (m,n,o) -> m * n * o
  Nothing    -> error "triple that sums to 2020 not found"

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = [(x,y) | y<-xs] ++ pairs xs

triples :: [a] -> [(a,a,a)]
triples []     = []
triples (x:xs) = [(x,y,z) | (y,z) <- pairs xs] ++ triples xs

numbersFromFile :: FilePath -> IO [Int]
numbersFromFile path = map read . lines <$> readFile path
