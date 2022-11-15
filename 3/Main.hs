module Main where

import Data.List
import Data.Char

main = do
  rows@(l:ls) <- lines <$> readFile "input"
  let w = length l
  print (tally w 3 1 rows)
  let ns = map (\(r,d) -> tally w r d rows) [(1,1),(3,1),(5,1),(7,1),(1,2)]
  print (product ns)

data Ski = Ski
  { skiI       :: Int
  , skiRows    :: [String] } deriving Show

rightN :: Int -> Int -> Ski -> Ski
rightN w n (Ski i rows) =
  let i' = (i + n) `mod` w
  in Ski i' rows

downN :: Int -> Ski -> Maybe Ski
downN 0 ski = Just ski
downN n ski = case skiRows ski of
  (_:[])   -> Nothing
  (_:rows) -> downN (n-1) (ski{skiRows=rows})

path :: Int -> Int -> Int -> Ski -> [Ski]
path w r d s = case downN d (rightN w r s) of
  Nothing -> [s]
  Just s' -> s : path w r d s'

sample :: Ski -> Char
sample (Ski i (row:_)) = row !! i

tally :: Int -> Int -> Int -> [String] -> Int
tally w r d rows = f (Ski 0 rows) where
  f = length . filter (=='#') . map sample . path w r d
