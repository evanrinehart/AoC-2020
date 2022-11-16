module Main where

import Data.List

-- 7 x F|B
decodeRow :: String -> Int
decodeRow cs = go 0 cs where
  go i [] = i
  go i ('F':more) = go (2*i) more
  go i ('B':more) = go (2*i + 1) more

-- 3 x L|R
decodeColumn :: String -> Int
decodeColumn cs = go 0 cs where
  go i [] = i
  go i ('L':more) = go (2*i) more
  go i ('R':more) = go (2*i + 1) more

decodeSeat :: String -> Int
decodeSeat str =
  let (a,b) = splitAt 7 str
      row = decodeRow a
      col = decodeColumn b
  in row * 8 + col

seatsFromFile :: FilePath -> IO [Int]
seatsFromFile path = map decodeSeat . lines <$> readFile path

main = do
  seats <- seatsFromFile "input"
  print (maximum seats)
  print (sort seats)
