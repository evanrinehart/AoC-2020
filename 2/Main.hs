module Main where

import Data.List
import Data.Char

main = do
  ps <- map passwordLine . lines <$> readFile "input"
  let n = length (filter validLine1 ps)
  print n
  let m = length (filter validLine2 ps)
  print m

validLine1 :: Passdata -> Bool
validLine1 (Passdata a b c str) =
  let n = length (filter (==c) str)
  in a <= n && n <= b

validLine2 :: Passdata -> Bool
validLine2 (Passdata i j c str) =
  let x = str !! (i - 1)
      y = str !! (j - 1) in
  if x==c && y==c then False else
  if x==c then True else
  if y==c then True else
  False

data Passdata = Passdata
  { param1   :: Int
  , param2   :: Int
  , paramC   :: Char
  , passData :: String } deriving Show

passwordLine :: String -> Passdata
passwordLine = parse pass

pass :: Ps Passdata
pass =
  Passdata <$>
  (num <* skip1) <*>
  (num <* skip1) <*>
  (char <* skip 2) <*>
  remainder

newtype Ps a = Ps (String -> (a, String))

num :: Ps Int 
num = Ps f where
  f str = let (ds,rest) = span isDigit str in (read ds, rest)

skip1 :: Ps ()
skip1 = Ps (\(s:ss) -> ((), ss))

skip :: Int -> Ps ()
skip 0 = pure ()
skip n = skip1 *> skip (n-1)

char :: Ps Char
char = Ps (\(s:ss) -> (s, ss))

remainder :: Ps String
remainder = Ps (\str -> (str, ""))

parse :: Ps a -> String -> a
parse (Ps f) raw = let (x,_) = f raw in x

instance Functor Ps where
  fmap g (Ps f) = Ps (\str -> let (x,rest) = f str in (g x, rest))

instance Applicative Ps where
  pure x = Ps (\str -> (x,str))
  Ps f <*> Ps g = Ps h where
    h str = let (fun,rest)  = f str
                (arg,rest') = g rest
            in (fun arg,rest')
