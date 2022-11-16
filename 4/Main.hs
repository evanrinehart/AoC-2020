#!cabal
{- cabal:
build-depends: base, mtl, containers
-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative
import Data.Char
import Control.Monad.State
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

data Pass = Pass
  { byr :: String
  , iyr :: String
  , eyr :: String
  , hgt :: String
  , hcl :: String
  , ecl :: String
  , pid :: String
  , cid :: Maybe String }
      deriving Show

validate :: Map String String -> Maybe Pass
validate m =
  Pass <$>
  req "byr" m <*>
  req "iyr" m <*>
  req "eyr" m <*>
  req "hgt" m <*>
  req "hcl" m <*>
  req "ecl" m <*>
  req "pid" m <*>
  opt "cid" m

main = do
  raw <- readFile "input"
  let (_, outs) = execState bakeLoop (raw, [M.empty])
  let ps = catMaybes (map validate outs)
  print (length ps)
  let veryValidPS = catMaybes (map veryValid ps)
  print (length veryValidPS)

-- Input reader

type A o a = State ([Char],[o]) a

bakeLoop :: A (Map String String) ()
bakeLoop = do
  mtok <- consume token
  case mtok of
    Field p    -> adjustOutput (M.union p) >> bakeLoop
    Separator  -> newOutput >> bakeLoop
    EndOfInput -> return ()

req :: String -> Map String String -> Maybe String
req k m = M.lookup k m

opt :: String -> Map String String -> Maybe (Maybe String)
opt k m = Just (M.lookup k m)

isWordChar c = not (isSpace c || c == ':')

dropSpace ('\n':'\n':more) = '\n':'\n':more
dropSpace ('\n':more) = dropSpace more
dropSpace (' ':more)  = dropSpace more
dropSpace other = other

data Token =
  Field (Map String String) |
  Separator |
  EndOfInput

token :: String -> (Token, String)
token str = case dropSpace str of
  []             -> (EndOfInput, [])
  '\n':'\n':more -> (Separator, more)
  other          -> flip runState other $ do
    w1 <- stuff
    colon
    w2 <- stuff
    return (Field (M.singleton w1 w2))

colon :: State String ()
colon = modify $ \(':':more) -> more

stuff :: State String String
stuff = state (span isWordChar)

consume :: (String -> (a, String)) -> A o a
consume f = state $ \(is,os) -> let (x,is') = f is in (x, (is',os))

newOutput :: Monoid o => A o ()
newOutput = modify $ \(is,os) -> (is,mempty:os)

adjustOutput :: (o -> o) -> A o ()
adjustOutput f = modify $ \(is,o:os) -> (is,f o:os)




-- Additional Validation

validByr :: String -> Maybe ()
validByr str = fourDigits str >>= between 1920 2002

validIyr :: String -> Maybe ()
validIyr str = fourDigits str >>= between 2010 2020

validEyr :: String -> Maybe ()
validEyr str = fourDigits str >>= between 2020 2030

validHgt :: String -> Maybe ()
validHgt str = f =<< someMeasurement str where
  f (n, "cm") = between 150 193 n
  f (n, "in") = between 59 76 n
  f _         = Nothing

validHcl :: String -> Maybe ()
validHcl ('#':str) = sixHex str
validHcl _         = Nothing

validEcl :: String -> Maybe ()
validEcl = f where
  f "amb" = pure ()
  f "blu" = pure ()
  f "brn" = pure ()
  f "gry" = pure ()
  f "grn" = pure ()
  f "hzl" = pure ()
  f "oth" = pure ()
  f _     = Nothing

validPid :: String -> Maybe ()
validPid str
  | length str == 9 && all isDigit str = Just ()
  | otherwise                          = Nothing

veryValid :: Pass -> Maybe ()
veryValid Pass{byr,iyr,eyr,hgt,hcl,ecl,pid,cid} = do
  validByr byr
  validIyr iyr
  validEyr eyr
  validHgt hgt
  validHcl hcl
  validEcl ecl
  validPid pid
  pure ()

someMeasurement :: String -> Maybe (Int,String)
someMeasurement str = case span isDigit str of
  ([], _)    -> Nothing
  (ds, rest) -> Just (read ds, rest)

between :: Int -> Int -> Int -> Maybe ()
between a b n
  | a <= n && n <= b = Just ()
  | otherwise        = Nothing

fourDigits :: String -> Maybe Int
fourDigits ds@[d1,d2,d3,d4]
  | all isDigit ds = Just (read ds)
  | otherwise      = Nothing
fourDigits _ = Nothing

sixHex :: String -> Maybe ()
sixHex hs@[h1,h2,h3,h4,h5,h6]
  | all isHex hs = Just ()
  | otherwise    = Nothing
sixHex _ = Nothing

isHex :: Char -> Bool
isHex c = if isHexDigit c
  then isDigit c || isLower c
  else False

