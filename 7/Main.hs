#!cabal
{- cabal:
build-depends: base, containers, megaparsec
-}
module Main where

import Data.List
import Data.Char
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Parser = Parsec Void String

type Color = (String,String)

-- for any bag color, how many of what colors must it contain
type RulesNumColor = Map Color [(Int,Color)]

-- for any bag color, what colors must it contain (ignoring counts)
type RulesColor = Map Color [Color]

-- for any bag color, what bags may contain it
type ReverseRules = Map Color [Color]

main = do
  rulesNC <- fmap (M.unions . catMaybes . map (parseMaybe rule) . lines) (readFile "input")
  let rulesC = forgetNumbers rulesNC
  let reverseRules = reverseIndex rulesC
  print (question1 reverseRules ("shiny","gold"))
  print (question2 rulesNC      ("shiny","gold"))

forgetNumbers :: RulesNumColor -> RulesColor
forgetNumbers = M.map (map (\(n,color) -> color))

reverseIndex :: RulesColor -> ReverseRules
reverseIndex index2 =
  let visit s k cs = s `S.union` S.fromList (map (\c -> (c,k)) cs)
      links = M.foldlWithKey visit S.empty index2
      single (k,c) = M.singleton k [c]
  in M.unionsWith (<>) (map single (S.toList links))

question1 :: ReverseRules -> Color -> Int
question1 index3 = length . nub . visit index3 . (index3 !)

question2 :: RulesNumColor -> Color -> Int
question2 index1 start = totalSize index1 start

totalSize :: RulesNumColor -> Color -> Int
totalSize index1 color = sum (map f (index1 ! color)) where
  f (n, subcolor) = n + n * totalSize index1 subcolor

    



visit :: Map Color [Color] -> [Color] -> [Color]
visit index cs = do
  c <- cs
  c : visit index (M.findWithDefault [] c index)

color :: Parser Color
color = do
  w1 <- some letterChar
  space
  w2 <- some letterChar
  space
  return (w1,w2)

datum :: Parser (Int,Color)
datum = do
  n <- read <$> some digitChar
  space
  c <- color
  space
  string "bag" >> optional (char 's')
  space
  return (n,c)

rule :: Parser (Map Color [(Int,Color)])
rule = do
  c <- color
  string "bags contain"
  space
  cs <- (string "no other bags" >> return []) <|>
            datum `sepBy1` (char ',' >> space)
  char '.'
  return (M.singleton c cs)
  
  
