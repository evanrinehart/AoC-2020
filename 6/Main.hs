module Main where

import Data.List
import Data.Char

main = do
  txt <- readFile "input"
  let groupData = map lines (chunks txt)
  print (question1 groupData)
  print (question2 groupData)

question1 :: [[String]] -> Int
question1 = sum . map (length . nub . concat)

question2 :: [[String]] -> Int
question2 = sum . map (f 0 . map sort)

f :: Int -> [String] -> Int
f n strings = case minHead strings of
  Nothing -> n
  Just c  -> f 
              (n + if all (startsWith c) strings then 1 else 0)
              (map (dropC c) strings) 

startsWith x xs = [x] `isPrefixOf` xs

minHead :: [String] -> Maybe Char
minHead xss = go maxBound xss where
  go b ((c:cs):more) = go (min c b) more
  go b ([]:more)     = Nothing
  go b []            = Just b

dropC :: Char -> String -> String
dropC c []    = ""
dropC c (d:ds)
  | c == d    = ds
  | otherwise = d:ds

isLetter '\n' = False
isLetter _    = True

breakParagraph :: String -> (String,String)
breakParagraph [] = ("","")
breakParagraph ('\n':'\n':more) = ("",more)
breakParagraph (c:more) =
  let (cs,more') = breakParagraph more 
  in (c:cs, more')

chunks :: String -> [String]
chunks str = case breakParagraph str of
  ("","")   -> []
  (ch,more) -> ch : chunks more
