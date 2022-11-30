#!cabal
{- cabal:
build-depends: base, containers
-}
module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List

data Ins =
  NOP Int |
  ACC Int |
  JMP Int
    deriving (Eq,Ord,Show,Read)

parseLine :: String -> Ins
parseLine (c1:c2:c3:' ':s:more) = f [c1,c2,c3] (g s (h more)) where
  f "nop" = NOP
  f "acc" = ACC
  f "jmp" = JMP
  g '+' = id
  g '-' = negate
  h str = case reads str of
    [(i,_)] -> i
    _       -> error "parse int failed"

loadFile :: FilePath -> IO [Ins]
loadFile path = do
  ls <- fmap lines (readFile path)
  return (map parseLine ls)

data VideoGame = VideoGame
  { vgProg  :: [Ins]
  , vgAccum :: Int
  , vgPC    :: Int
  , vgSeen  :: IntSet }
    deriving Show

-- if this is the last instruction, return the final accumulator value
close :: VideoGame -> Maybe Int
close VideoGame{vgProg=[p],vgAccum=acc} = case p of
  NOP _ -> Just acc
  ACC d -> Just (acc + d)
  JMP d -> Nothing
close VideoGame{vgProg=[]} = error "bad game state"
close _ = Nothing

jumpTo :: Int -> [Ins] -> Maybe [Ins]
jumpTo 0 (x:xs) = Just (x:xs)
jumpTo 0 []     = Nothing
jumpTo n (x:xs) = jumpTo (n-1) xs
jumpTo n []     = Nothing

step :: [Ins] -> VideoGame -> VideoGame
step program (VideoGame (p:ps) acc i seen) = case p of
  NOP _ -> VideoGame ps acc     (i+1) (IS.insert i seen)
  ACC d -> VideoGame ps (acc+d) (i+1) (IS.insert i seen)
  JMP d -> let i' = i + d in
           case jumpTo i' program of
             Just ps' -> VideoGame ps' acc i' (IS.insert i seen)
             Nothing  -> error ("jumped out (" ++ show acc ++ ")") -- good enough

isOnSecondPass :: VideoGame -> Bool
isOnSecondPass VideoGame{vgPC=i,vgSeen=seen} = i `IS.member` seen

question1 :: [VideoGame] -> Int
question1 hist =
  case find isOnSecondPass hist of
    Just (VideoGame{vgAccum=acc}) -> acc
    Nothing                       -> error "no duplicate found"

isLooping :: [Ins] -> VideoGame -> Bool
isLooping program vg = case close vg of
  Just _  -> False
  Nothing -> isOnSecondPass vg || isLooping program (step program vg)

changeIns :: Ins -> Ins
changeIns (NOP d) = JMP d
changeIns (JMP d) = NOP d
changeIns (ACC d) = ACC d

onDiagonal :: (a -> a) -> [a] -> [[a]]
onDiagonal f []     = []
onDiagonal f (x:xs) = (f x : xs) : map (x:) (onDiagonal f xs)

final :: [Ins] -> VideoGame -> Int
final program vg = case close vg of
  Nothing -> final program (step program vg)
  Just i  -> i

-- program alternates with each instruction changed
alternatesOf = onDiagonal changeIns

main = do
  program <- loadFile "input"
  putStrLn "broken program loaded"
  let hist = iterate (step program) (VideoGame program 0 0 IS.empty)
  putStrLn "history 1 set up"
  print (question1 hist)
  putStrLn "question 1 done"
  let alts = alternatesOf program
  putStrLn "alternates set up"
  let Just fixed = find (\p -> not (isLooping p (VideoGame p 0 0 IS.empty))) alts
  putStrLn "fixed program found"
  mapM_ print fixed
  print (final fixed (VideoGame fixed 0 0 IS.empty))
  putStrLn "question 2 done"
