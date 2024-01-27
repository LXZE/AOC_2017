{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace)
import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.List (sort, find, foldl', nub, elemIndex)
import Data.Foldable (for_, toList, foldMap')
import qualified Data.Sequence as Seq (Seq, fromList, foldlWithIndex, chunksOf, mapWithIndex)
import qualified Data.Text as Txt
import Data.Text (Text, splitOn, pack, unpack)
import qualified Data.Map as Map (Map, empty, (!), member, insert)
-- import qualified Data.Set as Set (Set)
import Numeric (showBin)
import Data.Bits (bit, (.&.))
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Control.Monad (foldM)

spinCW :: [a] -> Int -> [a]
spinCW ls n = take size $ drop (size - n) $ cycle ls
  where size = length ls

swap :: String -> Int -> Int -> String
swap str i j
  -- | trace (show (front, a, mid', b, back)) False = undefined
  | i < j = front ++ [b] ++ mid' ++ [a] ++ back
  | otherwise = error "expect i < j, got" ++ show (i, j)
    where
      (mid, b:back) = splitAt j str
      (front, a:mid') = splitAt i mid

searchIndex :: String -> Char -> Int
searchIndex str ch = fromJust $ ch `elemIndex` str

operate :: String -> String -> String
operate progs ('s':args) = spinCW progs val
  where val = read args :: Int
operate progs ('x':args) = swap progs a b
  where [a,b] = sort $ map (read . unpack) $ splitOn "/" $ pack args :: [Int]
operate progs ('p':args) = swap progs a b
  where [a,b] = sort $ map (searchIndex progs . head . unpack) $ splitOn "/" $ pack args

part1 :: [String] -> IO (Maybe String)
part1 [[ls], cmdsRaw] = do
  return . Just $ foldl' operate props cmds
  where
    props = ['a'..ls]
    cmds = map unpack $ splitOn "," $ pack cmdsRaw

type History = Map.Map String Int
searchCycle :: (String, History) -> ([String], Int) -> Either (String, Int, Int) (String, History)
searchCycle (progs, hist) (cmds, step)
  -- | trace (show (progs, step)) False = undefined
  | Map.member progs hist = Left (progs, hist Map.! progs ,step)
  | otherwise = Right (progs', Map.insert progs step hist)
    where progs' = foldl' operate progs cmds

part2 :: [String] -> IO (Maybe String)
part2 [[ls], cmdsRaw] = do
  let Left (state, startCycle, endCycle) = foldM searchCycle (props, Map.empty) $ map (cmds,) [0..]
      cycleSize = endCycle - startCycle
      remainCycle = (round 1e9 - startCycle) `rem` cycleSize
  return . Just $ foldl' (foldl' operate) state $ replicate remainCycle cmds
  where
    props = ['a'..ls]
    cmds = map unpack $ splitOn "," $ pack cmdsRaw

testData :: [String]
testData = lines $ chomp [r|
s1,x3/4,pe/b
|]

test :: [([String], String)]
test = [
    (testData, "baedc")
  ]

test2 :: [([String], String)]
test2 = [
    (testData, "abcde")
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 ("e" : input) expect
      for_ test2 $ \(input, expect) -> do
        assertSolution 2 part2 ("e" : input) expect
    else do
      input <- getInput date
      runSolution 1 part1 ("p" : input)
      runSolution 2 part2 ("p" : input)
