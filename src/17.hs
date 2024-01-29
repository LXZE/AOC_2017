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

spinCCW :: [a] -> Int -> [a]
spinCCW ls n = take (length ls) $ drop n $ cycle ls

addUntil :: [Int] -> Int -> [Int]
addUntil ls step
  -- | trace (show ls) False = undefined
  | size == 1 = addUntil [0, 1] step
  | size < 2018 = addUntil (spinCCW ls step' ++ [size]) step
  | otherwise = ls
    where
      size = length ls
      step' = step `rem` size

part1 :: [String] -> IO (Maybe Int)
part1 [s] = do
  return . Just $ head $ addUntil [0] input
  where
    input = read s :: Int

-- next_idx = idx + step `mod` size + 1
findNext :: Int -> (Int, Int) -> Int -> (Int, Int)
findNext step (idx, prev) size = (res, new)
  where
    res = 1 + (idx + step) `mod` size
    new = if res == 1 then size else prev

part2 :: [String] -> IO (Maybe Int)
part2 [s] = do
  return . Just $ snd $ foldl' (findNext input) (1, 1) range
  where
    input = read s :: Int
    range = [2 .. (round 5e7)]

testData :: [String]
testData = lines $ chomp [r|
3
|]

test :: [([String], Int)]
test = [
    (testData, 638)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 1222153)
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 input expect
      for_ test2 $ \(input, expect) -> do
        assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      runSolution 2 part2 input
