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
import Data.List (sort, find, foldl', nub)
import Data.Foldable (for_, toList, foldMap')
import qualified Data.Sequence as Seq (Seq, fromList, foldlWithIndex, chunksOf, mapWithIndex)
import qualified Data.Text as Txt
import Data.Text (Text, splitOn, pack, unpack)
-- import qualified Data.Map as Map (Map)
-- import qualified Data.Set as Set (Set)
import Numeric (showBin)
import Data.Bits (bit, (.&.))
import Data.Char (isDigit)

type Generator = (Int, Int)

findNext :: Int -> Int -> Int
findNext prev factor = prev*factor `rem` lim
  where lim = 2^31 - 1

yield :: Generator -> Generator
yield (prev, factor) = (findNext prev factor, factor)

mask :: Int
mask = (2^16) - 1

isMatch :: (Generator, Generator, Int) -> (Generator, Generator, Int)
isMatch (a, b, count) = (a', b', if a'' == b'' then count + 1 else count)
  where
    [a', b'] = map yield [a,b]
    [a'', b''] = map ((mask .&.) . fst) [a', b']

part1 :: [String] -> IO (Maybe Int)
part1 str = do
  let (_,_,res) = foldl' (\a _ -> isMatch a) ((a, aF), (b, bF), 0) range
  return . Just $ res
  where
    parse s = read $ dropWhile (not . isDigit) s :: Int
    [a, b] = map parse str
    [aF, bF] = [16807, 48271]
    range = [1..4e7] -- 40 million

type Generator2 = (Int, Int, Int)

yieldOnce :: Generator2 -> Generator2
yieldOnce (prev, factor, mul) = (findNext prev factor, factor, mul)

yieldUntil :: Generator2 -> Generator2
yieldUntil gen
  | prev `rem` mul == 0 = gen
  | otherwise = yieldUntil (findNext prev factor, factor, mul)
  where (prev, factor, mul) = gen

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

isMatch' :: (Generator2, Generator2, Int) -> (Generator2, Generator2, Int)
-- isMatch' (a, b, c) | trace (show (map fst3 [a, b])) False = undefined
isMatch' (a, b, count) = (a', b', if a'' == b'' then count + 1 else count)
  where
    [a', b'] = map (yieldUntil . yieldOnce) [a,b]
    [a'', b''] = map ((mask .&.) . fst3) [a', b']

part2 :: [String] -> IO (Maybe Int)
part2 str = do
  let (_,_,res) = foldl' (\a _ -> isMatch' a) ((a, aF, 4), (b, bF, 8), 0) range
  return . Just $ res
  where
    parse s = read $ dropWhile (not . isDigit) s :: Int
    [a, b] = map parse str
    [aF, bF] = [16807, 48271]
    range = [1..5e6] -- 5 million

testData :: [String]
testData = lines $ chomp [r|
Generator A starts with 65
Generator B starts with 8921
|]

test :: [([String], Int)]
test = [
    (testData, 588)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 309)
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
