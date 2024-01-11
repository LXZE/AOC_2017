{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import Data.List (findIndex, elemIndex, map)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map, member)
import qualified Data.Map as Map
import Debug.Trace as T

maxAt :: [Int] -> Int
maxAt nums = fromJust $ elemIndex maxNum nums
  where maxNum = maximum nums

redis :: [Int] -> Int -> [Int]
redis nums idx = toList . mapWithIndex (\i v ->
    (if i == idx then 0 else v) + -- n[idx] become start with 0 else remain
    q + -- val spread to all num equally
    (if (i - (idx + 1)) `mod` len < r then 1 else 0) -- add 1 if idx position less than remain val
  ) $ fromList nums
  where len = length nums; val = nums !! idx; (q, r) = val `quotRem` len

run :: [Int] -> Int -> Map [Int] Int -> (Int, Int)
run nums step visited
  -- | T.trace (show (nums, step, visited)) False = undefined
  | member nums visited = (step, step - fromJust (Map.lookup nums visited))
  | otherwise = run nums' (step + 1) $ Map.insert nums step visited
  where
    len = length nums
    targetIdx = maxAt nums
    val = nums !! targetIdx
    nums' = redis nums targetIdx

part1 :: [String] -> IO (Maybe Int)
part1 [s] = do
  return . Just $ fst $ run num 0 Map.empty
  where num = map read $ words s :: [Int]

part2 :: [String] -> IO (Maybe Int)
part2 [s] = do
  return . Just $ snd $ run num 0 Map.empty
  where num = map read $ words s :: [Int]

testData :: [String]
testData = lines $ chomp [r|0 2 7 0|]

test :: [([String], Int)]
test = [
    (testData, 5)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 4)
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
