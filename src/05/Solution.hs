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
import Data.Char (digitToInt)
import Debug.Trace (trace)
import Data.Sequence (mapWithIndex, fromList)

import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)

-- updateList :: [Int] -> Int -> Int -> [Int]
-- updateList lst targetIdx newVal = front ++ [newVal] ++ back
--   where (front, _ : back) = splitAt targetIdx lst

-- run :: [Int] -> (Int -> Int) -> Int -> Int -> Int
-- run nums fn idx step
--   -- | trace (show (idx, step)) False = undefined
--   | idx < 0 || idx >= length nums = step
--   | otherwise = run
--     (updateList nums idx $ fn $ nums !! idx)
--     fn (idx + nums !! idx) (step + 1)

runArray :: (Int -> Int) -> [Int] -> Int
runArray fn numsList = runST $ do
  let len = length numsList
  nums <- newListArray (0, len-1) numsList :: ST s (STUArray s Int Int)
  let run' idx step = if 0 > idx || idx >= len
      then return step
      else do
        val <- readArray nums idx
        writeArray nums idx (fn val)
        run' (idx + val) (step + 1)
  run' 0 0

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return . Just $ runArray (+1) num
  where num = map read s :: [Int]

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ runArray (\x -> if x >= 3 then x-1 else x+1) num
  where num = map read s :: [Int]

testData :: [String]
testData = lines $ chomp [r|
0
3
0
1
-3
|]

test :: [([String], Int)]
test = [
    (testData, 5)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 10)
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
