{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace)
import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.List (sort, find)
import Data.Foldable (for_, toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Txt
import Data.Text (Text, splitOn, pack, unpack)
import qualified Data.Map as Map (Map)
import qualified Data.Set as Set (Set)
import Data.Maybe (fromJust)

data Dir = Up | Down
  deriving (Show, Eq)
type Scanner = (Int, Int, Dir) -- length, current position, direction

getScanner :: Int -> Scanner
getScanner len = (len, 0, Down)

moveScanner :: Scanner -> Scanner
moveScanner (len, pos, dir)
  | pos == 0 && dir == Up = (len, 1, Down)
  | pos == (len - 1) && dir == Down = (len, pos - 1, Up)
  | dir == Up = (len, pos - 1, dir)
  | dir == Down = (len, pos + 1, dir)

getPosition :: Scanner -> Int -> Int -> Int
getPosition sc tgt step
  | tgt == step = pos
  | otherwise = getPosition (moveScanner sc) tgt $ step + 1
  where (len, pos, dir) = sc

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return . Just $ sum $ map (\[a,b] -> a*b) caughtKey
  where
    s' = map (splitOn ": " . pack) s
    input = map (map (read . unpack)) s' :: [[Int]]
    caughtKey = filter (\[key, len] -> getPosition (getScanner len) key 0 == 0) input

feasible :: [[Int]] -> Int -> Bool
-- feasible _ delay | trace (show delay) False = undefined
feasible scanCmd delay = null caughtKey
  where
    caughtKey = filter
      (\[key, len] ->
        getPosition (getScanner len) (key+delay `rem` ((len - 1) * 2)) 0 == 0
        )
      scanCmd

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ fromJust $ find (feasible input) searchSpace
  where
    s' = map (splitOn ": " . pack) s
    input = map (map (read . unpack)) s' :: [[Int]]
    searchSpace = [1..]

testData :: [String]
testData = lines $ chomp [r|
0: 3
1: 2
4: 4
6: 4
|]

test :: [([String], Int)]
test = [
    (testData, 24)
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
