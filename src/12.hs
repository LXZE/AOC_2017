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
import Data.List (sort)
import Data.Foldable (for_, toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Txt
import Data.Text (Text, splitOn, pack, unpack)
import qualified Data.Map as Map (Map, fromList, (!), member, keys)
import qualified Data.Set as Set (
  Set, fromList, member, insert, notMember, singleton, empty, size, (\\), union
  )

search :: Map.Map Int [Int] -> Set.Set Int -> [Int] -> Set.Set Int
search dict visited [] = visited
search dict visited (x:xs)
  | x `Set.member` visited = search dict visited xs
  | otherwise = search dict (Set.insert x visited) (
    xs ++ filter (`Set.notMember` visited) (dict Map.! x)
  )

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return . Just $ Set.size $ search dict Set.empty [0]
  where
    s' = map (splitOn " <-> " . pack) s
    input = map (\[keyRaw, valRaw] -> (
      read . unpack $ keyRaw :: Int,
      map (read . unpack) $ splitOn ", " valRaw :: [Int]
      )) s'
    dict = Map.fromList input

findRemain :: Set.Set Int -> [Int] -> [Int]
findRemain set list = sort $ toList $ Set.fromList list Set.\\ set

countGroup :: Map.Map Int [Int] -> Set.Set Int -> [Int] -> Int
countGroup dict visited [] = 0
countGroup dict visited (x:xs) = 1 + countGroup dict
  (Set.union result visited)
  (findRemain (Set.union result visited) xs)
  where
    result = search dict Set.empty [x]

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ countGroup dict Set.empty $ Map.keys dict
  where
    s' = map (splitOn " <-> " . pack) s
    input = map (\[keyRaw, valRaw] -> (
      read . unpack $ keyRaw :: Int,
      map (read . unpack) $ splitOn ", " valRaw :: [Int]
      )) s'
    dict = Map.fromList input

testData :: [String]
testData = lines $ chomp [r|
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
|]

test :: [([String], Int)]
test = [
    (testData, 6)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 2)
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
