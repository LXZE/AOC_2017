{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace as DBG
import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, Foldable (toList), maximumBy)
import Data.Sequence (fromList, mapWithIndex)
import Data.List (findIndex, elemIndex, mapAccumL)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text as Text (Text, splitOn, pack, unpack)
import Data.Char (isDigit)
import Data.Function (on)

-- str, level, isGarbage
score :: Int -> Bool -> String -> Int
score lvl True ('!':_:xs) = score lvl True xs
score lvl True ('>':xs) = score lvl False xs
score lvl True (_:xs) = score lvl True xs
score lvl False ('<':xs) = score lvl True xs
score lvl False ('{':xs) = score (lvl + 1) False xs
score lvl False ('}':xs) = lvl + score (lvl - 1) False xs
score lvl False (',':xs) = score lvl False xs
score _ _ "" = 0
score _ _ remain = error $ "unexpected end: " ++ remain

part1 :: [String] -> IO (Maybe Int)
part1 [s] = do
  return . Just $ score 0 False s

-- str, level, isGarbage
score' :: Bool -> String -> Int
score' True ('!':_:xs) = score' True xs
score' True ('>':xs) = score' False xs
score' True (_:xs) = 1 + score' True xs
score' False ('<':xs) = score' True xs
score' False (_:xs) = score' False xs
score' _ "" = 0

part2 :: [String] -> IO (Maybe Int)
part2 [s] = do
  return . Just $ score' False s

test :: [([String], Int)]
test = [
  (["{}"], 1),
  (["{{{}}}"], 6),
  (["{{},{}}"], 5),
  (["{{{},{},{{}}}}"], 16),
  (["{<a>,<a>,<a>,<a>}"], 1),
  (["{{<ab>},{<ab>},{<ab>},{<ab>}}"], 9),
  (["{{<!!>},{<!!>},{<!!>},{<!!>}}"], 9),
  (["{{<a!>},{<a!>},{<a!>},{<ab>}}"], 3)
  ]

test2 :: [([String], Int)]
test2 = [
  (["<>"], 0),
  (["<random characters>"], 17),
  (["<<<<>"], 3),
  (["<{!>}>"], 2),
  (["<!!>"], 0),
  (["<!!!>>"], 0),
  (["<{o\"i!a,<{i<a>"], 10)
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
