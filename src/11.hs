-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, toList, foldl', maximumBy)
import Data.Sequence (fromList, mapWithIndex, foldlWithIndex, chunksOf)
import qualified Data.Text as Text
import Data.Text as Text (Text, splitOn, pack, unpack)
import Numeric (showHex)
import Data.Bits (bit, xor)

-- reference https://www.redblobgames.com/grids/hexagons/
-- q r s
type Pos = (Int, Int, Int)

move :: Pos -> String -> Pos
move (q, r, s) "n"   = (q, r-1, s+1)
move (q, r, s) "nw"  = (q-1, r, s+1)
move (q, r, s) "ne"  = (q+1, r-1, s)
move (q, r, s) "s"   = (q, r+1, s-1)
move (q, r, s) "sw"  = (q-1, r+1, s)
move (q, r, s) "se"  = (q+1, r, s-1)

distance :: Pos -> Int
distance (q, r, s) = maximum $ map abs [q,r,s]

part1 :: [String] -> IO (Maybe Int)
part1 [s] = do
  return . Just $ distance $ foldl' move (0, 0, 0) input
  where
    input = map unpack $ splitOn "," $ pack s

moveAccumulate :: (Pos, [Pos]) -> String -> (Pos, [Pos])
moveAccumulate (pos, mem) cmd = (newPos, mem ++ [newPos])
  where newPos = move pos cmd

part2 :: [String] -> IO (Maybe Int)
part2 [s] = do
  return . Just $ maximum $ map distance res
  where
    input = map unpack $ splitOn "," $ pack s
    (_, res) = foldl' moveAccumulate ((0, 0, 0), []) input

test :: [([String], Int)]
test = [
  (["ne,ne,ne"], 3),
  (["ne,ne,sw,sw"], 0),
  (["ne,ne,s,s"], 2),
  (["se,sw,se,sw,sw"], 3)
  ]

test2 :: [([String], String)]
test2 = [
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 input expect
      -- for_ test2 $ \(input, expect) -> do
      --   assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      runSolution 2 part2 input
