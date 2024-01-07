{-# LANGUAGE QuasiQuotes #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ ( r )
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_)
import Data.Char (digitToInt)

maxSubMin :: [Int] -> Int
maxSubMin xs = maximum xs - minimum xs

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  let rows = map (map read . words) s :: [[Int]]
  return $ Just $ sum $ map maxSubMin rows

divided :: [Int] -> Int
divided xs = case filter ((== 0) . snd) [i `quotRem` j | i <- xs, j <- xs, i /= j] of
  [(q, _)] -> q
  _ -> error "no solution"

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  let rows = map (map read . words) s :: [[Int]]
  return $ Just $ sum $ map divided rows

test :: [([String], Int)]
test = [
    ([[r|
5 1 9 5
7 5 3
2 4 6 8
    |]], 18)
  ]

test2 :: [([String], Int)]
test2 = [
    ([[r|
5 9 2 8
9 4 7 3
3 8 6 5
    |]], 9)
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 (lines . head $ map chomp input) expect
      for_ test2 $ \(input, expect) -> do
        assertSolution 2 part2 (lines . head $ map chomp input) expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      runSolution 2 part2 input
