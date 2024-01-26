{-# LANGUAGE QuasiQuotes #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, find)
import Data.Char (digitToInt, ord)
import Data.Set (fromList, size)
import Text.Printf (printf)
import Data.List (sort)

isSame :: (Int, Int) -> Bool
isSame (a, b) = a == b

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return . Just $ sum $ map fromEnum $ zipWith (curry isSame) strSize setSize
  where
    ls = map words s
    strSize = map length ls
    setSize = map (size . fromList) ls

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ sum $ map fromEnum $ zipWith (curry isSame) strSize setSize
  where
    ls = map (map (sort . map ord) . words) s
    strSize = map length ls
    setSize = map (size . fromList) ls

test :: [([String], Int)]
test = [
    (lines $ chomp [r|
aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa
    |], 2)
  ]

test2 :: [([String], Int)]
test2 = [
    (lines $ chomp [r|
abcde fghij
abcde xyz ecdab
a ab abc abd abf abj
iiii oiii ooii oooi oooo
oiii ioii iioi iiio
    |], 3)
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
