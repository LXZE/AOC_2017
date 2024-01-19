-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, toList, foldl')
import Data.Sequence (fromList, mapWithIndex, foldlWithIndex, chunksOf)
import qualified Data.Text as Text
import Data.Text as Text (Text, splitOn, pack, unpack)
import Numeric (showHex)
import Data.Bits (bit, xor)

rotateCW :: Int -> [Int] -> [Int]
rotateCW 0 ls = ls
rotateCW n ls = rotateCW (n-1) $ last ls : init ls

twist :: Int -> Int -> [Int] -> [Int]
-- twist idx len range | trace (show (idx, len, range)) False = undefined
twist _ len range | len <= 0 = range
twist idx len range = rotateCW tailSize $ front ++ twistedPart ++ back
  where
    rangeSize = length range
    isRolled = idx + len > rangeSize
    tailSize = if isRolled then (idx + len) `rem` rangeSize else 0
    rolledRange = take rangeSize $ drop tailSize $ cycle range
    newIdx = idx - tailSize
    (front, range') = splitAt newIdx rolledRange
    (range'', back) = splitAt len range'
    twistedPart = reverse range''

knotHash :: [Int] -> [Int] -> ([Int], Int)
knotHash range input = foldlWithIndex (
  \(state, pointer) offset len -> (twist pointer len state, (pointer + len + offset) `rem` size)
  ) (range, 0) $ fromList input
  where size = length range

part1 :: [String] -> IO (Maybe Int)
part1 [m, s] = do
  let (a:b:_) = fst $ knotHash range input
  return . Just $ a*b
  where
    range = [0..(read m :: Int)]
    input = map (read . unpack) $ splitOn (pack ",") (pack s) :: [Int]

intToHex :: Int -> String
intToHex num = take (2-l) "00" ++ str
  where str = showHex num ""; l = length str

toDenseHash :: [[Int]] -> String
toDenseHash = concatMap (intToHex . foldl' xor 0)

part2 :: [String] -> IO (Maybe String)
part2 [s] = do
  let chunked = chunksOf 16 $ fromList $ fst $ knotHash range input
      normal = toList $ fmap toList chunked
  return . Just $ toDenseHash normal
  where
    range = [0..255]; size = length range
    input = concat $ replicate 64 $ map fromEnum s ++ additionalTail
additionalTail :: [Int]
additionalTail = [17, 31, 73, 47, 23]

test :: [([String], Int)]
test = [
  (["3,4,1,5"], 12)
  ]

test2 :: [([String], String)]
test2 = [
  ([""], "a2582a3a0e66e6e86e3812dcb672a272"),
  (["AoC 2017"], "33efeb34ea91902bb2f59c9920caa6cd"),
  (["1,2,3"], "3efbe78a8d82f29979031a4aa0b16a9d"),
  (["1,2,4"], "63960835bcdc130f0b66d7ff4f6a5a8e")
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 ("4" : input) expect
      for_ test2 $ \(input, expect) -> do
        assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 $ "255" : input
      runSolution 2 part2 input
