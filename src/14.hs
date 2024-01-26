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
import qualified Data.Map as Map (Map)
import qualified Data.Set as Set (Set, member, insert, fromList, empty, notMember, (\\))
import Data.Maybe (fromJust)
import Numeric (showHex)
import Data.Bits (bit, xor)

rotateCW :: Int -> [Int] -> [Int]
rotateCW n ls = take size $ drop (size - n) $ cycle ls
  where size = length ls

twist :: Int -> Int -> [Int] -> [Int]
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

knotHashTuple :: [Int] -> [Int] -> ([Int], Int)
knotHashTuple range input = Seq.foldlWithIndex (
  \(state, pointer) offset len -> (twist pointer len state, (pointer + len + offset) `rem` size)
  ) (range, 0) $ Seq.fromList input
  where size = length range

intToHex :: Int -> String
intToHex num = take (2-l) "00" ++ str
  where str = showHex num ""; l = length str

toDenseHash :: [[Int]] -> String
toDenseHash = concatMap (intToHex . foldl' xor 0)

knothash :: String -> String
-- knothash s | trace s False = undefined
knothash s = toDenseHash $ toList $ toList <$> chunked
  where
    range = [0..255]; size = length range
    input = concat $ replicate 64 $ map fromEnum s ++ [17, 31, 73, 47, 23]
    chunked = Seq.chunksOf 16 $ Seq.fromList $ fst $ knotHashTuple range input

hexToBin :: String -> String
hexToBin ('0':xs) = "0000" ++ hexToBin xs
hexToBin ('1':xs) = "0001" ++ hexToBin xs
hexToBin ('2':xs) = "0010" ++ hexToBin xs
hexToBin ('3':xs) = "0011" ++ hexToBin xs
hexToBin ('4':xs) = "0100" ++ hexToBin xs
hexToBin ('5':xs) = "0101" ++ hexToBin xs
hexToBin ('6':xs) = "0110" ++ hexToBin xs
hexToBin ('7':xs) = "0111" ++ hexToBin xs
hexToBin ('8':xs) = "1000" ++ hexToBin xs
hexToBin ('9':xs) = "1001" ++ hexToBin xs
hexToBin ('a':xs) = "1010" ++ hexToBin xs
hexToBin ('b':xs) = "1011" ++ hexToBin xs
hexToBin ('c':xs) = "1100" ++ hexToBin xs
hexToBin ('d':xs) = "1101" ++ hexToBin xs
hexToBin ('e':xs) = "1110" ++ hexToBin xs
hexToBin ('f':xs) = "1111" ++ hexToBin xs
hexToBin "" = ""

countUsed :: String -> Int
countUsed "" = 0
countUsed ('0':xs) = countUsed xs
countUsed ('1':xs) = 1 + countUsed xs

part1 :: [String] -> IO (Maybe Int)
part1 [key] = do
  return . Just $ sum $ map (countUsed . hexToBin . knothash) input
  where
    input = map (\i -> key ++ "-" ++ show i) [0..127]

type Pos = (Int, Int)

binToGrid :: [String] -> [Pos]
binToGrid rows = map snd $ filter (\(c, _) -> c == '1') grid
  where
    rowsWithIndex = toList $ Seq.mapWithIndex (,) $ Seq.fromList rows
    grid = concatMap
      (\(row, str) ->
        toList $ Seq.mapWithIndex
          (\col char -> (char, (row, col)))
          $ Seq.fromList str
      )
      rowsWithIndex

adjacent :: Pos -> [Pos]
adjacent (r, c) = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

neighbors :: [Pos] -> Set.Set Pos -> Set.Set Pos -> Set.Set Pos
neighbors [] _ visited = visited
neighbors (x:xs) grid visited = neighbors (nub $ xs ++ candidates) grid (Set.insert x visited)
    where
      candidates = filter (\p ->
        (p `Set.member` grid) && (p `Set.notMember` visited)
        ) $ adjacent x

getRemain :: [Pos] -> Set.Set Pos -> [Pos]
getRemain remainPosition visited = toList $ rem Set.\\ visited
  where rem = Set.fromList remainPosition

countGroup :: [Pos] -> Set.Set Pos -> Int
countGroup [] _ = 0
countGroup (pos:xs) refGrid = 1 + countGroup grid' refGrid
  where
    nbs = neighbors [pos] refGrid Set.empty
    grid' = getRemain xs nbs

part2 :: [String] -> IO (Maybe Int)
part2 [key] = do
  return . Just $ countGroup grid $ Set.fromList grid
  where
    input = map (\i -> key ++ "-" ++ show i) [0..127]
    rows =  map (hexToBin . knothash) input
    grid = binToGrid rows

testData :: [String]
testData = lines $ chomp [r|
flqrgnkx
|]

test :: [([String], Int)]
test = [
    (testData, 8108)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 1242)
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
