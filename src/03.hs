-- {-# LANGUAGE QuasiQuotes #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, find)
import Data.Char (digitToInt)
import Data.List (findIndex, unfoldr, mapAccumL)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

odds :: [Int]
odds = filter odd [1..]
odds2 :: [Int]
odds2 = map (^ 2) odds

distance :: Int -> Int
distance n = fromMaybe 0 $ findIndex (n <=) odds2

diffFromMid :: Int -> Int -> Int
diffFromMid 1 _ = 0
diffFromMid num circle = minimum $ map (\n -> abs $ n - num) mids
  where
    steps = 2 * circle -- 2,4,6...
    startMid = odds2 !! (circle - 1) + circle
    mids = [startMid + (x * steps) | x <- [0..3]]

part1 :: [String] -> IO (Maybe Int)
part1 [s] = do
  return . Just $ circle + diffFromMid num circle
  where num = read s :: Int; circle = distance num

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int)
-- (0,  1) == go right (-1, 0) == go north
-- (0, -1) == go left  ( 1, 0) == go south
type Step = (Int, Int, Dir, Pos)

spiral :: [Pos]
spiral = unfoldr (Just . step) (0, 0, Dir (0, 1), (0, 0))

step :: Step -> (Pos, Step)
step (origin, len, Dir (dr, dc), (r, c)) = (pos', (origin', len', dir', pos'))
  where
    pos' = (r + dr, c + dc)
    (origin', len', dir') = if len == 0
      then
        -- increase orig size every time axis changed from vertical to horizon
        let o = origin + if dr == 0 then 0 else 1
        in (o, o, Dir (-dc, dr))
      else
        (origin, len-1, Dir (dr, dc))

updateMap :: Map Pos Int -> Pos -> (Map Pos Int, Int)
updateMap map pos = (Map.insert pos newVal map, newVal)
  where newVal = sum (mapMaybe (`Map.lookup` map) (adjacent pos))

adjacent :: Pos -> [Pos]
adjacent (r, c) = [(r + dr, c + dc) | dr <- [-1, 0, 1], dc <- [-1, 0, 1], (dr, dc) /= (0, 0)]

part2 :: [String] -> IO (Maybe Int)
part2 [s] = do
  return . find (>= num) $ snd $ mapAccumL updateMap map spiral
  where num = read s :: Int; map = Map.singleton (0, 0) 1

test :: [([String], Int)]
test = [
    (["1"], 0),
    (["2"], 1),
    (["3"], 2),
    (["4"], 1),
    (["7"], 2),
    (["12"], 3),
    (["23"], 2),
    (["34"], 3),
    (["1024"], 31)
  ]

test2 :: [([String], Int)]
test2 = [
    (["362"], 362),
    (["747"], 747),
    (["806"], 806)
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
