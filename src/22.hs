import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace, traceShowId)
import Text.RawString.QQ (r)
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Text.Regex.TDFA.Text (Regex)
import Data.Functor ((<&>))
import System.Environment (getArgs, getProgName)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State as State

type Position = (Int, Int)
type InfectedSet = Set.Set Position
data Direction = North | East | West | South
  deriving (Show)
data Virus = Virus {
  dir :: Direction,
  pos :: Position
} deriving (Show)

grid2InfectedSet :: [String] -> InfectedSet
grid2InfectedSet grid = Set.fromList [
    (rowIdx, colIdx) |
    (row, rowIdx) <- zip grid [0..],
    (elem, colIdx) <- zip row [0..],
    elem == '#'
  ]

cw :: Direction -> Direction
cw North = East
cw East = South
cw South = West
cw West = North

ccw :: Direction -> Direction
ccw North = West
ccw West = South
ccw South = East
ccw East = North

moveForward :: Direction -> Position -> Position
moveForward North (r, c) = (r-1, c)
moveForward South (r, c) = (r+1, c)
moveForward West (r, c) = (r, c-1)
moveForward East (r, c) = (r, c+1)

stateFn :: Virus -> Int -> Int -> State.State (InfectedSet, Int) Int
stateFn _ current limit | current == limit = State.get <&> snd
stateFn (Virus { dir, pos }) current limit = do
    (infectedList, burstCount) <- State.get
    let isInfected = pos `Set.member` infectedList
    let (newDir, newInfectedList) = if isInfected
        then (cw dir, Set.delete pos infectedList)
        else (ccw dir, Set.insert pos infectedList)
    let newPos = moveForward newDir pos

    State.put (newInfectedList, burstCount + if not isInfected then 1 else 0)
    stateFn (Virus { dir=newDir, pos=newPos }) (current+1) limit

part1 :: [String] -> IO (Maybe Int)
part1 grid = do
  let infectedList = grid2InfectedSet grid
  let res = State.evalState (stateFn (Virus { dir=North, pos=center }) 0 10000) (infectedList, 0)
  return $ Just res
  where
    size = length grid
    mid = (size - 1) `div` 2
    center = (mid, mid)

data NodeState = Weakened | Infected | Flagged deriving (Show)
type InfectedMap = Map.Map Position NodeState

grid2InfectedMap :: [String] -> InfectedMap
grid2InfectedMap grid = Map.fromList [
    ((rowIdx, colIdx), Infected) |
    (row, rowIdx) <- zip grid [0..],
    (elem, colIdx) <- zip row [0..],
    elem == '#'
  ]

stateFn' :: Virus -> Int -> Int -> State.State (InfectedMap, Int) Int
stateFn' _ current limit | current == limit = State.get <&> snd
stateFn' (Virus { dir, pos }) current limit = do
    (infectedMap, burstCount) <- State.get
    let currentPosStatus = Map.lookup pos infectedMap
    let (newDir, newMap, incBurst) =
          case currentPosStatus of
            Nothing -> (ccw dir, Map.insert pos Weakened infectedMap, 0)
            Just Weakened -> (dir, Map.alter (const $ Just Infected) pos infectedMap, 1)
            Just Infected -> (cw dir, Map.alter (const $ Just Flagged) pos infectedMap, 0)
            Just Flagged -> ((cw .cw) dir, Map.delete pos infectedMap, 0)
    let newPos = moveForward newDir pos

    State.put (newMap, burstCount + incBurst)
    stateFn' (Virus { dir=newDir, pos=newPos }) (current+1) limit

part2 :: [String] -> IO (Maybe Int)
part2 grid = do
  let infectedMap = grid2InfectedMap grid
  let res = State.evalState (stateFn' (Virus { dir=North, pos=center }) 0 10_000_000) (infectedMap, 0)
  return $ Just res
  where
    size = length grid
    mid = (size - 1) `div` 2
    center = (mid, mid)

testData :: [String]
testData = lines $ chomp [r|
..#
#..
...
|]

test :: [([String], Int)]
test = [
    (testData, 5587)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 2511944)
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      Fold.for_ test $ \(input, expect) -> do
        assertSolution 1 part1 input expect
      Fold.for_ test2 $ \(input, expect) -> do
        assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      runSolution 2 part2 input
