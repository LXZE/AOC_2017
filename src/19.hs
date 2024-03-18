import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace, traceShowId)
import Text.RawString.QQ (r)
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

data Dir = Up | Down | Left | Right
type Pos = (Int, Int)
data World = World {
  map :: Map.Map Pos Char,
  maxRow :: Int,
  maxCol :: Int
}

getAdjacent :: Pos -> [Pos]
getAdjacent (row, col) = [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

existInWorld :: Pos -> World -> Bool
existInWorld pos (World { map }) = Map.member pos map

findNextPos :: World -> Pos -> Pos-> Maybe Pos
findNextPos world prevPos pos = case filter filterFn $ getAdjacent pos of
  x:[] -> Just x
  other -> trace (show (prevPos, pos, other)) $ error "filter should return list length 1"
  where filterFn candidate = candidate /= prevPos && existInWorld candidate world

convertToWorld :: [String] -> World
convertToWorld m = World {
  map = Map.fromList [((row, col), ch) | (row, r) <- zip [0..] m, (col, ch) <- zip [0..] r, ch /= ' '],
  maxRow = length m - 1,
  maxCol = length (m !! 0) - 1
}

getPosChar :: World -> Pos -> Char
getPosChar (World { map }) pos  = Maybe.fromJust $ Map.lookup pos map

getNextDir :: Pos -> Pos -> Dir
getNextDir from@(r1, c1) to@(r2, c2)
  | r1 == r2 && c1 < c2 = Main.Right
  | r1 == r2 && c1 > c2 = Main.Left
  | r1 < r2 && c1 == c2 = Down
  | r1 > r2 && c1 == c2 = Up

getNextPos :: Pos -> Dir -> Pos
getNextPos (r, c) Up = (r-1, c)
getNextPos (r, c) Down = (r+1, c)
getNextPos (r, c) Main.Left = (r, c-1)
getNextPos (r, c) Main.Right = (r, c+1)

stateFn :: World -> Maybe Pos -> Dir -> State.State (Pos, String) ()
stateFn _ Nothing _ = return ()
stateFn world (Just pos) cursor = let
    currentChar = getPosChar world pos
    toMaybePos pos = if existInWorld pos world
      then Just pos
      else Nothing
  in do
    (prevPos, answer) <- State.get

    if Char.isAlpha currentChar
      then State.put (pos, currentChar:answer)
      else State.put (pos, answer)

    let (nextPos, nextCursor) = if currentChar == '+'
        then
          let next = Maybe.fromJust $ findNextPos world prevPos pos
          in (Just next, getNextDir pos next)
        else (toMaybePos $ getNextPos pos cursor, cursor)

    stateFn world nextPos nextCursor

runState :: Pos -> World -> String
runState startPos world = reverse.snd.snd $ State.runState (stateFn world (Just startPos) Down) (startPos, "")

part1 :: [String] -> IO (Maybe String)
part1 map@(x:_) = do
  let startCol = Maybe.fromJust $ List.findIndex (=='|') x
  let world = convertToWorld map
  let result = runState (0, startCol) world

  return $ Just result

stateFn' :: World -> Maybe Pos -> Dir -> State.State (Pos, Int) ()
stateFn' _ Nothing _ = return ()
stateFn' world (Just pos) cursor = let
    currentChar = getPosChar world pos
    toMaybePos pos = if existInWorld pos world
      then Just pos
      else Nothing
  in do
    (prevPos, answer) <- State.get
    State.put (pos, answer + 1)

    let (nextPos, nextCursor) = if currentChar == '+'
        then
          let next = Maybe.fromJust $ findNextPos world prevPos pos
          in (Just next, getNextDir pos next)
        else (toMaybePos $ getNextPos pos cursor, cursor)

    stateFn' world nextPos nextCursor

runState' :: Pos -> World -> Int
runState' startPos world = snd.snd $ State.runState (stateFn' world (Just startPos) Down) (startPos, 0)

part2 :: [String] -> IO (Maybe Int)
part2 map@(x:_) = do
  let startCol = Maybe.fromJust $ List.findIndex (=='|') x
  let world = convertToWorld map
  let result = runState' (0, startCol) world

  return $ Just result

testData :: [String]
testData = snd <$> Maybe.fromJust $ List.uncons $ lines $ [r|
     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+
|]

test :: [([String], String)]
test = [
    (testData, "ABCDEF")
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 38)
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
