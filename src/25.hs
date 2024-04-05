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

-- int to write, move left or right (-1, 1), next state char
data Choice = Choice Int Int Char
  deriving (Show)
type Rule = Map.Map Char (Choice, Choice)

groupByEmptyLine :: [[String]] -> [String] -> [[String]]
groupByEmptyLine acc [] = acc
groupByEmptyLine acc@(f:b) (line:rem)
  | line == "" =  groupByEmptyLine ([]:acc) rem
  | otherwise = groupByEmptyLine ((line:f):b) rem

translate :: String -> Int
translate "left" = -1
translate "right" = 1

parseChoice :: [String] -> Choice
parseChoice [
  read . last . words -> c1,
  translate . last . words -> c2,
  head . last . words -> c3
  ] = Choice c1 c2 c3

parseCmd :: [String] -> (Choice, Choice)
parseCmd (splitAt 4 -> (_:a,_:b)) = (parseChoice a, parseChoice b)

cleanString :: String -> String
cleanString = filter (\c -> c /= '.' && c /= ':')

stateFn :: Char -> Int -> Int -> (Int, Rule) -> State.State (Map.Map Int Int) ()
stateFn state cursor step const@(stop, rule) | step == stop = return ()
stateFn state cursor step const@(stop, rule) = do
  tape <- State.get
  let (Choice val offset newState) = (
        case Maybe.fromMaybe 0 $ Map.lookup cursor tape of
            0 -> fst; 1 -> snd
        ) $ Maybe.fromJust $ Map.lookup state rule

  State.put $ Map.insert cursor val tape
  stateFn newState (cursor + offset) (step+1) const

part1 :: [String] -> IO (Maybe Int)
part1 (map cleanString -> text) = do
  let ([
        head . last . words -> startState,
        read . head . tail . reverse . words -> stop :: Int
        ]:rawStates) = reverse $ map reverse $ groupByEmptyLine [[]] text

  let rule = Map.fromList $
        foldl (\acc ((getStateName -> state):cmd) -> (state, parseCmd cmd):acc) [] rawStates

  let res = sum
        $ map snd
        $ Map.toList
        $ State.execState (stateFn startState 0 0 (stop, rule)) Map.empty
  return $ Just res
  where
    getStateName = head . last . words

testData :: [String]
testData = lines $ chomp [r|
Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
|]

test :: [([String], Int)]
test = [
    (testData, 3)
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      Fold.for_ test $ \(input, expect) -> do
        assertSolution 1 part1 input expect
      -- Fold.for_ test2 $ \(input, expect) -> do
      --   assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      -- runSolution 2 part2 input
