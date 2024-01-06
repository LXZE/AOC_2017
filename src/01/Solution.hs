import Utils (
  getInput, getRawInput,
  assertSolution, runSolution
  )

import Debug.Trace (trace)
import System.Environment (getArgs, getProgName)
import System.Directory (getCurrentDirectory)
import Data.Foldable

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  print s
  return Nothing

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  print s
  return Nothing

test :: [([String], Int)]
test = [
    (["1122"], 3),
    (["1111"], 4),
    (["1234"], 0),
    (["91212129"], 9)
  ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      for_ test $ \(input, expect) -> do
        assertSolution 1 part1 input expect
        assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 input
      runSolution 2 part2 input
