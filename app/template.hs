import Utils (
  getInput, getRawInput,
  runSolution, assertSolution
  )

import System.Environment (getArgs, getProgName)
import Data.Foldable (for_)

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return Nothing

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return Nothing

test :: [([String], Int)]
test = [
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
