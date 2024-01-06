import Utils (
  getInput, getRawInput,
  runSolution, assertSolution
  )

import System.Environment (getArgs, getProgName)
import Data.Foldable (for_)
import Data.Char (digitToInt)

dup_head :: Int -> String -> String
dup_head size str = take (length str + size) (cycle str)

pair :: [Int] -> [(Int, Int)]
pair (x:xs) | length xs > 0 = [(x, xs !! 0)] ++ pair xs
pair _ = []

pair' :: [Int] -> [(Int, Int)]
pair' (x:xs) = [(x, xs !! (x - 1))] ++ pair xs
pair' _ = []

part1 :: [String] -> IO (Maybe Int)
part1 [str] = do
  let res = sum $ map fst $ filter (\(a, b) -> a == b) $ pair $ map digitToInt (dup_head 1 str)
  return (Just res)

part2 :: [String] -> IO (Maybe Int)
part2 [str] = do
  let res = sum $ map fst $ filter (uncurry (==)) $ zip s (drop (length s `div` 2) s)
  return (Just $ res * 2)
  where s = map digitToInt str

test :: [([String], Int)]
test = [
    (["1122"], 3),
    (["1111"], 4),
    (["1234"], 0),
    (["91212129"], 9)
  ]

test2 :: [([String], Int)]
test2 = [
    (["1212"], 6),
    (["1221"], 0),
    (["123425"], 4),
    (["123123"], 12),
    (["12131415"], 4)
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
