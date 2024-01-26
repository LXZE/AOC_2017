{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace as DBG
import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.Foldable (for_, Foldable (toList), maximumBy)
import Data.Sequence (fromList, mapWithIndex)
import Data.List (findIndex, elemIndex, mapAccumL)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text as Text (Text, splitOn, pack, unpack)
import Data.Char (isDigit)
import Data.Function (on)


type Inst = (String, Int, String, Int -> Bool)
parse :: String -> Inst
parse str = case words str of
  [target, cmd, amntStr, "if", deps, condCmd, condValStr] -> (target, val, deps, cond)
    where
      valCmd "inc" = 1
      valCmd "dec" = -1
      valCmd _ = error ("invalid command " ++ str)
      amnt = read amntStr :: Int
      val = valCmd cmd * amnt
      condVal = read condValStr :: Int
      condFn = case condCmd of
        "==" -> (==)
        "!=" -> (/=)
        ">" -> (<)
        ">=" -> (<=)
        "<" -> (>)
        "<=" -> (>=)
      cond = condFn condVal
  _ -> error ("invalid instruction " ++ str)

run :: Map String Int -> Inst -> Map String Int
run regs (tgt, val, src, fn) = if condRes then regs' else regs
  where
    condRes = fn $ Map.findWithDefault 0 src regs
    regs' = snd $ Map.insertLookupWithKey (const (+)) tgt val regs

run' :: Map String Int -> Inst -> (Map String Int, Int)
run' regs (tgt, val, src, fn) = if condRes then (regs', val') else (regs, 0)
  where
    condRes = fn $ Map.findWithDefault 0 src regs
    (prevVal, regs') = Map.insertLookupWithKey (const (+)) tgt val regs
    val' = val + fromMaybe 0 prevVal

part1 :: [String] -> IO (Maybe Int)
part1 s = do
  return . Just $ maximum $ map snd $ Map.toList (foldl run Map.empty codes)
  where codes = map parse s

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ maximum $ snd $ mapAccumL run' Map.empty codes
  where codes = map parse s

testData :: [String]
testData = lines $ chomp [r|
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
|]

test :: [([String], Int)]
test = [
    (testData, 1)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 10)
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
