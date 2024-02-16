{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace)
import Text.RawString.QQ (r)
import System.Environment (getArgs, getProgName)
import Data.List (sort, find, foldl', nub, elemIndex, mapAccumL)
import Data.Foldable (for_, toList, foldMap')
import qualified Data.Sequence as Seq (Seq, fromList, foldlWithIndex, chunksOf, mapWithIndex)
import qualified Data.Text as Txt (Text, splitOn, pack, unpack)
import qualified Data.Map as Map (Map, empty, (!), member, insert, singleton, lookup)
-- import qualified Data.Set as Set (Set)
import Numeric (showBin)
import Data.Bits (bit, (.&.))
import Data.Char (isDigit, isAlpha)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (foldM)
import Data.Either (fromLeft, fromRight, isLeft)

type Address = Either Char Int
address :: String -> Address
address [ch] | isAlpha ch = Left ch
address s = Right (read s :: Int)

data Code = OpsSnd Address -- play sound from value in address
  | OpsBin (Int -> Int -> Int) Char Address
  | OpsRcv Char -- recovery
  | OpsJgz Address Address -- jump on greater than zero

parseToCode :: String -> Code
-- parseToCode inst | trace inst False = undefined
parseToCode inst = case words inst of
  "snd":(address->val):_ -> OpsSnd val
  "set":[tgt]:(address->val):_ -> OpsBin (const id) tgt val -- == flip const
  "add":[tgt]:(address->val):_ -> OpsBin (+) tgt val
  "mul":[tgt]:(address->val):_ -> OpsBin (*) tgt val
  "mod":[tgt]:(address->val):_ -> OpsBin mod tgt val
  "mod":[tgt]:(address->val):_ -> OpsBin mod tgt val
  "rcv":[tgt]:_ -> OpsRcv tgt
  "jgz":(address->tgt):(address->val):_ -> OpsJgz tgt val
  _ -> error "error on parse"

type Memory = Map.Map Char Int
exec :: (Memory, Int) -> Code -> Either Int (Memory, Int)
-- exec (mem, latest) _ | trace (show (mem, latest)) False = undefined
exec (mem, latest) (OpsSnd (Left ch)) = Right (mem, fromMaybe 0 $ Map.lookup ch mem)
exec (mem, latest) (OpsRcv ch) = case mem Map.! ch of
  0 -> Right (mem, latest)
  n -> Left latest
exec (mem, latest) (OpsBin fn tgt (getAddressVal mem -> val)) = Right (mem', latest)
  where
    tgt' = Map.lookup tgt mem
    res = fromMaybe 0 tgt' `fn` val
    mem' = Map.insert tgt res mem

getAddressVal :: Memory -> Address -> Int
getAddressVal mem (Left ch) = fromMaybe 0 $ Map.lookup ch mem
getAddressVal _ (Right v) = v

engine :: [Code] -> Int
engine insts = let Left res = foldM fn (Map.empty, 0, 0) $ repeat 0
  in res
  where
    fn (mem, latest, pc) _ = case insts !! pc of
      OpsJgz (getAddressVal mem -> tgt) (getAddressVal mem -> val) ->
        Right (mem, latest, pc + (if tgt > 0 then val else 1))
      inst -> case exec (mem, latest) inst of
        Right (mem', latest') -> Right (mem', latest', pc + 1)
        Left val -> Left val

part1 :: [String] -> IO (Maybe Int)
part1 inputs = do
  return . Just $ engine inputs'
  where inputs' = map parseToCode inputs

type Pipe = [Int]
exec' :: (Memory, Pipe, Pipe) -> Code -> Maybe (Memory, Pipe, Pipe)
exec' (mem, pipeIn, pipeOut) (OpsSnd (getAddressVal mem -> val)) = Just (mem, pipeIn, pipeOut ++ [val])
exec' (_, [], _) (OpsRcv tgt) = Nothing
exec' (mem, x:xs, pipeOut) (OpsRcv tgt) = Just (Map.insert tgt x mem, xs, pipeOut)
exec' (mem, i, o) (OpsBin fn tgt (getAddressVal mem -> val)) = Just (mem', i, o)
  where
    tgt' = Map.lookup tgt mem
    res = fromMaybe 0 tgt' `fn` val
    mem' = Map.insert tgt res mem

isSend :: Code -> Bool
isSend (OpsSnd _) = True
isSend _ = False

execOrBranch :: Int -> Memory -> Int -> Pipe -> Pipe -> Code -> (Bool, Int, Memory, Pipe, Pipe, Int)
execOrBranch count ram pc pipeIn pipeOut code = case code of
  OpsJgz (getAddressVal ram -> tgt) (getAddressVal ram -> val) ->
    (False, count, ram, pipeIn, pipeOut, pc + (if tgt > 0 then val else 1))
  code ->
    let count' = if isSend code then count + 1 else count
    in case exec' (ram, pipeIn, pipeOut) code of
    Nothing -> (True, count', ram, pipeIn, pipeOut, pc)
    Just (ram', pipeIn', pipeOut') -> (False, count', ram', pipeIn', pipeOut', pc + 1)

type State = (Int, Memory, Memory, Int, Int, Pipe, Pipe, [Code])
runProcess :: State -> Int -> Either State State
-- runProcess (sndCount, ram0, ram1, pc0, pc1, pipe01, pipe10, _) n
--   | trace (show (n, sndCount, ram0, ram1, pc0, pc1, pipe01, pipe10)) False = undefined
runProcess state n = if isLeft then Left newState else Right newState
  where
    (sndCount, ram0, ram1, pc0, pc1, pipe01, pipe10, codes) = state
    (isLeft, newState) = case n of
      0 ->
        let (isLeft, _, ram0',  pipe10', pipe01', pc0') = execOrBranch 0 ram0 pc0 pipe10 pipe01 (codes !! pc0)
        in (isLeft, (sndCount, ram0', ram1, pc0', pc1, pipe01', pipe10', codes))
      1 ->
        let (isLeft, sndCount', ram1',  pipe01', pipe10', pc1') = execOrBranch sndCount ram1 pc1 pipe01 pipe10 (codes !! pc1)
        in (isLeft, (sndCount', ram0, ram1', pc0, pc1', pipe01', pipe10', codes))

duplicate :: a -> (a, a)
duplicate a = (a, a)

getFirstFromState :: State -> Int
getFirstFromState (n, _, _, _, _, _, _, _) = n

removeEither :: Either State State -> State
removeEither (Left s) = s
removeEither (Right s) = s

engines :: [Code] -> Int
engines codes = fromLeft 0 $ foldM fn initState (repeat 0)
  where
    initState = (0, Map.singleton 'p' 0, Map.singleton 'p' 1, 0, 0, [], [], codes) :: State
    fn acc _ =
      let (acc', result) = mapAccumL (\a pc -> duplicate $ runProcess (removeEither a) pc) (Right acc) [0, 1]
      in if all isLeft result
        then Left $ getFirstFromState $ removeEither acc'
        else Right $ removeEither acc'

part2 :: [String] -> IO (Maybe Int)
part2 inputs = do
  return . Just $ engines inputs'
  where inputs' = map parseToCode inputs

testData :: [String]
testData = lines $ chomp [r|
set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2
|]

testData2 :: [String]
testData2 = lines $ chomp [r|
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d
|]

test :: [([String], Int)]
test = [
    (testData, 4)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData2, 3)
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
