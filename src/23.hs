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

type Register = Map.Map Char Int
data Value = Address Char | Raw Int deriving (Show)
address :: String -> Value
address [ch] | Char.isAlpha ch = Address ch
address s = Raw (read s :: Int)

data Code = OpsBin String (Int -> Int -> Int) Char Value
  | OpsJnz Value Value

instance Show Code where
  show :: Code -> String
  show (OpsBin fnName _fn ch addr) = "FN: " ++ fnName ++ show ch ++ show addr
  show (OpsJnz addr1 addr2) = "Jnz" ++ show addr1 ++ show addr2

parseCode :: String -> Code
parseCode inst = case words inst of
  "set":[reg]:(address -> val):_ -> OpsBin "set" (const id) reg val
  "sub":[reg]:(address -> val):_ -> OpsBin "sub" (-) reg val
  "mul":[reg]:(address -> val):_ -> OpsBin "mul" (*) reg val
  "jnz":(address -> v1):(address -> v2):_ -> OpsJnz v1 v2
  _ -> error $ "error unparsable: " ++ inst

getValue :: Register -> Value -> Int
getValue reg (Address addr) = Maybe.fromJust $ Map.lookup addr reg
getValue _ (Raw v) = v

type PC = Int
fetchCode :: [Code] -> PC -> Code
fetchCode codes pc = codes !! pc

isCodeMul :: Code -> Bool
isCodeMul (OpsBin "mul" _ _ _) = True
isCodeMul _ = False

stateFn :: [Code] -> PC -> State.State (Register, Int) Int
stateFn codes pc | pc >= length codes = State.get <&> snd
stateFn codes pc = do
  (reg, mulCount) <- State.get
  let code = fetchCode codes pc
  let (newReg, jmpAmnt) = case code of
          (OpsBin _ fn tgt (getValue reg -> val))
            -> let tgtVal = getValue reg (Address tgt)
              in (Map.update (const $ Just $ fn tgtVal val) tgt reg, 1)
          (OpsJnz (getValue reg -> condVal) (getValue reg -> jmpVal))
            -> (reg, if condVal /= 0 then jmpVal else 1)

  State.put (newReg, mulCount + if isCodeMul code then 1 else 0)
  stateFn codes (pc + jmpAmnt)

part1 :: [String] -> IO (Maybe Int)
part1 cmds = let
  regs = Map.fromList $ map (, 0) ['a'..'h']
  codes = map parseCode cmds
  in do
  let res = State.evalState (stateFn codes 0) (regs, 0)
  return $ Just res

-- for debugging
stateFn' :: [Code] -> PC -> State.State Register Int
stateFn' codes pc | pc >= length codes = do
  State.get >>= \reg -> return $ reg Map.! 'h'
stateFn' codes pc = do
  reg <- State.get
  let code = fetchCode codes pc
  let (newReg, jmpAmnt) = case code of
          (OpsBin _ fn tgt (getValue reg -> val))
            -> let tgtVal = getValue reg (Address tgt)
              in (Map.update (const $ Just $ fn tgtVal val) tgt reg, 1)
          (OpsJnz (getValue reg -> condVal) (getValue reg -> jmpVal))
            -> (reg, if condVal /= 0 then jmpVal else 1)

  trace (show reg) State.put newReg
  stateFn' codes (pc + jmpAmnt)

part2 :: [String] -> IO (Maybe Int)
part2 cmds = do
  -- regs = Map.fromList $ map (, 0) ['a'..'h']
  -- regs' = Map.update (const $ Just 1) 'a' regs
  -- codes = map parseCode cmds
  -- in do
  -- let res = State.evalState (stateFn' codes 0) regs'
  -- print res

-- b = 106500
-- c = 123500
-- while b != c
--   isFound = False
--   d = 2
--   e = 2
--   while d != b # line 22-24
--     while e != b # line 18-20
--       if (d*e) - b == 0 # line 12-15
--         isFound = True # line 16
--       e += 1 # line 17
--     d += 1 # line 21
--   if isFound # line 25
--     counter += 1 # line 26
--   b += 17

  -- assembly to haskell (optimized)
  -- count numbers between [106500, (106500 + 17) .. 123500]
  -- that has any composite number
  let res = length $ filter
        (\n -> any (\m -> n `mod` m == 0) [2..(n-1)])
        [106500, (106500 + 17) .. 123500]
  return $ Just res

testData :: [String]
testData = lines $ chomp [r|
|]

test :: [([String], Int)]
test = [
    -- (testData, )
  ]

test2 :: [([String], Int)]
test2 = [
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
