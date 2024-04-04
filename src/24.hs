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

newtype Comp = Comp {
  pin :: (Int, Int)
} deriving (Show, Eq)

getCompsFromPort :: [Comp] -> Int -> [Comp]
getCompsFromPort comps port = filter
  (\(Comp (p1, p2)) -> p1 == port || p2 == port)
  comps

bridgeStrength :: [Comp] -> Int
bridgeStrength comps = sum $ concatMap (\(Comp (p1, p2)) -> [p1, p2]) comps

parseComps :: String -> Comp
parseComps (Text.pack -> str) =
  let [a, b] = map Text.unpack $ Text.split (=='/') str
  in Comp { pin = (read a , read b) }

removeComps :: [Comp] -> Comp -> [Comp]
removeComps comps comp = filter (/= comp) comps

getNextPort :: Comp -> Int -> Int
getNextPort (Comp (p1, p2)) p
  | p == p1 = p2
  | otherwise = p1

findMaxStrength :: [Comp] -> ([Comp], Int) -> Int
findMaxStrength availableComps (bridge, connectPort) =
  let
    candidates = getCompsFromPort availableComps connectPort
  in
    if null candidates
      then bridgeStrength bridge
      else maximum $ map (\cd -> findMaxStrength
        (removeComps availableComps cd)
        (cd:bridge, getNextPort cd connectPort)
      ) candidates

part1 :: [String] -> IO (Maybe Int)
part1 (map parseComps -> comps) = do
  return . Just $ findMaxStrength comps ([], 0)

findAllBridge :: [Comp] -> ([Comp], Int) -> [[Comp]]
findAllBridge availableComps (bridge, connectPort) =
  let
    candidates = getCompsFromPort availableComps connectPort
  in
    if null candidates
      then [bridge]
      else concatMap (\cd -> findAllBridge
        (removeComps availableComps cd)
        (cd:bridge, getNextPort cd connectPort)
      ) candidates

part2 :: [String] -> IO (Maybe Int)
part2 (map parseComps -> comps) = do
  let (_, longestBriges) = foldl (\acc@(len, bridgeAcc) bridge ->
        case length bridge of
          newLength | newLength == len -> (len, bridge:bridgeAcc)
                    | newLength > len  -> (newLength, [bridge])
                    | otherwise -> acc
        ) (0,[]) $ findAllBridge comps ([], 0)
  return . Just $ maximum $ map bridgeStrength longestBriges

testData :: [String]
testData = lines $ chomp [r|
0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10
|]

test :: [([String], Int)]
test = [
    (testData, 31)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 19)
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
