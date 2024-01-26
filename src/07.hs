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
import Data.Foldable (for_, Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import Data.List (findIndex, elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text as Text (Text, splitOn, pack, unpack )
import Data.Char (isDigit)

type Program = (String, Int, [String])
parse :: Text -> Program
parse str = (key, val, children)
  where
    (l:r) = splitOn (pack " -> ") str
    key = takeWhile (/=' ') $ unpack l
    val = read $ filter isDigit $ unpack l :: Int
    children = map unpack $ if null r then [] else splitOn (pack ", ") $ head r

findRoot :: [Program] -> String
findRoot progs = head $ Set.toList diff
  where
    (parents, _, children) = unzip3 progs
    setParents = Set.fromList parents
    setChildren = Set.fromList $ concat $ filter (not . null) children
    diff = Set.difference setParents setChildren

part1 :: [String] -> IO (Maybe String)
part1 s = do
  return . Just $ findRoot progs
  where progs = map (parse . pack) s

tree :: [Program] -> Map String [(String, Int)]
tree progs = valMap'
  where
    valMap = Map.fromList [(key, val) | (key, val, _) <- progs]
    valMap' = Map.fromList [(parent, map selfAndLeafVal children) | (parent, _, children) <- progs]
    selfAndLeafVal node = (node, (valMap Map.! node) + sum (map snd (valMap' Map.! node)))

unbalance :: [(String, Int)] -> Maybe (Int, String)
-- unbalance vs | DBG.trace (show vs) False = undefined
unbalance [(_, w1), (_, w2), (_, w3)] | w1 == w2 && w2 == w3 = Nothing
unbalance (node1@(key1, val1) : node2@(key2, val2) : (key3, val3) : xs)
  | val1 == val2 && val2 == val3 = unbalance (node1:node2:xs) -- proceed
  | val2 == val3 = Just (val2, key1) -- 1 wrong, expect 2
  | val3 == val1 = Just (val3, key2) -- 2 wrong, expect 3
  | val1 == val2 = Just (val1, key3) -- 3 wrong, expect 1
unbalance xs = error $ "expect tree to be balance or wrong by at least 1, got " ++ show xs

-- curried for node key
findValChange :: [Program] -> String -> Int
findValChange progs = findValChange' (error "balanced")
  where
    treeMap = tree progs
    findValChange' expectVal candidate =
      -- if children are balance (return Nothing)
      --  return current node's val - sum of children
      -- otherwise findVal on node that unbalance
      maybe (expectVal - sum (map snd children)) (uncurry findValChange') (unbalance children)
      where children = treeMap Map.! candidate

part2 :: [String] -> IO (Maybe Int)
part2 s = do
  return . Just $ findValChange progs $ findRoot progs
  where progs = map (parse . pack) s

testData :: [String]
testData = lines $ chomp [r|
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
|]

test :: [([String], String)]
test = [
    (testData, "tknk")
  ]

test2 :: [([String], Int)]
test2 = [
    (testData, 60)
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
