import Utils (
  getInput, getRawInput, chomp,
  runSolution, assertSolution
  )

import Debug.Trace (trace, traceShowId)
import Text.RawString.QQ (r)
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Text.Regex.TDFA.Text (Regex)
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

type Row = Text.Text
type Grid = [Row]
type Rule = Map.Map Grid Grid

parseRule :: String -> [(Grid, Grid)]
parseRule line = case words line of
  [unline -> from,_, unline -> to] -> map (,to)
    $ take 4 (iterate rotateGrid from) ++
      take 4 (iterate rotateGrid $ flipGridH from) ++
      take 4 (iterate rotateGrid $ flipGridV from)
  _ -> error ("unexpected line struct: " ++ line)
  where unline s = Text.split (=='/') $ Text.pack s

transpose :: Grid -> Grid
transpose grid = map Text.pack $ List.transpose $ map Text.unpack grid
rotateGrid :: Grid -> Grid
rotateGrid = flipGridH . transpose

flipGridH :: Grid -> Grid
flipGridH = map Text.reverse
flipGridV :: Grid -> Grid
flipGridV = reverse

splitGrid :: Grid -> [Grid]
splitGrid grid =
  Fold.toList $ Map.fromListWith (++) $ reverse . concatMap mapFn $ zip [0..] chunks
  where
    getChunkSize :: Int -> Int
    getChunkSize size
      | even size = 2
      | size `mod` 3 == 0 = 3
      | otherwise = error $ "unexpected grid size: " ++ show size
    chunkSize = getChunkSize $ length grid
    chunks = map (Text.chunksOf chunkSize) grid
    mapFn (rowIdx, row) = let gridRow = rowIdx `div` chunkSize
      in zipWith (\gridCol elem -> ((gridRow * length row) + gridCol, [elem])) [0..] row

getGrid :: Rule -> Grid -> Grid
getGrid rules grid = Maybe.fromMaybe
  (error $ trace ("error at: " ++ show grid) "unexpected grid")
  $ Map.lookup grid rules

concatGrid :: [Grid] -> Grid
concatGrid (Seq.fromList -> grids) = let
  colSize = floor . sqrt . fromIntegral $ length grids
  rows = map Fold.toList . Fold.toList $ Seq.chunksOf colSize grids
  in concatMap joinGridRows rows

joinGridRows :: [[Row]] -> [Row]
joinGridRows grids | all (==[]) grids = []
joinGridRows grids = let
  heads = Text.concat $ map head grids
  tails = map tail grids
  in heads:joinGridRows tails

enhanceGrid :: Rule -> Grid -> Grid
enhanceGrid rules = concatGrid . map (getGrid rules) . splitGrid

debugGrid :: Grid -> IO Grid
debugGrid grid = do
  Monad.forM_ grid (putStrLn . Text.unpack)
  return grid

countPixel :: Grid -> Int
countPixel grid = sum $ map (Text.count "#") grid

part1 :: (Int, [String]) -> IO (Maybe Int)
part1 (n, xs) = do
  let rules = Map.fromList $ concatMap parseRule xs
  let res = iterate (enhanceGrid rules) starter !! n
  -- debugGrid res
  return . Just $ countPixel res

starter :: Grid
starter = map Text.pack $ lines $ chomp [r|
.#.
..#
###
|]

testData :: [String]
testData = lines $ chomp [r|
../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#
|]

test :: [([String], Int)]
test = [
    (testData, 12)
  ]

-- test2 :: [([String], Int)]
-- test2 = [
--     (testData2, 1)
--   ]

main :: IO ()
main = do
  args <- getArgs
  date <- getProgName
  if args == ["test"]
    then do
      Fold.for_ test $ \(input, expect) -> do
        assertSolution 1 part1 (2, input) expect
      -- Fold.for_ test2 $ \(input, expect) -> do
      --   assertSolution 2 part2 input expect
    else do
      input <- getInput date
      runSolution 1 part1 (5, input)
      runSolution 2 part1 (18, input)
