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

digitRegex :: String
digitRegex = [r|-?[0-9]+|]

type Val3 = (Int, Int, Int)
data Particle = Particle {
  id :: Int,
  pos :: Val3,
  vel :: Val3,
  acc :: Val3
} deriving (Show)

instance Eq Particle where
  (==) :: Particle -> Particle -> Bool
  (==) p1 p2 = distanceFromCenter p1 == distanceFromCenter p2

instance Ord Particle where
  compare :: Particle -> Particle -> Ordering
  compare p1 p2 = compare (distanceFromCenter p1) (distanceFromCenter p2)

updateParticle :: Particle -> Particle
updateParticle (Particle { id, pos=(x,y,z), vel=(vx,vy,vz), acc=(ax,ay,az) }) = Particle {
  id,
  pos = (x+vx+ax, y+vy+ay, z+vz+az),
  vel = (vx+ax, vy+ay, vz+az),
  acc = (ax,ay,az)
}

distanceFromCenter :: Particle -> Int
distanceFromCenter (Particle { pos=(x,y,z) }) = sum $ map abs [x,y,z]

-- magic number XD
loopThreshold :: Int
loopThreshold = 200 -- run state at least this number and all states must be same

stateFn :: ([[Particle]] -> Bool) -> ([Particle] -> [Particle]) -> State.State [[Particle]] [Particle]
stateFn checkFn updateStateFn = do
  allPs <- State.get
  if length allPs == loopThreshold && checkFn allPs
    then return $ head allPs
    else do
      State.modify (\aps@(p:_) -> take loopThreshold $ updateStateFn p:aps)
      stateFn checkFn updateStateFn

getParticleId :: Particle -> Int
getParticleId (Particle { id }) = id

toParticle :: Int -> [Int] -> Particle
toParticle id [x,y,z,vx,vy,vz,ax,ay,az] = Particle {
  id, pos = (x,y,z), vel = (vx,vy,vz), acc = (ax,ay,az)
}

inputToParticles :: [String] -> [Particle]
inputToParticles xs =
  let nums = map (\x -> map read $ getAllTextMatches (x =~ digitRegex) :: [Int]) xs
  in zipWith toParticle [0..] nums

part1 :: [String] -> IO (Maybe Int)
part1 xs = do
  let particles = inputToParticles xs
  let closestParticle = minimum $ State.evalState (stateFn checkFn (map updateParticle)) [particles]
  return . Just $ getParticleId closestParticle
  where
    checkFn :: [[Particle]] -> Bool
    checkFn prevStates = length (List.nub $ map (getParticleId . minimum) prevStates) == 1

getParticlePos :: Particle -> Val3
getParticlePos (Particle { pos }) = pos

groupByFn :: (Ord k) => (v -> k) -> [v] -> Map.Map k [v]
groupByFn fn = Map.fromListWith (++) . map (\val -> (fn val, [val]))

part2 :: [String] -> IO (Maybe Int)
part2 xs = do
  let particles = inputToParticles xs
  let remainParticles = State.evalState (stateFn checkFn updateFn) [particles]
  return . Just $ length remainParticles
  where
    checkFn :: [[Particle]] -> Bool
    checkFn prevStates = length (List.nub $ map length prevStates) == 1
    updateFn :: [Particle] -> [Particle]
    updateFn ps = let ps' = map updateParticle ps
      in concat -- concat the remain groups (single particle) to list of particles
        $ List.filter (\gp -> 1 == length gp) -- remove the group where particles at same position > 1
        $ map snd -- get only list of grouped particles
        $ Map.toList -- convert from map to list
        $ groupByFn getParticlePos ps' -- group by particle position

testData :: [String]
testData = lines $ chomp [r|
p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
|]

testData2 :: [String]
testData2 = lines $ chomp [r|
p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=<0,0,0>
|]

test :: [([String], Int)]
test = [
    (testData, 0)
  ]

test2 :: [([String], Int)]
test2 = [
    (testData2, 1)
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
