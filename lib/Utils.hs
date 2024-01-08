module Utils where

import Text.Printf ( printf )
import System.IO (readFile)
import Data.Maybe (fromJust)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Control.Exception (catch, SomeException)
import Control.Monad (join)
import System.Clock
import Data.Text (strip, unpack, pack, split)

chomp :: String -> String
chomp = unpack . strip . pack

inputFile :: String -> String
inputFile date = "src/" ++ date ++ "/input.txt"

getInput :: String -> IO [String]
getInput date = lines <$> getRawInput date

getRawInput :: String -> IO String
getRawInput date = catch (readFile $ inputFile date) handler
  where
    handler :: SomeException -> IO String
    handler ex = printf "\n[ERR] src/%s/input.txt not found, exit with code 1\n" date
      >> exitWith (ExitFailure 1)

diffMicroSec :: TimeSpec -> TimeSpec -> Integer
diffMicroSec stop start = toNanoSecs $ diffTimeSpec stop start `div` 1000

assertSolution :: (Eq a, Show a) => Int -> ([String] -> IO (Maybe a)) -> [String] -> a -> IO ()
assertSolution part fn input expect = do
  putStrLn $ "Part " ++ show part
  start <- getTime Monotonic
  result <- fn input
  stop <- getTime Monotonic
  case result of
    Nothing -> print "No result"
    Just result -> printf "Expect %s | Result %s (%s)\n" (show expect) (show result) (show $ expect == result)
  printf "Exec time: %d µs\n" (diffMicroSec stop start)
  putStrLn ""

runSolution :: (Show b) => Int -> (a -> IO (Maybe b)) -> a -> IO ()
runSolution part fn input = do
  putStrLn $ "Part " ++ show part
  start <- getTime Monotonic
  result <- fn input
  stop <- getTime Monotonic
  putStrLn $ maybe "No result" show result
  printf "Exec time: %d µs\n" (diffMicroSec stop start)
  putStrLn ""
