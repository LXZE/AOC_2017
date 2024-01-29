module Utils where

import Text.Printf ( printf )
import System.IO (readFile)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Control.Exception (catch, SomeException)
import Data.Text (strip, unpack, pack, split)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.DeepSeq (deepseq, NFData)

chomp :: String -> String
chomp = unpack . strip . pack

inputFile :: String -> String
inputFile date = "inputs/" ++ date ++ ".txt"

getInput :: String -> IO [String]
getInput date = lines <$> getRawInput date

getRawInput :: String -> IO String
getRawInput date = catch (readFile $ inputFile date) handler
  where
    handler :: SomeException -> IO String
    handler ex = printf "\n[ERR] inputs/%s.txt not found, exit with code 1\n" date
      >> exitWith (ExitFailure 1)

timeit :: (NFData a, NFData b) => (a -> IO b) -> a -> IO (Double, b)
timeit action input = do
  start <- getCurrentTime
  result <- deepseq start action input
  end <- deepseq result getCurrentTime
  return (realToFrac $ diffUTCTime end start, result)

assertSolution :: (NFData a, NFData b, Eq a, Show a) =>
  Int -> (b -> IO (Maybe a)) -> b -> a -> IO ()
assertSolution part fn input expect = do
  putStrLn $ "Part " ++ show part
  (sec, result) <- timeit fn input
  case result of
    Nothing -> print "No result"
    Just result -> printf "Expect %s | Result %s (%s)\n" (show expect) (show result) (show $ expect == result)
  printf "Exec time: %f seconds\n\n" sec

runSolution :: (NFData a, NFData b, Show b) =>
  Int -> (a -> IO (Maybe b)) -> a -> IO ()
runSolution part fn input = do
  putStrLn $ "Part " ++ show part
  (sec, result) <- timeit fn input
  putStrLn $ maybe "No result" show result
  printf "Exec time: %f seconds\n\n" sec
