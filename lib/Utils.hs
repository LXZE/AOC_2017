module Utils where

import Text.Printf ( printf )
import System.IO (readFile)
import System.Exit ( ExitCode(ExitFailure), exitWith )
import Control.Exception (catch, SomeException)

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

assertSolution :: Eq a => Int -> ([String] -> IO (Maybe a)) -> [String] -> a -> IO ()
assertSolution part fn input expect = do
  print $ "Part " ++ show part
  result <- fn input
  print $ case result of
    Nothing -> "No result"
    Just result -> show $ expect == result
  putStrLn ""

runSolution :: Int -> (a -> IO (Maybe Int)) -> a -> IO ()
runSolution part fn input = do
  print $ "Part " ++ show part
  fn input >>= print
  putStrLn ""
