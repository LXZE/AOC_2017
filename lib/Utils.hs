module Utils where

import System.Environment (getArgs)
import System.IO (readFile)
import GHC.IO.Exception (IOErrorType(UserError))
import Data.Monoid (Any)

inputFile :: String -> String
inputFile date = "src/" ++ date ++ "/input.txt"

getInput :: String -> IO [String]
getInput date = lines <$> getRawInput date

getRawInput :: String -> IO String
getRawInput date = readFile $ inputFile date

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