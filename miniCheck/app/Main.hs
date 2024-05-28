module Main where

import System.IO
import TransitionSystem
import ComputationalTreeLogic
import ModelChecking
import Text.Parsec (parse)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Either (fromRight)
import System.Exit (exitFailure)

exitOnLeft :: Show a => Either a b -> IO b
exitOnLeft (Left err) = do
  print (show err)
  exitFailure
exitOnLeft (Right result) = return result
  
main :: IO ()
main = do
  -- parse arguments
  args <- getArgs
  checkArgs args
  let ts_path = args !! 0
  let ctl_path = args !! 1
  -- read and parse transition system file
  ts_file <- openFile ts_path ReadMode
  ts_contents <- hGetContents ts_file
  ts <- exitOnLeft $ parse parseTransitionSystem ts_path ts_contents
  print ts
  hClose ts_file
  -- read and parse ctl file
  ctl_file <- openFile ctl_path ReadMode
  ctl_contents <- hGetContents ctl_file
  ctl <- exitOnLeft $ parse parseComputationalTreeLogic ctl_path ctl_contents
  print ctl
  hClose ctl_file
  -- model checking
  let result = modelCheck ts ctl
  print result


checkArgs :: [String] -> IO ()
checkArgs args
  | length(args) == 2 = printf "Reading from path %s and %s\n" (args !! 0) (args !! 1)
  | otherwise = error "Too few or too many arguments. Please provide exactly 2 arguments."
