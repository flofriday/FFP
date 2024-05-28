{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import TransitionSystem
import ComputationalTreeLogic
import ModelChecking
import Text.Parsec (parse)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Either (fromRight)
import System.Exit (exitFailure, exitSuccess)
import System.Console.CmdArgs
import Data.Data (Data, Typeable)


data MiniCheckArgs = MiniCheckArgs {tsFilePath :: FilePath, ctlFilePath :: FilePath, ts :: Bool}  
                    deriving (Show, Data, Typeable)

miniCheckArgs = MiniCheckArgs{ 
  tsFilePath = def &= argPos 0 &= typ "TS_FILE_PATH", 
  ctlFilePath = def &= argPos 1 &= typ "CTL_FILE_PATH",
  ts = def &= help "Set this flag to only check the input transition system for correctness" &= explicit &= name "ts"
} &= summary "MiniCheck v0.1"


exitOnLeft :: Show a => Either a b -> IO b
exitOnLeft (Left err) = do
  print "ERROR:"
  print err
  exitFailure
exitOnLeft (Right result) = return result
  
main :: IO ()
main = do
  -- parse arguments
  args <- cmdArgs miniCheckArgs
  let ts_path = tsFilePath args
  let ctl_path = ctlFilePath args
  let only_check_ts = ts args
  --printf "Reading from path %s and %s %s\n" (ts_path) (ctl_path) (show only_check_ts)
  -- read and parse transition system file
  ts_file <- openFile ts_path ReadMode
  ts_contents <- hGetContents ts_file
  ts <- exitOnLeft $ parse parseTransitionSystem ts_path ts_contents
  print ts
  hClose ts_file
  if only_check_ts then do
    putStrLn "\n************* \n--ts mode was enabled. Therefore we are only checking the transition system. Exiting...\n*************\n"
    exitSuccess
  else do
    -- read and parse ctl file
    ctl_file <- openFile ctl_path ReadMode
    ctl_contents <- hGetContents ctl_file
    ctl <- exitOnLeft $ parse parseComputationalTreeLogic ctl_path ctl_contents
    print ctl
    hClose ctl_file
    -- model checking
    let result = modelCheck ts ctl
    print result
