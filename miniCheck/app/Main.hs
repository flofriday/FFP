{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.IO
import TransitionSystem
import ComputationalTreeLogic
import ModelChecking
import Text.Parsec (parse)
import System.Environment (getArgs, withArgs)
import Text.Printf (printf)
import Data.Either (fromRight)
import System.Exit (exitFailure, exitSuccess)
import System.Console.CmdArgs
import Data.Data (Data, Typeable)

-- | Name of the program
_PROGRAM_NAME = "miniCheck"
-- | Version of the program
_PROGRAM_VERSION = "1.0"
-- | Program info consisting of the name and the version
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
-- | A short description of the program
_PROGRAM_ABOUT = "a simple command line tool for convenient model checking"
-- | Authors of the program
_AUTHORS = "Johannes Blaha, Florian Freitag"

data MiniCheckArgs = NormalMode {tsFilePath :: FilePath, ctlFilePath :: FilePath, ts :: Bool}  
                    | ExtensionMode {extensions :: Bool}  
                    deriving (Show, Data, Typeable, Eq)

-- | Defines the arguments for the normal mode. This mode is also used it no mode is specified
normalMode :: MiniCheckArgs
normalMode = NormalMode{ 
  tsFilePath = def &= argPos 0 &= typ "TS_FILE_PATH", 
  ctlFilePath = def &= argPos 1 &= typ "CTL_FILE_PATH",
  ts = def &= help "Set this flag to only check the input transition system for correctness" &= explicit &= name "ts"
}

-- | Defines the arguments for the extension mode.
extensionMode :: MiniCheckArgs
extensionMode = ExtensionMode{ 
  extensions = def &= help "Set this flag to check the existing extensions" &= explicit &= name "extensions"
}

-- | Defines the arguments that can be used with miniCheck
miniCheckModes :: Mode (CmdArgs MiniCheckArgs)
miniCheckModes = cmdArgsMode $ modes [normalMode &= auto, extensionMode]
    &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _AUTHORS)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

{- | Exits with an error when the provided argument (Either type) is left
 otherwise returns the value from Right
-}
exitOnLeft :: Show a => Either a b -> IO b
exitOnLeft (Left err) = do
  print "ERROR:"
  print err
  exitFailure
exitOnLeft (Right result) = return result
  
main :: IO ()
main = do
  mode <- cmdArgsRun miniCheckModes
  case mode of
        NormalMode { tsFilePath = path_ts, ctlFilePath = path_ctl , ts = tsFlag } ->
            parseAndModelCheck path_ts path_ctl tsFlag
        ExtensionMode { extensions = extFlag } ->
            listExtensions extFlag

{- | This is executed when the program is started in the default mode
-}
parseAndModelCheck :: String -> String -> Bool -> IO ()
parseAndModelCheck ts_path ctl_path only_check_ts = do
  -- printf "Reading from path %s and %s %s\n" (ts_path) (ctl_path) (show only_check_ts)
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

{- | This is executed when the program is started in the extension mode
-}
listExtensions :: Bool -> IO ()
listExtensions flag = do
  if flag then do
    putStrLn "The implemented extensions are: "
    exitSuccess
  else do
    putStrLn ""