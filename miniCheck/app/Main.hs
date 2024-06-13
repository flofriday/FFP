{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

{- ORMOLU_DISABLE -}
import ComputationalTreeLogic
import LinearTemporalLogic
import MiniMM
import MiniMMCompiler
import ModelChecking
import ModelCheckingLTL
import System.Console.CmdArgs
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Parsec (parse)
import TransitionSystem
{- ORMOLU_ENABLE -}

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

data MiniCheckArgs
  = NormalMode {tsFilePath :: FilePath, ctlFilePath :: FilePath, ts :: Bool}
  | MiniMMMode {minimmFilePath :: FilePath, ctlFilePath :: FilePath, dumpTs :: Bool}
  | LtlMode {tsFilePath :: FilePath, ltlFilePath :: FilePath, kBound:: Int}
  | ExtensionMode {extensions :: Bool}
  deriving (Show, Data, Typeable, Eq)

-- | Defines the arguments for the normal mode. This mode is also used it no mode is specified
normalMode :: MiniCheckArgs
normalMode =
  NormalMode
    { tsFilePath = def &= argPos 0 &= typ "TS_FILE_PATH",
      ctlFilePath = def &= argPos 1 &= typ "CTL_FILE_PATH",
      ts = def &= help "Set this flag to only check the input transition system for correctness" &= explicit &= name "ts"
    }

-- | Defines the arguments for the Mini-- mode.
minimmMode :: MiniCheckArgs
minimmMode =
  MiniMMMode
    { minimmFilePath = def &= argPos 2 &= typ "MINIMM_FILE_PATH",
      ctlFilePath = def &= argPos 3 &= typ "CTL_FILE_PATH",
      dumpTs = def &= help "Set this flag to dump the generated TS from the MiniMM" &= explicit &= name "dump-ts"
    }

-- | Defines the arguments for the Mini-- mode.
ltlMode :: MiniCheckArgs
ltlMode =
  LtlMode
    { tsFilePath = def &= argPos 4 &= typ "TS_FILE_PATH",
      ltlFilePath = def &= argPos 5 &= typ "LTL_FILE_PATH",
      kBound = def &= argPos 6 &= typ "K_BOUND"
    }

-- | Defines the arguments for the extension mode.
extensionMode :: MiniCheckArgs
extensionMode =
  ExtensionMode
    { extensions = def &= help "Set this flag to check the existing extensions" &= explicit &= name "extensions"
    }

-- | Defines the arguments that can be used with miniCheck
miniCheckModes :: Mode (CmdArgs MiniCheckArgs)
miniCheckModes =
  cmdArgsMode $
    modes [normalMode &= auto, minimmMode, ltlMode, extensionMode]
      &= verbosityArgs [explicit, name "Verbose", name "V"] []
      &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
      &= summary (_PROGRAM_INFO ++ ", " ++ _AUTHORS)
      &= help _PROGRAM_ABOUT
      &= helpArg [explicit, name "help", name "h"]
      &= program _PROGRAM_NAME

-- | Exits with an error when the provided argument (Either type) is left
-- otherwise returns the value from Right
exitOnLeft :: (Show a) => Either a b -> IO b
exitOnLeft (Left err) = do
  print "ERROR:"
  print err
  exitFailure
exitOnLeft (Right result) = return result

main :: IO ()
main = do
  mode <- cmdArgsRun miniCheckModes
  case mode of
    NormalMode {tsFilePath = path_ts, ctlFilePath = path_ctl, ts = tsFlag} ->
      parseAndModelCheck path_ts path_ctl tsFlag
    MiniMMMode {minimmFilePath = path_mini, ctlFilePath = path_ctl, dumpTs = dump} ->
      parseMiniMMAndCheck path_mini path_ctl dump
    LtlMode {tsFilePath = path_ts, ltlFilePath = path_ltl, kBound = k_bound} ->
      parseAndLtlModelCheck path_ts path_ltl k_bound
    ExtensionMode {extensions = extFlag} ->
      listExtensions extFlag

-- | This is executed when the program is started in the default mode
parseAndModelCheck :: String -> String -> Bool -> IO ()
parseAndModelCheck ts_path ctl_path only_check_ts = do
  -- printf "Reading from path %s and %s %s\n" (ts_path) (ctl_path) (show only_check_ts)
  -- read and parse transition system file
  ts_file <- openFile ts_path ReadMode
  ts_contents <- hGetContents ts_file
  tsystem <- exitOnLeft $ parse parseTransitionSystem ts_path ts_contents
  -- print ts
  hClose ts_file
  if only_check_ts
    then do
      putStrLn "\n************* \n--ts mode was enabled. Therefore we are only checking the transition system. This succeeded therefore we are exiting...\n*************\n"
      exitSuccess
    else do
      -- read and parse ctl file
      ctl_file <- openFile ctl_path ReadMode
      ctl_contents <- hGetContents ctl_file
      ctl <- exitOnLeft $ parse parseComputationalTreeLogic ctl_path ctl_contents
      -- print ctl
      hClose ctl_file
      -- model checking
      let result = modelCheck tsystem ctl
      print result

-- | This is executed if the first extension Mini-- is invoked.
parseMiniMMAndCheck :: String -> String -> Bool -> IO ()
parseMiniMMAndCheck mini_path ctl_path dump = do
  -- Parse and compile the minimm program
  mini_file <- openFile mini_path ReadMode
  mini_contents <- hGetContents mini_file
  mini <- exitOnLeft $ parse parseMiniMM mini_path mini_contents
  tsystem <- exitOnLeft $ compileMiniMM mini

  if dump
    then do
      print tsystem
    else do
      -- Parse the ctl
      ctl_file <- openFile ctl_path ReadMode
      ctl_contents <- hGetContents ctl_file
      ctl <- exitOnLeft $ parse parseComputationalTreeLogic ctl_path ctl_contents

      -- Check it
      let result = modelCheck tsystem ctl
      print result
      hClose mini_file
      hClose ctl_file


-- | This is executed if the second extension Bounded Model checking is invoked.
parseAndLtlModelCheck :: String -> String -> Int -> IO ()
parseAndLtlModelCheck ts_path ltl_path k_bound = do
  -- read and parse transition system file
  ts_file <- openFile ts_path ReadMode
  ts_contents <- hGetContents ts_file
  tsystem <- exitOnLeft $ parse parseTransitionSystem ts_path ts_contents
  -- print ts
  hClose ts_file

  -- read and parse ltl file
  ltl_file <- openFile ltl_path ReadMode
  ltl_contents <- hGetContents ltl_file
  ltl <- exitOnLeft $ parse parseLinearTemporalLogic ltl_path ltl_contents
  print ltl
  hClose ltl_file

  -- model checking
  let result = modelCheckLTL tsystem ltl k_bound
  print result

-- | This is executed when the program is started in the extension mode
listExtensions :: Bool -> IO ()
listExtensions flag = do
  if flag
    then do
      putStrLn "The implemented extensions are: "
      putStrLn "\t Extension 1: Mini--"
      exitSuccess
    else do
      putStrLn ""