module Main where

import System.IO
import TransitionSystem
import Text.Parsec (parse)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  checkArgs args
  let ts_path = args !! 0
  --let ctl_path = args !! 1
  file_handle <- openFile ts_path ReadMode
  contents <- hGetContents file_handle
  let ts = parse parseTransitionSystem ts_path contents
  print ts
  hClose file_handle


checkArgs :: [String] -> IO ()
checkArgs args
  | length(args) == 1 = printf "Reading from path %s\n" (args !! 0)
  | otherwise = error "Too few arguments. Please provide at exactly 1 argument."
