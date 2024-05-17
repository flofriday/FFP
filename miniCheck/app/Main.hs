module Main where

import System.IO
import TransitionSystem
import Text.Parsec (parse)

main :: IO ()
main = do
  file_handle <- openFile "./app/transition_system.txt" ReadMode
  contents <- hGetContents file_handle
  let ts = parse parseTransitionSystem "" contents
  print ts
  hClose file_handle
