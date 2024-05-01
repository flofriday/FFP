module Main where

import System.IO

import TransitionSystem

main :: IO ()
main = do
    handle <- openFile "./app/transition_system.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (parserTransitionSystem contents)
    hClose handle