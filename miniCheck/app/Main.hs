module Main where

import System.IO

import TransitionSystem

main :: IO ()
main = do
    handle <- openFile "./app/transition_system.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (parseTransitionSystem contents)
    hClose handle