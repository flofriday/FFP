{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.Array
import Data.Maybe
import Text.Printf
import Data.List

-- Task 1 ----------------------------------------------------------------------
type Nat1 = Integer

type Index = (Nat1, Nat1)
data Cell = X | O | Empty

type BinoxxoL = [[Cell]]

type BinoxxoF = Array Index Cell

lengthAsInteger :: [a] -> Integer
lengthAsInteger x = toInteger (length x)


-- Task 2 ----------------------------------------------------------------------
generiereBinoxxoL :: Index -> BinoxxoL -> BinoxxoL
generiereBinoxxoL (rows, cols) cells = cells


generiereBinoxxoF1 :: Index -> [(Index,Cell)] -> BinoxxoF
generiereBinoxxoF1 (rows, cols) cells = array ((1,rows),(1,cols)) cells

generiereBinoxxoF2 :: Index -> [Cell] -> BinoxxoF
generiereBinoxxoF2 (rows, cols) cells = listArray ((1,rows),(1,cols)) cells

generiereBinoxxoF3 :: Index -> [(Index,Cell)] -> BinoxxoF
generiereBinoxxoF3 (rows, cols) cells = accumArray (\a b -> b) Empty ((1, rows), (1, cols)) cells 

-- TestSuite -------------------------------------------------------------------

-- Asserts that two values are equal, otherwise prints an error message.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected =
  if actual == expected
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\tExpected: %s\n\tActual: %s\n" testName (show expected) (show actual)

assertContains :: (Eq a, Show a) => String -> [a] -> a -> IO ()
assertContains testName haystack needle =
  if isJust (find (== needle) haystack)
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\tExpected Containing: %s\n\tActual: %s\n" testName (show needle) (show haystack)

-- Runs all tests
--runTests :: IO ()
--runTests = do
  -- Exerise 1 tests --
--  assertEqual "generiereBinoxxoL 2/3" (generiereBinoxxoF1 (1,3) [(1,0),(2,1),(3,1)]) (array (1,3) [(1,0),(2,1),(3,1)])