{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

import Data.Array
import Data.List
import Data.Maybe
import Text.Printf

-- Preliminaries

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x : xs) = x `notElem` xs && distinct xs

-- Task 1 ----------------------------------------------------------------------
type Nat1 = Integer

type Index = (Nat1, Nat1)

data Cell = X | O | Empty deriving (Show)

instance Eq Cell where
  (==) X X = True
  (==) O O = True
  (==) Empty Empty = True
  (==) _ _ = False

type BinoxxoL = [[Cell]]

type BinoxxoF = Array Index Cell

lengthAsInteger :: [a] -> Integer
lengthAsInteger x = toInteger (length x)

-- Task 2 ----------------------------------------------------------------------
generiereBinoxxoL :: Index -> BinoxxoL -> BinoxxoL
generiereBinoxxoL (rows, cols) cells = cells

generiereBinoxxoF1 :: Index -> [(Index, Cell)] -> BinoxxoF
generiereBinoxxoF1 (rows, cols) cells = array ((1, 1), (rows, cols)) cells

generiereBinoxxoF2 :: Index -> [Cell] -> BinoxxoF
generiereBinoxxoF2 (rows, cols) cells = listArray ((1, 1), (rows, cols)) cells

-- aggregation function (\a b -> b) makes sure that if an index is defined multiple times,
-- then the last specified value should be kept
generiereBinoxxoF3 :: Index -> [(Index, Cell)] -> BinoxxoF
generiereBinoxxoF3 (rows, cols) cells = accumArray (\a b -> b) Empty ((1, 1), (rows, cols)) cells

-- Task 3 ----------------------------------------------------------------------

listHasEqualXandO :: [Cell] -> Bool
listHasEqualXandO list = amountXs == amountOs
  where
    amountXs = length (filter (== X) list)
    amountOs = length (filter (== O) list)

maxTwoAdjacent :: [Cell] -> Bool
maxTwoAdjacent [] = True
maxTwoAdjacent [x] = True
maxTwoAdjacent [x, y] = True
maxTwoAdjacent (x : y : z : xs)
  | x == y && y == z = False
  | otherwise = True

istWgfL :: BinoxxoL -> Bool
istWgfL grid = wgf1 && wgf2 && wgf3
  where
    rows = grid
    columns = transpose grid
    wgf1 = all listHasEqualXandO rows && all listHasEqualXandO columns
    wgf2 = distinct rows && distinct columns
    wgf3 = all maxTwoAdjacent rows && all maxTwoAdjacent columns

istWgfF :: BinoxxoF -> Bool
istWgfF grid = False

istVollständigL :: BinoxxoL -> Bool
istVollständigL grid = all (Empty `notElem`) grid

istVollstaendigF :: BinoxxoF -> Bool
istVollstaendigF grid = False

-- Task 4 ----------------------------------------------------------------------

-- Task 5 ----------------------------------------------------------------------

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
runTests :: IO ()
runTests = do
  -- Task 2 tests --
  assertEqual
    "generiereBinoxxoL basic case"
    (generiereBinoxxoL (2, 2) [[X, O], [X, Empty]])
    [[X, O], [X, Empty]]
  assertEqual
    "generiereBinoxxoF1 basic case"
    (generiereBinoxxoF1 (2, 2) [((1, 1), X), ((1, 2), O), ((2, 1), X), ((2, 2), Empty)])
    (array ((1, 1), (2, 2)) [((1, 1), X), ((1, 2), O), ((2, 1), X), ((2, 2), Empty)])
  assertEqual
    "generiereBinoxxoF2 basic case"
    (generiereBinoxxoF2 (2, 2) [X, O, X, Empty])
    (array ((1, 1), (2, 2)) [((1, 1), X), ((1, 2), O), ((2, 1), X), ((2, 2), Empty)])
  assertEqual
    "generiereBinoxxoF3 basic case"
    (generiereBinoxxoF3 (2, 2) [((1, 1), X), ((1, 2), O), ((1, 1), O), ((2, 1), X), ((2, 2), Empty)])
    (array ((1, 1), (2, 2)) [((1, 1), O), ((1, 2), O), ((2, 1), X), ((2, 2), Empty)])
  -- Task 3 tests --
  assertEqual
    "istWgfL 2x2 valid1"
    (istWgfL [[X, O], [O, X]])
    True
  assertEqual
    "istWgfL 2x2 valid2"
    (istWgfL [[O, X], [X, O]])
    True
  assertEqual
    "istWgfL 2x2 invalid1"
    (istWgfL [[X, X], [X, O]])
    False
  assertEqual
    "istWgfL 2x2 invalid2"
    (istWgfL [[O, X], [O, O]])
    False
  assertEqual
    "istWgfL 2x2 invalid3"
    (istWgfL [[O, O], [O, O]])
    False
  assertEqual
    "istWgfL 4x4 valid1"
    (istWgfL [[O, O, X, X], [X, O, O, X], [X, X, O, O], [O, X, X, O]])
    True
  assertEqual
    "istWgfL 4x4 valid2"
    (istWgfL [[X, O, X, O], [O, X, O, X], [X, O, O, X], [O, X, X, O]])
    True
  assertEqual
    "istWgfL 4x4 invalid1"
    (istWgfL [[X, O, X, O], [O, X, O, X], [X, O, X, O], [O, X, O, X]])
    False
  assertEqual
    "istWgfL 4x4 invalid2"
    (istWgfL [[X, O, X, O], [O, X, O, X], [X, X, X, O], [O, X, X, O]])
    False
  assertEqual
    "istVollständigL 2x2 valid1"
    (istVollständigL [[O, O], [O, O]])
    True
  assertEqual
    "istVollständigL 2x2 invalid1"
    (istVollständigL [[O, Empty], [O, O]])
    False
  assertEqual
    "istVollständigL 2x2 invalid2"
    (istVollständigL [[O, O], [O, Empty]])
    False
  assertEqual
    "istVollständigL 4x4 valid1"
    (istVollständigL [[X, O, X, O], [O, X, O, X], [X, X, X, O], [O, X, X, O]])
    True
  assertEqual
    "istVollständigL 4x4 invalid1"
    (istVollständigL [[X, O, X, O], [O, X, O, X], [X, X, X, Empty], [O, X, X, O]])
    False
  assertEqual
    "istVollständigL 4x4 invalid2"
    (istVollständigL [[X, O, X, O], [O, X, O, X], [X, X, X, O], [O, X, X, Empty]])
    False
