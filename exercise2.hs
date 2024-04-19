import Data.Array
import Data.List
import Data.Maybe
import Distribution.Simple.Build (repl)
import Text.Printf

-- Preliminaries

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x : xs) = x `notElem` xs && distinct xs

-- Task 1 ----------------------------------------------------------------------
type Nat1 = Integer

type Index = (Nat1, Nat1)

data Cell = X | O | Empty deriving (Show)

-- FIXME: There might be some changes on how they are defined so that they are
-- more testable.
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
maxTwoAdjacent [_] = True
maxTwoAdjacent [_, _] = True
maxTwoAdjacent (x : y : z : xs)
  | x == y && y == z = False
  | otherwise = maxTwoAdjacent (y : z : xs)

istWgfL :: BinoxxoL -> Bool
istWgfL grid = wgf1 && wgf2 && wgf3
  where
    rows = grid
    columns = transpose grid
    wgf1 = all listHasEqualXandO rows && all listHasEqualXandO columns
    wgf2 = distinct rows && distinct columns
    wgf3 = all maxTwoAdjacent rows && all maxTwoAdjacent columns

istWgfF :: BinoxxoF -> Bool
istWgfF arr = wgf1 && wgf2 && wgf3
  where
    ((rowStart, columnStart), (rowSize, columnSize)) = bounds arr
    rows = [[arr ! (i, j) | j <- [columnStart .. columnSize]] | i <- [rowStart .. rowSize]]
    columns = transpose rows
    wgf1 = all listHasEqualXandO rows && all listHasEqualXandO columns
    wgf2 = distinct rows && distinct columns
    wgf3 = all maxTwoAdjacent rows && all maxTwoAdjacent columns

istVollständigL :: BinoxxoL -> Bool
istVollständigL grid = all (Empty `notElem`) grid

istVollstaendigF :: BinoxxoF -> Bool
istVollstaendigF arr = Empty `notElem` elements
  where
    elements = elems arr

-- Task 4 ----------------------------------------------------------------------

-- Task 4 (Lists)

-- Fills the first empty cell (and only the first cell) with the given value.
fillFirstEmptyInRowL :: Cell -> [Cell] -> [Cell]
fillFirstEmptyInRowL _ [] = []
fillFirstEmptyInRowL cell (Empty : fields) = cell : fields
fillFirstEmptyInRowL cell (field : fields) = field : fillFirstEmptyInRowL cell fields

-- Fills the first empty field with the cell type specified.
fillFirstEmptyL :: Cell -> BinoxxoL -> BinoxxoL
fillFirstEmptyL Empty _ = error "What are you doing?"
fillFirstEmptyL cell (row : rows)
  | any (== Empty) row = fillFirstEmptyInRowL cell row : rows
  | otherwise = row : fillFirstEmptyL cell rows

-- A version of listHasEqualXandO that respects empty fields which might become
-- X or O in the future.
listPossiblyHasEqualXandO :: [Cell] -> Bool
listPossiblyHasEqualXandO list = amountXs <= length_half && amountOs <= length_half
  where
    amountXs = length (filter (== X) list)
    amountOs = length (filter (== O) list)
    length_half = length list `div` 2

-- A version of maxTwoAdjacent that respects empty fields which might become
-- X or O in the future.
maxPossiblyTwoAdjacent :: [Cell] -> Bool
maxPossiblyTwoAdjacent [] = True
maxPossiblyTwoAdjacent [_] = True
maxPossiblyTwoAdjacent [_, _] = True
maxPossiblyTwoAdjacent (Empty : xs) = maxPossiblyTwoAdjacent xs
maxPossiblyTwoAdjacent (x : y : z : xs)
  | x == y && y == z = False
  | otherwise = maxPossiblyTwoAdjacent (y : z : xs)

-- A version which is a more relaxed istWgf but respects that some fields are
-- empty and therefore can be either case.
isPossiblyWfgL :: BinoxxoL -> Bool
isPossiblyWfgL grid = wgf1 && wgf2 && wgf3
  where
    rows = grid
    columns = transpose grid
    wgf1 = all listPossiblyHasEqualXandO rows && all listPossiblyHasEqualXandO columns
    wgf2 = distinct (filter (notElem Empty) rows) && distinct (filter (notElem Empty) columns)
    wgf3 = all maxPossiblyTwoAdjacent rows && all maxPossiblyTwoAdjacent columns

-- This solution iterates over all empty fields, randomly set's one of the two
-- possibilities and and verifies it and uses some backtracking in case some
-- errors only come up later
loeseNaivL :: BinoxxoL -> Maybe BinoxxoL
loeseNaivL board
  | not (isPossiblyWfgL board) = Nothing
  | istVollständigL board = Just board
  | otherwise = result
  where
    filledWithCross = fillFirstEmptyL X board
    continuedWithCross = loeseNaivL filledWithCross
    filledWithCircle = fillFirstEmptyL O board
    continuedWithCircle = loeseNaivL filledWithCircle
    result = case (continuedWithCross, continuedWithCircle) of
      (Just a, _) -> Just a
      (_, Just a) -> Just a
      _ -> Nothing

-- Task 4 (Arrays)

isPossiblyWfgF :: BinoxxoF -> Bool
isPossiblyWfgF arr = wgf1 && wgf2 && wgf3
  where
    ((rowStart, columnStart), (rowSize, columnSize)) = bounds arr
    rows = [[arr ! (i, j) | j <- [columnStart .. columnSize]] | i <- [rowStart .. rowSize]]
    columns = transpose rows
    wgf1 = all listPossiblyHasEqualXandO rows && all listPossiblyHasEqualXandO columns
    wgf2 = distinct (filter (notElem Empty) rows) && distinct (filter (notElem Empty) columns)
    wgf3 = all maxPossiblyTwoAdjacent rows && all maxPossiblyTwoAdjacent columns

findFirstEmptyIndexF :: BinoxxoF -> Maybe Index
findFirstEmptyIndexF arr =
  case emptyElements of
    [] -> Nothing
    ((row, column), _) : _ -> Just (row, column)
  where
    emptyElements = filter (\((row, column), cell) -> cell == Empty) (assocs arr)

fillFirstEmptyF :: Cell -> BinoxxoF -> BinoxxoF
fillFirstEmptyF Empty _ = error "What are you doing?"
fillFirstEmptyF cell arr =
  case index of
    Nothing -> error "WTF"
    Just index -> arr // [(index, cell)]
  where
    index = findFirstEmptyIndexF arr

loeseNaivF :: BinoxxoF -> Maybe BinoxxoF
loeseNaivF board
  | not (isPossiblyWfgF board) = Nothing
  | istVollstaendigF board = Just board
  | otherwise = result
  where
    filledWithCross = fillFirstEmptyF X board
    continuedWithCross = loeseNaivF filledWithCross
    filledWithCircle = fillFirstEmptyF O board
    continuedWithCircle = loeseNaivF filledWithCircle
    result = case (continuedWithCross, continuedWithCircle) of
      (Just a, _) -> Just a
      (_, Just a) -> Just a
      _ -> Nothing

-- Task 5 ----------------------------------------------------------------------

-- Somtimes we need to invert a cell
inverseOf :: Cell -> Cell
inverseOf X = O
inverseOf O = X

-- Rule three states that only two X and two O's can be adjacent, from that rule
-- it is quite easy to determine that some cells need to be filled.
-- Currently this implementation does the following filling inside a row
-- (alsways looking at the first 3 elements and iterating over it.)
-- Examples:
--    X X Empty ==> X X O
--    X Empty X ==> X O X
--    Empty X X ==> O X X
collapseAdjacentDeterminedRowL :: [Cell] -> (Bool, [Cell])
collapseAdjacentDeterminedRowL [] = (False, [])
collapseAdjacentDeterminedRowL [a] = (False, [a])
collapseAdjacentDeterminedRowL [a, b] = (False, [a, b])
-- The Rule: X X Empty ==> X X O
--           O O Empty ==> O O X
collapseAdjacentDeterminedRowL (a : b : Empty : xs)
  | a == b && a /= Empty = (True, a : snd (collapseAdjacentDeterminedRowL (b : inverseOf b : xs)))
  | otherwise = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : Empty : xs)

-- The Rule: X Empty X ==> X O X
--           O Empty O ==> O X O
collapseAdjacentDeterminedRowL (a : Empty : c : xs)
  | a == c && a /= Empty = (True, a : snd (collapseAdjacentDeterminedRowL (inverseOf a : c : xs)))
  | otherwise = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (Empty : c : xs)

-- The Rule: Empty X X ==> O X X
--           Empty O O ==> X O O
collapseAdjacentDeterminedRowL (Empty : b : c : xs)
  | b == c && b /= Empty = (True, inverseOf b : snd (collapseAdjacentDeterminedRowL (b : c : xs)))
  | otherwise = (restModified, Empty : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : c : xs)

-- No rule to apply, just keep the recursion alive
collapseAdjacentDeterminedRowL (a : b : c : xs) = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : c : xs)

replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll _ _ [] = []
replaceAll a b (x : xs)
  | a == x = b : replaceAll a b xs
  | otherwise = x : replaceAll a b xs

-- The count of X and O must be equal in each row. Therefore if one of them
-- already reached there max, we can fill all empty cells with the other one.
-- FIXME: either use it or and change it's signature
collapseCountDeterminedRow :: [Cell] -> (Bool, [Cell])
collapseCountDeterminedRow row
  | numX >= rowSize `div` 2 = (True, replaceAll Empty O row)
  | numO >= rowSize `div` 2 = (True, replaceAll Empty X row)
  | otherwise = (False, row)
  where
    rowSize = length row
    numX = length (filter (== X) row)
    numO = length (filter (== O) row)

-- "Nothing" when no new cells have been filled
-- Returns "Just BinoxxoL" when the board has been changed
collapseDeterminedRowsL :: BinoxxoL -> Maybe BinoxxoL
collapseDeterminedRowsL board
  | anyBoardModified = Just result
  | otherwise = Nothing
  where
    -- FIXME: God I hate this code
    fillRow row =
      if any (== Empty) row
        then case collapseAdjacentDeterminedRowL row of
          (modifiedAdjecent, newRow) -> case collapseCountDeterminedRow newRow of
            (modifiedCount, finalRow) -> (modifiedAdjecent || modifiedCount, finalRow)
        else (False, row)
    filledBoard = map fillRow board
    anyBoardModified = any fst filledBoard
    result = map snd filledBoard

-- We apply all collapse rules once on all rows and than on all columns by
-- transposing the grid first.
collapseDeterminedCellsL :: BinoxxoL -> Maybe BinoxxoL
-- FIXME: God this code is ugly
collapseDeterminedCellsL board = case collapseDeterminedRowsL board of
  (Just filledRows) -> case collapseDeterminedRowsL (transpose filledRows) of
    (Just filledColumns) -> Just (transpose filledColumns)
    _ -> Just filledRows
  _ -> case collapseDeterminedRowsL (transpose board) of
    (Just filledColumns) -> Just (transpose filledColumns)
    _ -> Nothing

-- Optimizatino 1: Filling constraind cells
-- We can deterministically fill some cells since there is just one allowed
-- solution. For example with X, X, Empty we know that only O is allowed in the
-- Empty field. (From 2.99s to 2.47)
loeseSmartL :: BinoxxoL -> Maybe BinoxxoL
loeseSmartL board
  | not (isPossiblyWfgL board) = Nothing
  | istVollständigL board = Just board
  | otherwise = result
  where
    filledWithCross = fillFirstEmptyL X board
    continuedWithCross = loeseSmartL filledWithCross
    filledWithCircle = fillFirstEmptyL O board
    continuedWithCircle = loeseSmartL filledWithCircle
    fallback = case (continuedWithCross, continuedWithCircle) of
      (Just a, _) -> Just a
      (_, Just a) -> Just a
      _ -> Nothing
    result = case collapseDeterminedCellsL board of
      (Just filledBoard) -> loeseSmartL filledBoard
      _ -> fallback

-- Optimierungidee: Ungerade Anzahlen an Spalten/Reihen ist immer nicht lösbar

-- NOTE: Performance with dynamischen feldern eventuell besser
-- Es muss aber nicht extrem performant sein.

-- Schrittweise erklären wie die naive auf die performante kommt, nicht strikt
-- mit funktionalen perlen aber doch erklärbar.

-- TestSuite -------------------------------------------------------------------

-- Asserts that two values are equal, otherwise prints an error message.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected =
  if actual == expected
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\tExpected: %s\n\tActual: %s\n" testName (show expected) (show actual)

type BinoxxoLSolver = BinoxxoL -> Maybe BinoxxoL

-- Verifies that in each row the originally filled out cells still exist.
containsOriginalRow :: ([Cell], [Cell]) -> Bool
containsOriginalRow ([], []) = True
containsOriginalRow (a : as, b : bs) =
  if a /= b && a /= Empty
    then False
    else containsOriginalRow (as, bs)

-- Verifies that all the originally filled out cells still exist.
containsOriginalCellsL :: BinoxxoL -> BinoxxoL -> Bool
containsOriginalCellsL original solution = all containsOriginalRow (zip original solution)

-- Verifies that the solver
--  1) finds a solution
--  2) which is "wohlgeformt"
--  3) keeps all original fields
-- If any of the contraints are violated it prints an error message to stdout.
assertCorrectSolutionL :: String -> BinoxxoLSolver -> BinoxxoL -> IO ()
assertCorrectSolutionL testName solver input =
  if correct
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\t%s\n" testName "Something went wrong..."
  where
    returned = solver input
    (Just solution) = returned
    correct = isJust returned && istWgfL solution && containsOriginalCellsL input solution

-- Runs all tests

{- ORMOLU_DISABLE -}

-- The binoxxo 1 example from the assignment
assignmentBinoxxo1L =
  [ [Empty, Empty, Empty,   X  , Empty,   X  , Empty,   O  , Empty, Empty],
    [Empty, Empty,   O  , Empty, Empty,   X  , Empty, Empty, Empty,   X  ],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty,   X  ],
    [Empty, Empty,   O  , Empty, Empty, Empty, Empty, Empty,   O  , Empty],
    [Empty,   X  , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty,   X  , Empty,   X  , Empty, Empty, Empty],
    [Empty, Empty, Empty,   X  , Empty, Empty,   X  , Empty,   X  , Empty],
    [Empty,   O  ,   O  , Empty, Empty,   O  , Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty,   X  ,   X  , Empty, Empty, Empty, Empty],
    [Empty,   O  , Empty, Empty, Empty, Empty, Empty, Empty,   O  , Empty]
  ]


-- The binoxxo 2 example from the assignment
assignmentBinoxxo2L =
  [ [Empty, Empty, Empty,   X  , Empty,   O  , Empty,   O  , Empty,   O  ],
    [Empty, Empty, Empty, Empty,   O  , Empty, Empty, Empty, Empty, Empty],
    [  X  ,   O  , Empty, Empty, Empty, Empty, Empty,   O  , Empty, Empty],
    [Empty, Empty,   X  ,   X  , Empty, Empty, Empty,   O  ,   X  , Empty],
    [Empty, Empty, Empty, Empty, Empty,   X  , Empty, Empty, Empty, Empty],
    [  X  , Empty, Empty, Empty, Empty, Empty,   O  , Empty, Empty, Empty],
    [Empty,   O  , Empty, Empty, Empty,   X  , Empty, Empty, Empty, Empty],
    [Empty, Empty,   O  , Empty, Empty, Empty, Empty,   O  , Empty, Empty],
    [  X  , Empty, Empty, Empty, Empty,   X  , Empty, Empty, Empty,   X  ],
    [Empty,   O  , Empty,   O  , Empty, Empty,   X  , Empty, Empty, Empty]
  ]
{- ORMOLU_ENABLE -}

onlineBinoxxo1L =
  [ [X, O, O, X, O, X, O, O, X, O, X, X],
    [O, O, X, O, X, X, O, O, X, X, O, X],
    [X, X, O, X, O, O, X, X, O, X, O, O],
    [X, O, O, X, O, X, X, O, X, O, X, O],
    [O, X, X, O, X, O, O, X, X, O, O, X],
    [O, X, X, O, O, X, O, X, O, X, O, X],
    [X, O, O, X, X, O, X, O, O, X, X, O],
    [O, O, X, O, O, X, O, X, X, O, X, X],
    [X, X, O, X, X, O, X, O, O, X, O, O],
    [O, O, X, X, O, O, X, X, O, X, O, X],
    [O, X, X, O, X, X, O, O, X, O, X, O],
    [X, X, O, O, X, O, X, X, O, O, X, O]
  ]

-- Creates an empty Binoxxo of the dimension nxn
createEmptyBinoxxoL :: Int -> BinoxxoL
createEmptyBinoxxoL n = (take n (repeat (take n (repeat Empty))))

listToArray :: [[a]] -> Array (Nat1, Nat1) a
listToArray xss = array ((1, 1), (toInteger rowCount, toInteger colCount)) indices
  where
    rowCount = length xss
    colCount = if rowCount > 0 then length (head xss) else 0
    indices = [((toInteger (i + 1), toInteger (j + 1)), xss !! i !! j) | i <- [0 .. rowCount - 1], j <- [0 .. colCount - 1]]

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

  -- Task 3 tests Lists--
  assertEqual
    "maxTwoAdjecent invalid three O at the end"
    (maxTwoAdjacent [X, O, X, O, O, O])
    False
  assertEqual
    "maxTwoAdjacent valid long row"
    (maxTwoAdjacent [X, O, X, O, X, O, O, X, X, O])
    True
  assertEqual
    "maxPossiblyTwoAdjecent invalid three O at the end"
    (maxTwoAdjacent [X, O, X, O, O, O])
    False
  assertEqual
    "maxPossiblyTwoAdjacent valid many emptys"
    (maxPossiblyTwoAdjacent [X, O, X, Empty, Empty, Empty, Empty, O, O, X])
    True
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
  -- Task 3 tests Fields/Arrays--
  assertEqual
    "istWgfF 2x2 valid1"
    (istWgfF (listArray ((1, 1), (2, 2)) [X, O, O, X]))
    True
  assertEqual
    "istWgfF 2x2 valid2"
    (istWgfF (listArray ((1, 1), (2, 2)) [O, X, X, O]))
    True
  assertEqual
    "istWgfF 2x2 invalid1"
    (istWgfF (listArray ((1, 1), (2, 2)) [X, X, X, O]))
    False
  assertEqual
    "istWgfF 2x2 invalid2"
    (istWgfF (listArray ((1, 1), (2, 2)) [O, X, O, O]))
    False
  assertEqual
    "istWgfF 2x2 invalid3"
    (istWgfF (listArray ((1, 1), (2, 2)) [O, O, O, O]))
    False
  assertEqual
    "istWgfF 4x4 valid1"
    (istWgfF (listArray ((1, 1), (4, 4)) [O, O, X, X, X, O, O, X, X, X, O, O, O, X, X, O]))
    True
  assertEqual
    "istWgfF 4x4 valid2"
    (istWgfF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, O, O, X, O, X, X, O]))
    True
  assertEqual
    "istWgfF 4x4 invalid1"
    (istWgfF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, O, X, O, O, X, O, X]))
    False
  assertEqual
    "istWgfF 4x4 invalid2"
    (istWgfF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, X, X, O, O, X, X, O]))
    False
  assertEqual
    "istVollstaendigF 2x2 valid1"
    (istVollstaendigF (listArray ((1, 1), (2, 2)) [O, O, O, O]))
    True
  assertEqual
    "istVollstaendigF 2x2 invalid1"
    (istVollstaendigF (listArray ((1, 1), (2, 2)) [O, Empty, O, O]))
    False
  assertEqual
    "istVollstaendigF 2x2 invalid2"
    (istVollstaendigF (listArray ((1, 1), (2, 2)) [O, O, O, Empty]))
    False
  assertEqual
    "istVollstaendigF 4x4 valid1"
    (istVollstaendigF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, X, X, O, O, X, X, O]))
    True
  assertEqual
    "istVollstaendigF 4x4 invalid1"
    (istVollstaendigF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, X, X, Empty, O, X, X, O]))
    False
  assertEqual
    "istVollstaendigF 4x4 invalid2"
    (istVollstaendigF (listArray ((1, 1), (4, 4)) [X, O, X, O, O, X, O, X, X, X, X, O, O, X, X, Empty]))
    False
  -- Task 4 tests --
  assertEqual
    "loeseNaivL 2x2 valid from empty"
    (loeseNaivL [[Empty, Empty], [Empty, Empty]])
    (Just [[X, O], [O, X]])
  assertEqual
    "loeseNaivL 2x2 invalid all x"
    (loeseNaivL [[X, X], [X, X]])
    (Nothing)
  assertEqual
    "loeseNaivL 2x2 invalid all x"
    (loeseNaivL [[X, X], [X, X]])
    (Nothing)
  assertEqual
    "loeseNaivL 4x4 valid two missing"
    (loeseNaivL [[O, O, X, X], [X, O, O, X], [X, X, O, O], [Empty, X, Empty, O]])
    (Just [[O, O, X, X], [X, O, O, X], [X, X, O, O], [O, X, X, O]])
  assertEqual
    "loeseNaivF 2x2 two Empty"
    (loeseNaivF (listArray ((1, 1), (2, 2)) [O, X, Empty, Empty]))
    (Just (array ((1, 1), (2, 2)) [((1, 1), O), ((1, 2), X), ((2, 1), X), ((2, 2), O)]))
  assertEqual
    "loeseNaivF 2x2 nothing Empty"
    (loeseNaivF (listArray ((1, 1), (2, 2)) [O, X, X, O]))
    (Just (array ((1, 1), (2, 2)) [((1, 1), O), ((1, 2), X), ((2, 1), X), ((2, 2), O)]))
  assertEqual
    "loeseNaivF 2x2 invalid - all X"
    (loeseNaivF (listArray ((1, 1), (2, 2)) [X, X, X, X]))
    (Nothing)
  assertEqual
    "maxPossiblyTwoAjacent with 4 adjecent Empty"
    (maxPossiblyTwoAdjacent [X, Empty, Empty, Empty, Empty, X])
    (True)
  assertCorrectSolutionL
    "loeseNaivL 4x4 all empty (might take a while)"
    loeseNaivL
    (createEmptyBinoxxoL 4)
  assertCorrectSolutionL
    "loeseNaivL 6x6 all empty (might take a while)"
    loeseNaivL
    (createEmptyBinoxxoL 6)
  assertCorrectSolutionL
    "loeseNaivL Binoxxo1 from assignment"
    loeseNaivL
    assignmentBinoxxo1L
  assertCorrectSolutionL
    "loeseNaivL Binoxxo2 from assignment"
    loeseNaivL
    assignmentBinoxxo2L
  -- Task 5 tests --
  assertCorrectSolutionL
    "loeseSmartL 4x4 all empty"
    loeseSmartL
    (createEmptyBinoxxoL 4)
  assertCorrectSolutionL
    "loeseSmartL 6x6 all empty"
    loeseSmartL
    (createEmptyBinoxxoL 6)
  assertCorrectSolutionL
    "loeseSmartL 8x8 all empty"
    loeseSmartL
    (createEmptyBinoxxoL 8)
  assertCorrectSolutionL
    "loeseSmartL 10x10 all empty (low-key flex)"
    loeseSmartL
    (createEmptyBinoxxoL 10)
  assertCorrectSolutionL
    "loeseSmartL 12x12 all empty (not so low-key flex)"
    loeseSmartL
    (createEmptyBinoxxoL 12)
  assertCorrectSolutionL
    "loeseSmartL Binoxxo1 from assignment"
    loeseSmartL
    assignmentBinoxxo1L
  assertCorrectSolutionL
    "loeseSmartL Binoxxo2 from assignment"
    loeseSmartL
    assignmentBinoxxo2L
