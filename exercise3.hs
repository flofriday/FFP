-- Task 3 ----------------------------------------------------------------------
import Data.Array
import Data.Array.Base (IArray (numElements))
import Data.Char
import Data.List
import Data.Maybe
import Data.Maybe (Maybe (Nothing))
import Data.String (String)
import GHC.Integer.GMP.Internals (sqrInteger)
import GHC.Real (reduce)
import GHC.Utils.Binary (Bin)
import Test.QuickCheck
import Text.Printf

-- MARK: Part 1 --

-- MARK: Task 1.1 --
type Nat0 = Integer

type Nat1 = Integer

type Nat2 = Integer

f :: Nat0 -> Nat0
f 0 = 0
f 1 = 1
f n = sum [f n | n <- [0 .. (n - 1)]]

f' :: Nat2 -> Nat1
f' n = 2 ^ (n - 2)

trivial p = classify p "trivial"

-- Limiting the upper bound as otherwise the testing might run for ever
prop_equationalEquality :: Property
prop_equationalEquality = forAll (chooseInteger (2, 20)) $ \n -> trivial (n < 3) $ f n == f' n

-- Run with: quickCheck prop_equationalEquality

-- MARK: Task 1.2 --
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

getQuotient :: Integer -> Double
getQuotient i = actualDifference
  where
    fibQuot = fromIntegral (fib i) / fromIntegral (fib (i - 1))
    goldenRatio = (1 + sqrt 5) / 2
    actualDifference = abs (fibQuot - goldenRatio)

-- Change epsilon this to something smaller so that it only sometimes fails eg: (0.1)
isQuotientSmallerThanEpsilon :: Integer -> Bool
isQuotientSmallerThanEpsilon i = actualDifference <= epsilon
  where
    epsilon = 0.7
    fibQuot = fromIntegral (fib i) / fromIntegral (fib (i - 1))
    goldenRatio = (1 + sqrt 5) / 2
    actualDifference = abs (fibQuot - goldenRatio)

-- Again limiting the upper bound cause we cannot wait till the heat death of
-- the universe, or a stack overflow, whichever occurs first.
-- We report here the histogram of how many high numbers we got. Basically for
-- all numbers 10 till 19 we will catch them in "1x"
prop_quotientSmallerThanEpsilon :: Property
prop_quotientSmallerThanEpsilon = forAll (chooseInteger (2, 30)) $
  \i -> collect (show (i `div` 10) ++ "x") $ isQuotientSmallerThanEpsilon i

-- Run with: quickCheck prop_quotientSmallerThanEpsilon

-- MARK: Task 1.3 --

-- Definitions from exercise 2
type Index = (Nat1, Nat1)

data Cell = X | O | Empty deriving (Show)

instance Eq Cell where
  (==) X X = True
  (==) O O = True
  (==) Empty Empty = True
  (==) _ _ = False

type BinoxxoL = [[Cell]]

type BinoxxoF = Array Index Cell

type BinoxxoFRow = Array Nat1 Cell

generiereBinoxxoF2 :: Index -> [Cell] -> BinoxxoF
generiereBinoxxoF2 (rows, cols) = listArray ((1, 1), (rows, cols))

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x : xs) = x `notElem` xs && distinct xs

listHasEqualXandO :: [Cell] -> Bool
listHasEqualXandO list = amountXs == amountOs
  where
    amountXs = length (filter (== X) list)
    amountOs = length (filter (== O) list)

istWgfF :: BinoxxoF -> Bool
istWgfF arr = wgf1 && wgf2 && wgf3
  where
    ((rowStart, columnStart), (rowSize, columnSize)) = bounds arr
    rows = [[arr ! (i, j) | j <- [columnStart .. columnSize]] | i <- [rowStart .. rowSize]]
    columns = transpose rows
    wgf1 = all listHasEqualXandO rows && all listHasEqualXandO columns
    wgf2 = distinct rows && distinct columns
    wgf3 = all maxTwoAdjacent rows && all maxTwoAdjacent columns

istVollstaendigF :: BinoxxoF -> Bool
istVollstaendigF arr = Empty `notElem` elements
  where
    elements = elems arr

maxPossiblyTwoAdjacent :: [Cell] -> Bool
maxPossiblyTwoAdjacent [] = True
maxPossiblyTwoAdjacent [_] = True
maxPossiblyTwoAdjacent [_, _] = True
maxPossiblyTwoAdjacent (Empty : xs) = maxPossiblyTwoAdjacent xs
maxPossiblyTwoAdjacent (x : y : z : xs)
  | x == y && y == z = False
  | otherwise = maxPossiblyTwoAdjacent (y : z : xs)

maxTwoAdjacent :: [Cell] -> Bool
maxTwoAdjacent [] = True
maxTwoAdjacent [_] = True
maxTwoAdjacent [_, _] = True
maxTwoAdjacent (x : y : z : xs)
  | x == y && y == z = False
  | otherwise = maxTwoAdjacent (y : z : xs)

listPossiblyHasEqualXandO :: [Cell] -> Bool
listPossiblyHasEqualXandO list = amountXs <= length_half && amountOs <= length_half
  where
    amountXs = length (filter (== X) list)
    amountOs = length (filter (== O) list)
    length_half = length list `div` 2

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

inverseOf :: Cell -> Cell
inverseOf X = O
inverseOf O = X

collapseAdjacentDeterminedRowL :: [Cell] -> (Bool, [Cell])
collapseAdjacentDeterminedRowL [] = (False, [])
collapseAdjacentDeterminedRowL [a] = (False, [a])
collapseAdjacentDeterminedRowL [a, b] = (False, [a, b])
collapseAdjacentDeterminedRowL (a : b : Empty : xs)
  | a == b && a /= Empty = (True, a : snd (collapseAdjacentDeterminedRowL (b : inverseOf b : xs)))
  | otherwise = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : Empty : xs)
collapseAdjacentDeterminedRowL (a : Empty : c : xs)
  | a == c && a /= Empty = (True, a : snd (collapseAdjacentDeterminedRowL (inverseOf a : c : xs)))
  | otherwise = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (Empty : c : xs)
collapseAdjacentDeterminedRowL (Empty : b : c : xs)
  | b == c && b /= Empty = (True, inverseOf b : snd (collapseAdjacentDeterminedRowL (b : c : xs)))
  | otherwise = (restModified, Empty : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : c : xs)
collapseAdjacentDeterminedRowL (a : b : c : xs) = (restModified, a : rest)
  where
    (restModified, rest) = collapseAdjacentDeterminedRowL (b : c : xs)

replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll _ _ [] = []
replaceAll a b (x : xs)
  | a == x = b : replaceAll a b xs
  | otherwise = x : replaceAll a b xs

collapseCountDeterminedRow :: [Cell] -> (Bool, [Cell])
collapseCountDeterminedRow row
  | numX >= rowSize `div` 2 = (True, replaceAll Empty O row)
  | numO >= rowSize `div` 2 = (True, replaceAll Empty X row)
  | otherwise = (False, row)
  where
    rowSize = length row
    numX = length (filter (== X) row)
    numO = length (filter (== O) row)

fillRowL :: [Cell] -> (Bool, [Cell])
fillRowL row
  | Empty `elem` row && isCollapsed = (isCollapsed, finalRow)
  | otherwise = (False, row)
  where
    (modifiedAdjecent, newRow) = collapseAdjacentDeterminedRowL row
    (modifiedCount, finalRow) = collapseCountDeterminedRow newRow
    isCollapsed = modifiedAdjecent || modifiedCount

collapseDeterminedRowsL :: BinoxxoL -> Maybe BinoxxoL
collapseDeterminedRowsL board
  | anyBoardModified = Just result
  | otherwise = Nothing
  where
    filledBoard = map fillRowL board
    anyBoardModified = any fst filledBoard
    result = map snd filledBoard

transposeArray :: Array (Nat1, Nat1) a -> Array (Nat1, Nat1) a
transposeArray arr = array transposedBounds [((c, r), arr ! (r, c)) | r <- [rowStart .. rowEnd], c <- [colStart .. colEnd]]
  where
    ((rowStart, colStart), (rowEnd, colEnd)) = bounds arr
    transposedBounds = ((colStart, rowStart), (colEnd, rowEnd))

listOfArrayTo2DArray :: [Array Nat1 Cell] -> Array (Nat1, Nat1) Cell
listOfArrayTo2DArray arrays = array ((1, 1), (toInteger rowCount, toInteger colCount)) indices
  where
    rowCount = length arrays
    colCount = rangeSize (bounds (head arrays))
    indices = [((toInteger r, toInteger c), arrays !! (r - 1) ! toInteger c) | r <- [1 .. rowCount], c <- [1 .. colCount]]

getRowF :: BinoxxoF -> Nat1 -> BinoxxoFRow
getRowF board rowIndex = result
  where
    ((rowStart, colStart), (rowEnd, colEnd)) = bounds board
    row = [board ! (rowIndex, colIndex) | colIndex <- [colStart .. colEnd]]
    result = listArray (rowStart, rowEnd) row

fillRowF :: BinoxxoFRow -> Nat1 -> (Bool, BinoxxoFRow)
fillRowF arrayRow rowId
  | Empty `elem` rowElements && isCollapsed = (isCollapsed, listArray index finalRow)
  | otherwise = (False, arrayRow)
  where
    index = bounds arrayRow
    rowElements = elems arrayRow
    (modifiedAdjecent, newRow) = collapseAdjacentDeterminedRowL rowElements
    (modifiedCount, finalRow) = collapseCountDeterminedRow newRow
    isCollapsed = modifiedAdjecent || modifiedCount

collapseDeterminedRowsF :: BinoxxoF -> Maybe BinoxxoF
collapseDeterminedRowsF board
  | anyBoardModified = Just result
  | otherwise = Nothing
  where
    ((rowStart, colStart), (rowEnd, colEnd)) = bounds board
    filledRows = [fillRowF (getRowF board rowIndex) rowIndex | rowIndex <- [rowStart .. rowEnd]]
    anyBoardModified = any fst filledRows
    result = listOfArrayTo2DArray (map snd filledRows)

collapseDeterminedCellsF :: BinoxxoF -> Maybe BinoxxoF
collapseDeterminedCellsF board
  | Just filledRows <- collapseDeterminedRowsF board,
    Just filledColumns <- collapseDeterminedRowsF (transposeArray filledRows) =
      Just (transposeArray filledColumns)
  | Just filledColumns <- collapseDeterminedRowsF (transposeArray board) =
      Just (transposeArray filledColumns)
  | otherwise = Nothing

loeseSmartF :: BinoxxoF -> Maybe BinoxxoF
loeseSmartF board
  | not (isPossiblyWfgF board) = Nothing
  | istVollstaendigF board = Just board
  | otherwise = result
  where
    filledWithCross = fillFirstEmptyF X board
    continuedWithCross = loeseSmartF filledWithCross
    filledWithCircle = fillFirstEmptyF O board
    continuedWithCircle = loeseSmartF filledWithCircle
    fallback = case (continuedWithCross, continuedWithCircle) of
      (Just a, _) -> Just a
      (_, Just a) -> Just a
      _ -> Nothing
    result = case collapseDeterminedCellsF board of
      (Just filledBoard) -> loeseSmartF filledBoard
      _ -> fallback

createEmptyBinoxxoL :: Int -> BinoxxoL
createEmptyBinoxxoL n = take n (repeat (take n (repeat Empty)))

createEmptyBinoxxoF :: Int -> BinoxxoF
createEmptyBinoxxoF n = listToArray (createEmptyBinoxxoL n)

listToArray :: [[a]] -> Array (Nat1, Nat1) a
listToArray xss = array ((1, 1), (toInteger rowCount, toInteger colCount)) indices
  where
    rowCount = length xss
    colCount = if rowCount > 0 then length (head xss) else 0
    indices = [((toInteger (i + 1), toInteger (j + 1)), xss !! i !! j) | i <- [0 .. rowCount - 1], j <- [0 .. colCount - 1]]

-- MARK: 1.3 Implementation --

-- Given an input size this function creates a board to solve.
-- The input must be a postive even number.
-- A eight of the fields will be X another with O and the rest Empty.
genBoardOfSize :: Int -> Gen BinoxxoF
genBoardOfSize n = fmap (generiereBinoxxoF2 (toInteger n, toInteger n)) cellList
  where
    count = n * n
    oCount = fromIntegral (count `div` 8)
    xCount = count `div` 8
    eCount = count - oCount - xCount
    cellList = shuffle (replicate xCount X ++ replicate oCount O ++ replicate eCount Empty)

genBoard :: Gen BinoxxoF
genBoard = do
  n <- elements [i * 2 | i <- [1 .. 5]]
  genBoardOfSize n

-- We only want to generate solveable instances
genSolveableBoard :: Gen BinoxxoF
genSolveableBoard = genBoard `suchThat` isSolveable
  where
    isSolveable = \b -> isJust (loeseSmartF b)

-- Reports the sizes of the inputs
prop_binoxxoSmartSolve :: Property
prop_binoxxoSmartSolve = forAll genSolveableBoard $ \b -> collect (formatCollect b) $ case loeseSmartF b of
  Just b -> istWgfF b
  Nothing -> error "Generated not solveable input"
  where
    boardSize b = (round (sqrt (fromIntegral (numElements b))))
    formatCollect b = (show (boardSize b)) ++ "x" ++ (show (boardSize b))

-- Run with: quickCheck prop_binoxxoSmartSolve

-- MARK: Task2 --
type Parse1 a b = [a] -> [(b, [a])]

-- FIXME: this does no match the assignment description
topLevel1 :: Parse1 Char (Maybe String) -> [Char] -> Maybe String
topLevel1 parser input =
  case results of
    [] -> Nothing
    _ -> head results
  where
    results = [found | (found, []) <- parser input]

stringMaybeMapper :: (String, [Char]) -> (Maybe String, [Char])
stringMaybeMapper (s, c)
  | s == "" = (Nothing, c)
  | otherwise = (Just s, c)

parser1 :: Parse1 Char (Maybe String)
parser1 input = map stringMaybeMapper (programParser input)

-- A parser that allways fails
none :: Parse1 a b
none _ = []

-- A parser that allways succeeds
succeed :: b -> Parse1 a b
succeed val inp = [(val, inp)]

-- Consuming the single token if it matches the input and adding it to the output
token :: (Eq a) => a -> Parse1 a a
token t (x : xs)
  | t == x = [(t, xs)]
  | otherwise = []
token t [] = []

-- Consuming the single token if it fits the function and adding it to the output
spot :: (a -> Bool) -> Parse1 a a
spot p (x : xs)
  | p x = [(x, xs)]
  | otherwise = []
spot p [] = []

--
list :: Parse1 a b -> Parse1 a [b]
list p =
  succeed []
    `alt` ((p >*> list p) `build` uncurry (:))

--

-- Combining parser alterantives
alt :: Parse1 a b -> Parse1 a b -> Parse1 a b
alt p1 p2 input = p1 input ++ p2 input

-- Combining parsers sequentually
(>*>) :: Parse1 a b -> Parse1 a c -> Parse1 a (b, c)
(>*>) p1 p2 input = [((y, z), rem2) | (y, rem1) <- p1 input, (z, rem2) <- p2 rem1]

build :: Parse1 a b -> (b -> c) -> Parse1 a c
build p f input = [(f x, rem) | (x, rem) <- p input]

-- End of definitions

-- MARK: Custom definitions --
follows :: Parse1 Char String -> Parse1 Char String -> Parse1 Char String
follows p1 p2 = (p1 >*> p2) `build` uncurry (++)

buildNothing :: Parse1 Char String -> Parse1 Char String
buildNothing p input = [("", rem) | (x, rem) <- p input]

singleWhiteSpaceParser :: Parse1 Char String
singleWhiteSpaceParser input
  | " " `isPrefixOf` input || "\n" `isPrefixOf` input = [([], drop 1 input)]
  | otherwise = none input

-- second build swallows all whitespaces
whiteSpaceParser :: Parse1 Char String
whiteSpaceParser = list (singleWhiteSpaceParser `build` const ' ') `build` const ""

-- MARK: Parser --
-- Define a parser for terminals
terminalParser :: String -> Parse1 Char String
terminalParser target input
  | target `isPrefixOf` input = [(target, drop (length target) input)]
  | otherwise = none input

programParser :: Parse1 Char String
programParser =
  buildNothing (terminalParser "PROGRAM" `follows` whiteSpaceParser `follows` programNameParser)
    `follows` statementSeqParser
    `follows` buildNothing (terminalParser ".")

programNameParser :: Parse1 Char String
programNameParser = upperCharParser `follows` charDigSeqParser

-- Parsers a single (lower case) character
charParser :: Parse1 Char String
charParser = spot isAsciiLower `build` ((: []))

upperCharParser :: Parse1 Char String
upperCharParser = spot isAsciiUpper `build` ((: []))

digParser :: Parse1 Char String
digParser = spot isDigit `build` ((: []))

digSeqParser :: Parse1 Char String
digSeqParser = list (digParser `build` \[a] -> a)

charDigSeqParser :: Parse1 Char String
charDigSeqParser = list (alt charParser digParser `build` \[a] -> a)

skipParser :: Parse1 Char String
skipParser = terminalParser "SKIP"

statementSeqParser :: Parse1 Char String
statementSeqParser =
  whiteSpaceParser
    `follows` statementParser
    `follows` whiteSpaceParser
    `follows` ( list
                  ( whiteSpaceParser
                      `follows` ((terminalParser ";") `build` \semicolon -> semicolon ++ " ")
                      `follows` whiteSpaceParser
                      `follows` statementParser
                  )
                  `build` \sl -> concat sl
              )

statementParser :: Parse1 Char String
statementParser =
  skipParser
    `alt` assignmentParser
    `alt` ifParser
    `alt` whileParser
    `alt` (terminalParser "BEGIN" `follows` statementSeqParser `follows` terminalParser "END")

assignmentParser :: Parse1 Char String
assignmentParser =
  variableParser
    `follows` whiteSpaceParser
    `follows` (terminalParser "=" `build` \equalSign -> ":" ++ equalSign)
    `follows` whiteSpaceParser
    `follows` exprParser

variableParser :: Parse1 Char String
variableParser = charParser `follows` charDigSeqParser

exprParser :: Parse1 Char String
exprParser =
  variableParser
    `alt` integerParser
    `alt` floatParser
    `alt` (operatorParser `follows` exprParser `follows` exprParser)

integerParser :: Parse1 Char String
integerParser =
  (digParser `follows` digSeqParser)
    `alt` (terminalParser "-" `follows` digParser `follows` digSeqParser)

-- FIXME: How are floats designed
floatParser :: Parse1 Char String
floatParser = integerParser `follows` terminalParser "." `follows` integerParser

ifParser :: Parse1 Char String
ifParser =
  terminalParser "IF"
    `follows` predExprParser
    `follows` terminalParser "THEN"
    `follows` statementParser
    `follows` terminalParser "ELSE"
    `follows` statementParser

whileParser :: Parse1 Char String
whileParser =
  terminalParser "WHILE"
    `follows` predExprParser
    `follows` terminalParser "DO"
    `follows` statementParser

operatorParser :: Parse1 Char String
operatorParser =
  terminalParser "+"
    `alt` terminalParser "-"
    `alt` terminalParser "*"
    `alt` terminalParser "/"

predExprParser :: Parse1 Char String
predExprParser = relatorParser `follows` exprParser `follows` exprParser

relatorParser :: Parse1 Char String
relatorParser =
  terminalParser "=="
    `alt` terminalParser "/="
    `alt` terminalParser ">="
    `alt` terminalParser "<="

-- topLevel1 parser1 "PROGRAM someName SKIP"

-- Runs all tests
-- MARK: Tests --
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected =
  if actual == expected
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\tExpected: %s\n\tActual: %s\n" testName (show expected) (show actual)

runTests :: IO ()
runTests = do
  -- MARK: Task 2 tests --
  assertEqual
    "programParser \"PROGRAMFlo.\""
    (programParser "PROGRAMFlo.")
    []
  assertEqual
    "programParser \"PROGRAMFloSKIP.\""
    (programParser "PROGRAMFloSKIP.")
    [("SKIP", "")]
  assertEqual
    "programParser \"PROGRAM Flo SKIP.\""
    (programParser "PROGRAM Flo SKIP.")
    [("SKIP", "")]
  assertEqual
    "programParser \"PROGRAM Flo SKIP;SKIP;SKIP.\""
    (programParser "PROGRAM Flo SKIP;SKIP;SKIP.")
    [("SKIP; SKIP; SKIP", "")]
  -- MARK: Top Level tests --
  assertEqual
    "topLevel1 parser1 \"PROGRAMFlo.\""
    (topLevel1 parser1 "PROGRAMFlo.")
    Nothing
  assertEqual
    "topLevel1 parser1 \"PROGRAMFloSKIP.\""
    (topLevel1 parser1 "PROGRAMFloSKIP.")
    (Just "SKIP")
  assertEqual
    "topLevel1 parser1 \"PROGRAM Jojo SKIP.\""
    (topLevel1 parser1 "PROGRAM Jojo SKIP.")
    (Just "SKIP")
  assertEqual
    "topLevel1 parser1 \"PROGRAM Jojo SKIP;SKIP;SKIP.\""
    (topLevel1 parser1 "PROGRAM Jojo SKIP;SKIP;SKIP.")
    (Just "SKIP; SKIP; SKIP")
  assertEqual
    "topLevel1 parser1 \"PROGRAM Jojo SKIP;SKIP;SKIP.\""
    (topLevel1 parser1 "PROGRAM Jojo SKIP;SKIP;SKIP.")
    (Just "SKIP; SKIP; SKIP")
  assertEqual
    "topLevel1 parser1 \"PROGRAM JoJo x4 = 123.5.\""
    (topLevel1 parser1 "PROGRAM Jojo x4 = 123.5.")
    (Just "x4:=123.5")
