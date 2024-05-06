--In order to run assignment 3, the library QuickCheck is needed. You can install it with:
--
--```
--cabal update
--cabal install --lib QuickCheck
--```
-- if necessary set the according packages

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

getSign :: String -> (String, String)
getSign ('-' : xs) = ("-", xs)
getSign string = ([], string)

integerZeroesStripper :: String -> String
integerZeroesStripper input
  | all (\c -> c < '1' || c > '9') input && elem '0' input = "0"
  | otherwise = sign ++ dropWhile (== '0') number
  where
    (sign, number) = getSign input

-- second build swallows all whitespaces
whiteSpaceParser :: Parse1 Char String
whiteSpaceParser = list (singleWhiteSpaceParser `build` const ' ') `build` const ""

terminalParser :: String -> Parse1 Char String
terminalParser target input
  | target `isPrefixOf` input = [(target, drop (length target) input)]
  | otherwise = none input

-- MARK: Parser --
-- Define a parser for terminals
-- Grammar: <program> ::= PROGRAM <program_name> <statement_seq> .
programParser :: Parse1 Char String
programParser =
  buildNothing (terminalParser "PROGRAM" `follows` whiteSpaceParser `follows` programNameParser)
    `follows` statementSeqParser
    `follows` buildNothing (terminalParser ".")

-- Grammar: <program_name> ::= <upper_char><chardig_seq>
programNameParser :: Parse1 Char String
programNameParser = upperCharParser `follows` charDigSeqParser

-- Parse a single (upper case) character
-- Grammar: <upper_char> ::= A|B|C|...|Z
upperCharParser :: Parse1 Char String
upperCharParser = spot isAsciiUpper `build` ((: []))

-- Parsing a non empty sequence of statements, seperated by semicolon
-- (but no trailing)
-- Grammar: <statement_seq> ::= <statement>|<statement>;<statement_seq>
-- Target:  S ::= S1; S2
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

-- Parses a single statement
-- Grammar: <skip> | <assignment> | <if> | <while> | BEGIN < statement seq > END
statementParser :: Parse1 Char String
statementParser =
  skipParser
    `alt` assignmentParser
    `alt` ifParser
    `alt` whileParser
    `alt` ( buildNothing (terminalParser "BEGIN")
              `follows` whiteSpaceParser -- FIXME: I think the whitespace thing can be ignored
              `follows` statementSeqParser
              `follows` whiteSpaceParser
              `follows` buildNothing (terminalParser "END")
          )

-- Parse the skip keyword
-- Grammar: <skip> ::= SKIP
skipParser :: Parse1 Char String
skipParser = terminalParser "SKIP"

-- Parses an assignment
-- Grammar: <assignment> ::= <variable> = <expr>
-- Target:  V := E
assignmentParser :: Parse1 Char String
assignmentParser =
  variableParser
    `follows` whiteSpaceParser
    `follows` (terminalParser "=" `build` \equalSign -> ":" ++ equalSign)
    `follows` whiteSpaceParser
    `follows` exprParser

-- Parsing the if statement
-- Grammar: <if> ::= IF <pred_expr> THEN <statement> ELSE <statement>
-- Target:  if E then S1 else S2 fi
ifParser :: Parse1 Char String
ifParser =
  (terminalParser "IF" `build` const "if")
    `follows` (whiteSpaceParser `build` const " ")
    `follows` predExprParser
    `follows` (whiteSpaceParser `build` const " ")
    `follows` (terminalParser "THEN" `build` const "then")
    `follows` (whiteSpaceParser `build` const " ")
    `follows` statementParser
    `follows` (whiteSpaceParser `build` const " ")
    `follows` (terminalParser "ELSE" `build` const "else")
    `follows` (whiteSpaceParser `build` const " ")
    `follows` (statementParser `build` (++ " fi"))

-- Parsing the while statement
-- Grammar: WHILE <pred_expr> DO <statement>
-- Target:  while E do S od
whileParser :: Parse1 Char String
whileParser =
  (terminalParser "WHILE" `build` const "while")
    `follows` (whiteSpaceParser `build` const " ")
    `follows` predExprParser
    `follows` (whiteSpaceParser `build` const " ")
    `follows` (terminalParser "DO" `build` const "do")
    `follows` (whiteSpaceParser `build` const " ")
    `follows` (statementParser `build` (++ " od"))

-- Parsing a single expression
-- Grammar: <expr> ::= <variable> | <integer> | <float> | <operator><expr><expr>
-- Target:  V | I | (E1 + E2) | (E1 − E2) | (E1 * E2) | (E1 / E2)
exprParser :: Parse1 Char String
exprParser =
  variableParser
    `alt` integerParser
    `alt` floatParser
    `alt` ( ( operatorParser
                >*> whiteSpaceParser
                >*> exprParser
                >*> whiteSpaceParser
                >*> exprParser
            )
              `build` (\((((operator, _), expr1), _), expr2) -> expr1 ++ operator ++ expr2)
          )

-- Parsing just an operator
-- Grammar: <operator> ::= + | - | * | /
operatorParser :: Parse1 Char String
operatorParser =
  terminalParser "+"
    `alt` terminalParser "-"
    `alt` terminalParser "*"
    `alt` terminalParser "/"

-- Parsing a kind of comparison
-- Grammar: <pred_expr> ::= <relator><expr><expr>
-- Target:  (E1 = E2) | (E1 =/= E2) | (E1 >= E2) | (E1 <= E2)
predExprParser :: Parse1 Char String
predExprParser =
  ( relatorParser
      >*> whiteSpaceParser
      >*> exprParser
      >*> whiteSpaceParser
      >*> exprParser
  )
    `build` (\((((relator, _), expr1), _), expr2) -> expr1 ++ relator ++ expr2)

-- Parsing a comparison operator (aka relator)
-- Grammar: ::= ==|/=|>=|<=
relatorParser :: Parse1 Char String
relatorParser =
  (terminalParser "==" `build` const "=")
    `alt` (terminalParser "/=" `build` const "=/=")
    `alt` terminalParser ">="
    `alt` terminalParser "<="

-- Parses a variable name
-- Grammar: <variable> :== <char><chardig_seq>
variableParser :: Parse1 Char String
variableParser = charParser `follows` charDigSeqParser

-- Parsers a single (lower case) character
charParser :: Parse1 Char String
charParser = spot isAsciiLower `build` ((: []))

-- Parse a possible empty sequence of characters and digits
charDigSeqParser :: Parse1 Char String
charDigSeqParser = list (alt charParser digParser `build` \[a] -> a)

-- Parse a single digit
digParser :: Parse1 Char String
digParser = spot isDigit `build` ((: []))

-- Parse a possibly empty sequece of digits
digSeqParser :: Parse1 Char String
digSeqParser = list (digParser `build` \[a] -> a)

-- Parsing a positve or negative integer
-- Grammar: <integer> ::= <digit><digit seq> | - <digit><digit seq>
integerParser :: Parse1 Char String
integerParser =
  ( (digParser `follows` digSeqParser)
      `alt` (terminalParser "-" `follows` digParser `follows` digSeqParser)
  )
    `build` integerZeroesStripper

-- FIXME: Wait for response from LVA about how this is defined
-- Grammar: <float> ::= <integer> . <digit><digit_seq>
floatParser :: Parse1 Char String
floatParser =
  integerParser
    `follows` terminalParser "."
    `follows` digParser
    `follows` digSeqParser

-- Runs all tests
-- MARK: Tests --
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual testName actual expected =
  if actual == expected
    then putStrLn $ "\x1b[32mpassed\x1b[0m " ++ testName
    else printf "\x1b[31mfailed\x1b[0m %s\n\tExpected: %s\n\tActual: %s\n" testName (show expected) (show actual)

runTests :: IO ()
runTests = do
  assertEqual
    "Invalid Empty program"
    (topLevel1 parser1 "PROGRAMFlo.")
    Nothing
  assertEqual
    "Invalid Empty program with whitespace"
    (topLevel1 parser1 "PROGRAM Flo .")
    Nothing
  assertEqual
    "Single skip program"
    (topLevel1 parser1 "PROGRAMFloSKIP.")
    (Just "SKIP")
  assertEqual
    "Single skip program with whitespace"
    (topLevel1 parser1 "PROGRAM Jojo SKIP.")
    (Just "SKIP")
  assertEqual
    "Invalid small program name"
    (topLevel1 parser1 "PROGRAM flo SKIP.")
    (Nothing)
  assertEqual
    "Triple skip"
    (topLevel1 parser1 "PROGRAM Jojo SKIP;SKIP;SKIP.")
    (Just "SKIP; SKIP; SKIP")
  assertEqual
    "Single assignment"
    (topLevel1 parser1 "PROGRAM Jojo x4 = 123.5.")
    (Just "x4:=123.5")
  assertEqual
    "Invalid malformed float"
    (topLevel1 parser1 "PROGRAM Jojo x4 = -1.-5.")
    (Nothing)
  assertEqual
    "Single if statement"
    (topLevel1 parser1 "PROGRAM Jojo IF <= 4 10 THEN SKIP ELSE y = 14.")
    (Just "if 4<=10 then SKIP else y:=14 fi")
  assertEqual
    "Single while statement with equal"
    (topLevel1 parser1 "PROGRAM Jojo WHILE == x y DO x = + x 1.")
    (Just "while x=y do x:=x+1 od")
  assertEqual
    "Single while statement with unequal"
    (topLevel1 parser1 "PROGRAM Jojo WHILE /= x y DO x = + x 1.")
    (Just "while x=/=y do x:=x+1 od")
  assertEqual
    "Single while statement with greater equal"
    (topLevel1 parser1 "PROGRAM Jojo WHILE >= x y DO x = + x 1.")
    (Just "while x>=y do x:=x+1 od")
  assertEqual
    "Single while statement with less equal"
    (topLevel1 parser1 "PROGRAM Jojo WHILE <= x y DO x = + x 1.")
    (Just "while x<=y do x:=x+1 od")
  assertEqual
    "Multiple assignments"
    (topLevel1 parser1 "PROGRAM Jojo x1=+ 1 2;x2= - 3 4;x3= * 5 6;x4= / 10.5 3.2.")
    (Just "x1:=1+2; x2:=3-4; x3:=5*6; x4:=10.5/3.2")
  assertEqual
    "While with multiple statements in body"
    (topLevel1 parser1 "PROGRAM Jojo WHILE /= x y DO BEGIN x = 44; y=22; z = + 1 2 END.")
    (Just "while x=/=y do x:=44; y:=22; z:=1+2 od")
  assertEqual
    "Multiple high negative integers"
    (topLevel1 parser1 "PROGRAM Jojo x=000000001;x1=-000000001;x2=-000000001000000;x3=-453500454.")
    (Just "x:=1; x1:=-1; x2:=-1000000; x3:=-453500454")
  assertEqual
    "Negative zero integers"
    (topLevel1 parser1 "PROGRAM Jojo x=-0;x1=0.")
    (Just "x:=0; x1:=0")
  assertEqual
    "High precission floats"
    (topLevel1 parser1 "PROGRAM Jojo x=0001.000001;x1=-00020000.0000034404340000434000.")
    (Just "x:=1.000001; x1:=-20000.0000034404340000434000")
  assertEqual
    "Fibonacci"
    (topLevel1 parser1 "PROGRAM Fib n=10; a=0; b=1; i=0; WHILE <= i - n 1 DO BEGIN tmp= + a b; a=b; b=tmp END; return=b.")
    (Just "n:=10; a:=0; b:=1; i:=0; while i<=n-1 do tmp:=a+b; a:=b; b:=tmp od; return:=b")
