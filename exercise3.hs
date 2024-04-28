-- Task 3 ----------------------------------------------------------------------

import Data.Char
import Data.List
import Test.QuickCheck
import Text.Printf

-- MARK: Part 1

-- MARK: Task 1.1
type Nat0 = Integer

type Nat1 = Integer

type Nat2 = Integer

f :: Nat0 -> Nat0
f 0 = 0
f 1 = 1
f n = sum [f n | n <- [0 .. (n - 1)]]

f' :: Nat2 -> Nat1
f' n = 2 ^ (n - 2)

biggerOrEqualTo :: Nat0 -> Nat0 -> Bool
biggerOrEqualTo n lowerLimit = n >= lowerLimit

prop_equationalEquality :: Nat2 -> Property
prop_equationalEquality n = biggerOrEqualTo n 2 ==> f n == f' n

-- MARK: Task 1.2
fib :: Nat0 -> Nat0
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

prop_quotientSmalleThanEpsilon :: Nat2 -> Bool
prop_quotientSmalleThanEpsilon i = actualDifference <= epsilon
  where
    epsilon = 0.2
    fibQuot = fromIntegral (fib i) / fromIntegral (fib (i - 1))
    goldenRatio = (1 + sqrt 5) / 2
    actualDifference = abs (fibQuot - goldenRatio)

-- MARK: Parser
type Parse1 a b = [a] -> [(b, [a])]

topLevel1 :: Parse1 a b -> [a] -> b
topLevel1 parser input =
  case results of
    [] -> error "parse unsuccessful"
    _ -> head results
  where
    results = [found | (found, []) <- parser input]

parser1 :: Parse1 Char (Maybe String)
parser1 input = []

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

list :: Parse1 a b -> Parse1 a [b]
list p =
  (succeed [])
    `alt` ((p >*> list p) `build` (uncurry (:)))

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

-- Custom definitions
follows :: Parse1 Char String -> Parse1 Char String -> Parse1 Char String
follows p1 p2 = (p1 >*> p2) `build` uncurry (++)

buildNothing :: Parse1 Char String -> Parse1 Char String
buildNothing p input = [("", rem) | (x, rem) <- p input]

-- Define a parser for terminals
terminalParser :: [Char] -> Parse1 Char String
terminalParser target input
  | target `isPrefixOf` input = [(target, drop (length target) input)]
  | otherwise = none input

programParser :: Parse1 Char String
programParser = buildNothing (terminalParser "PROGRAM" `follows` programNameParser) `follows` buildNothing (terminalParser ".")

programNameParser :: Parse1 Char String
programNameParser = upperCharParser `follows` charDigSeq

charParser :: Parse1 Char String
charParser = spot isAsciiLower `build` (\a -> [a])

upperCharParser :: Parse1 Char String
upperCharParser = spot isAsciiUpper `build` (\a -> [a])

digParser :: Parse1 Char String
digParser = spot isDigit `build` (\a -> [a])

charDigSeq :: Parse1 Char String
charDigSeq = list (alt charParser digParser `build` \[a] -> a)

--
{- skipParser :: Parse1 Char String
skipParser = terminalParser "SKIP"

statementParser :: Parse1 Char String
statementParser =
  skipParser
    `alt` assignmentParser
    `alt` ifParser
    `alt` whileParser
    `alt` (terminalParser "BEGIN" `follows` statementSeqParser `follows` terminalParser "END") -}

-- topLevel1 parser1 "PROGRAM someName SKIP"

-- Runs all tests
-- MARK : Tests
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
    [("", "")]
