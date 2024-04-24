-- Task 3 ----------------------------------------------------------------------
import Test.QuickCheck

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
type Parse a b = [a] -> [(b,[a])]

topLevel :: Parse a b -> [a] -> b
topLevel p input
  = case results of
    [] -> error "parse unsuccessful"
    _ -> head results
  where
    results = [ found | (found, []) <- p input ]

parser :: Parse Char (Maybe String)


none :: Parse a b
none _ = []

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]

token :: Eq a => a -> Parse a a
token t (x:xs)
  | t == x = [(t,xs)]
  | otherwise = []
token t [] = []

spot :: (a -> Bool) -> Parse a a
spot p (x:xs)
  | p x = [(x,xs)]
  | otherwise = []
spot p [] = []

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 input = p1 input ++ p2 input

(>*>) :: Parse a b -> Parse a c -> Parse a (b,c)
(>*>) p1 p2 input = [((y,z),rem2) | (y,rem1) <- p1 input,(z,rem2) <- p2 rem1 ]

build :: Parse a b -> (b -> c) -> Parse a c
build p f input = [ (f x,rem) | (x,rem) <- p input ]


-- topLevel parser "PROGRAM someName SKIP"

-- Runs all tests
-- MARK: Tests
-- runTests :: IO ()
-- runTests = do
-- MARK: Task 3 tests --
--  assertEqual
--    "generiereBinoxxoL basic case"
--    (generiereBinoxxoL (2, 2) [[X, O], [X, Empty]])
--    [[X, O], [X, Empty]]
