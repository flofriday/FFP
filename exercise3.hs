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

-- Runs all tests
-- MARK: Tests
-- runTests :: IO ()
-- runTests = do
-- MARK: Task 3 tests --
--  assertEqual
--    "generiereBinoxxoL basic case"
--    (generiereBinoxxoL (2, 2) [[X, O], [X, Empty]])
--    [[X, O], [X, Empty]]
