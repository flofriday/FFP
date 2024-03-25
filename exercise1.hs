{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.List
import Data.Maybe
import GHC.Num
import GHC.Stack (HasCallStack, callStack, getCallStack)
import GHC.Stack.Types (SrcLoc)
import Text.Printf

{-# HLINT ignore "Use camelCase" #-}
type Nat1 = Integer

type Zaehler = Nat1

type Nenner = Nat1

type RationaleZahl = (Zaehler, Nenner)

type MaxNenner = Nat1

type MaxDifferenz = Nat1

type MaxSummanden = Nat1

type Stammbruchsumme = [Nenner]

-- Task 1  Data types and other stuff ------------------------------------------

-- Priority Queue from the lecture
newtype PQueue a = PQ [a]

emptyPQ = PQ []

is_emptyPQ :: PQueue a -> Bool
is_emptyPQ (PQ []) = True
is_emptyPQ _ = False

enPQ x (PQ pq) = PQ (insert x pq)
  where
    insert x [] = [x]
    insert x r@(e : r')
      | x <= e = x : r
      | otherwise = e : insert x r'

dePQ (PQ []) = error "Priority queue is empty"
dePQ (PQ (_ : xs)) = PQ xs

frontPQ (PQ []) = error "Priority queue is empty"
frontPQ (PQ (x : _)) = x

-- Greedy search implementation from the lecture
search_greedy :: (Foldable t, Ord a) => (a -> t a) -> (a -> Bool) -> a -> [a]
search_greedy succ goal n -- n for node
  =
  search (enPQ n emptyPQ)
  where
    search pq -- pq for priority queue
      | is_emptyPQ pq = []
      | goal (frontPQ pq) = [frontPQ pq]
      | otherwise =
          let m = frontPQ pq
           in search (foldr enPQ emptyPQ (succ m))

-- Task 1 Implementation -------------------------------------------------------

-- TODO: burn a duden and insert random pages from a oxford dictionary.

type Gierig_Node = (RationaleZahl, [Nenner])

-- There is an optimization here for the last case which is also the
-- most expensive to calculate.
rechneNaechster :: Gierig_Node -> [Gierig_Node]
rechneNaechster ((1, n), sol) = [((0, n), sol ++ [n])] -- last case, we can just return n and know that there can be no additonal cases (at least for this greedy implementation)
rechneNaechster ((z, n), sol) = naechster
  where
    naechster = [((restZaehler, restNenner), sol ++ [kanditat])]
    kanditat = head [cn | cn <- [2 ..], cn * z >= n]
    restZaehler = kanditat * z - n
    restNenner = kanditat * n

-- We use the greedy algorithm to calculate the results. We only return the list
-- of denominators, as the rest will aways be 0 if we found a solution.
-- As the start value we pass an empty list and the inital rationalNumber that will get
-- smaller and smaller in each recursive step
gierig :: RationaleZahl -> Stammbruchsumme
gierig rat = summe
  where
    [(_, summe)] = search_greedy rechneNaechster istLoesung initial
    istLoesung ((z, _), _) = z == 0
    initial = (rat, [])

-- Task 2.1 implementation -----------------------------------------------------

-- we branch each time we find a solution:
-- the first option is that we include the found solution in the rekurisve method
-- the second option is that we do not innclude it and look for different solutions
generier :: RationaleZahl -> Nenner -> MaxNenner -> [Stammbruchsumme]
generier (z, n) minN maxN
  | minN > maxN = []
  | kandidates == [] = []
  | z == 0 = []
  | otherwise = ergebnisInkludierend ++ ergebnisExkludierend
  where
    restInkludierend = (generier (restZaehler, restNenner) (kandidat + 1) maxN)
    ergebnisInkludierend =
      if restZaehler /= 0
        then map (kandidat :) restInkludierend
        else [[kandidat]]
    ergebnisExkludierend = generier (z, n) (kandidat + 1) maxN
    kandidates = [cn | cn <- [minN .. maxN], cn * z >= n]
    kandidat = head kandidates
    restZaehler = kandidat * z - n
    restNenner = kandidat * n

gen :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
gen rat maxN = generier rat 2 maxN

-- Task 2.2 implementation -----------------------------------------------------

-- find all Stammbruchsummen with the smallest amount of denominators, i.e. smallest length of list
ga1 :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
ga1 rat maxN = filter kondition kandidaten
  where
    kondition k = length k == kleinsteLän
    kandidaten = gen rat maxN
    kleinsteLän = minimum (map (length) kandidaten)

-- Task 2.3 implementation ------------------------------------------------------

ga2 :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
ga2 rat maxN = filter kondition kandidaten
  where
    kondition k = maximum k == kleinsterGrößterNenner -- 3) find all elements that match
    kleinsterGrößterNenner = minimum (map maximum kandidaten)  -- 2) for each candidate get the largest (last) element and find the one that has the smallest (largest) element
    kandidaten = gen rat maxN -- 1) generate all candidates

-- Task 3 Data and other stuff -------------------------------------------------

data Stack a = Empty | Stk a (Stack a)

empty = Empty

is_empty Empty = True
is_empty _ = False

push x s = Stk x s

pop Empty = error "Stack is empty"
pop (Stk _ s) = s

top Empty = error "Stack is empty"
top (Stk x _) = x

search_dfs succ goal n -- n for node
  =
  (search (push n empty))
  where
    search s -- s for stack
      | is_empty s = []
      | goal (top s) = top s : search (pop s)
      | otherwise =
          let m = top s
           in search (foldr push (pop s) (succ m))

-- Task 3.1 implementation -----------------------------------------------------

type RückNode = (RationaleZahl, Stammbruchsumme, Nat1)

-- rückNachfolger :: RückNode -> [RückNode]
-- rückNachfolger (rest, lös) = ergebnis
--   where
--     ergebnis
--     ober

rs1 :: RationaleZahl -> MaxNenner -> MaxDifferenz -> [Stammbruchsumme]
rs1 rat maxN maxDiff = map (\(_, l, _) -> l) rückSuchLösung
  where
    rückSuchLösung = search_dfs rückNachfolger ziel initial
    ziel ((z, _), _, _) = z == 0
    initial = (rat, [], 2)
    rückNachfolger ((z, n), lös, minN) = ergebnis
      where
        ergebnis =
          if kandidaten == []
            then []
            else [((restZaehler, restNenner), lös ++ [kandidat], kandidat + 1), ((z, n), lös, kandidat + 1)]
        oberGrenze =
          if lös == []
            then maxN
            else min ((minimum lös) + maxDiff) maxN
        kandidaten = [cn | cn <- [minN .. oberGrenze], cn * z >= n]
        kandidat = head kandidaten
        restZaehler = kandidat * z - n
        restNenner = kandidat * n

-- Task 3.2 implementation -----------------------------------------------------

rs2 :: RationaleZahl -> MaxNenner -> MaxSummanden -> [Stammbruchsumme]
rs2 rat maxN maxSum = map (\(_, l, _) -> l) rückSuchLösung
  where
    rückSuchLösung = search_dfs rückNachfolger ziel initial
    ziel ((z, _), _, _) = z == 0
    initial = (rat, [], 2)
    rückNachfolger ((z, n), lös, minN) = ergebnis
      where
        ergebnis =
          if null kandidaten || integerFromInt (length lös) == maxSum
            then []
            else [((restZaehler, restNenner), lös ++ [kandidat], kandidat + 1), ((z, n), lös, kandidat + 1)]
        kandidaten = [cn | cn <- [minN .. maxN], cn * z >= n]
        kandidat = head kandidaten
        restZaehler = kandidat * z - n
        restNenner = kandidat * n

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
  -- Exerise 1 tests --

  assertEqual "gierig 2/3" (gierig (2, 3)) [2, 6]
  assertEqual "gierig 2/5" (gierig (2, 5)) [3, 15]
  assertEqual "gierig 3/7" (gierig (3, 7)) [3, 11, 231]
  assertEqual "gierig 9/20" (gierig (9, 20)) [3, 9, 180]

  assertEqual "gierig 7/15" (gierig (7, 15)) [3, 8, 120]
  assertEqual "gierig 5/8" (gierig (5, 8)) [2, 8]
  assertEqual "gierig 5/12" (gierig (5, 12)) [3, 12]

  -- Exercise 2.1 tests --
  -- FIXME: More tests but most of the assignment cannot be run as it takes too
  -- long
  assertEqual "gen 2/3 max=6" (gen (2, 3) 6) [[2, 6]]
  assertEqual "gen 2/3 max=5" (gen (2, 3) 5) []
  assertContains "gen 2/3 max=10" (gen (2, 3) 6) [2, 6]
  assertEqual "gen 1/3 max=2" (gen (1, 3) 2) []
  assertEqual "gen 2/3 max=15" (gen (2, 3) 15) [[2, 6], [2, 10, 15], [3, 4, 12], [3, 6, 10, 15], [4, 6, 10, 12, 15]]

  -- Exercise 2.2 tests --
  assertEqual "ga1 9/20 max=20" (ga1 (9, 20) 20) [[4, 5]]
  assertEqual "ga1 2/3 max=5" (ga1 (2, 3) 5) []
  assertEqual "ga1 2/3 max=20" (ga1 (2, 3) 20) [[2, 6]]

  -- Exercise 2.3 tests --
  assertEqual "ga2 9/20 max=20" (ga2 (9, 20) 20) (Just [4,5])
  assertEqual "ga2 5/31 max=42" (ga2 (5,31) 42) Nothing
  assertEqual "ga2 2/3 max=5" (ga2 (2, 3) 5) Nothing
  assertEqual "ga2 2/3 max=20" (ga2 (2, 3) 20) (Just [2, 6])
  assertEqual "ga2 2/3 max=20" (ga2 (2, 3) 20) [[2, 6]]

  -- Exercise 3.1 tests --
  assertEqual "rs1 2/3 maxN=10 maxD=4" (rs1 (2, 3) 10 4) [[2, 6]]
  assertEqual "rs1 2/3 maxN=7 maxD=100" (rs1 (2, 3) 7 100) [[2, 6]]
  assertEqual "rs1 2/3 maxN=2 maxD=2" (rs1 (2, 3) 2 2) []
  assertEqual "rs1 2/3 maxN=10 maxD=3" (rs1 (2, 3) 10 3) []
  assertEqual "rs1 2/3 maxN=100 maxD=3" (rs1 (2, 3) 100 3) []
  assertEqual "rs1 2/5 maxN=15 maxD=11" (rs1 (2, 5) 15 11) [[4, 12, 15]]
  assertEqual "rs1 2/5 maxN=15 maxD=10" (rs1 (2, 5) 15 10) []

  -- Exercise 3.2 tests --
  assertEqual "rs2 2/3 maxN=10 maxS=2" (rs2 (2, 3) 10 2) [[2, 6]]
  assertEqual "rs2 2/3 maxN=2 maxS=2" (rs2 (2, 3) 2 2) []
  assertEqual "rs2 2/3 maxN=10 maxS=1" (rs2 (2, 3) 10 1) []
  assertEqual "rs2 2/3 maxN=100 maxS=1" (rs2 (2, 3) 100 1) []
  assertEqual "rs2 2/5 maxN=15 maxS=2" (rs2 (2, 5) 15 2) [[3, 15]]
  assertEqual "rs2 2/5 maxN=15 maxS=1" (rs2 (2, 5) 15 1) []
