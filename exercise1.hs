{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import GHC.Num

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

-- There is an optimization here for the last case which is also the
-- most expensive to calculate.
rechneNaechster :: (RationaleZahl, [Nenner]) -> [(RationaleZahl, [Nenner])]
rechneNaechster ((1, n), sol) = [((0, n), sol ++ [n])]
rechneNaechster ((z, n), sol) = naechster
  where
    naechster = [((restZaehler, restNenner), sol ++ [kanditat])]
    kanditat = head [cn | cn <- [2 ..], cn * z >= n]
    restZaehler = kanditat * z - n
    restNenner = kanditat * n

gierig :: RationaleZahl -> Stammbruchsumme
gierig rat = summe
  where
    [(_, summe)] = search_greedy rechneNaechster istLoesung initial
    istLoesung = \((z, _), _) -> z == 0
    initial = (rat, [])

-- Task 2.1 implementation -----------------------------------------------------

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

ga1 :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
ga1 rat maxN = filter kondition kandidaten
  where
    kondition = (\k -> length k == kleinsteLän)
    kandidaten = gen rat maxN
    kleinsteLän = minimum (map (length) kandidaten)

-- Task 2.3 implementation ------------------------------------------------------

ga2 :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
ga2 rat maxN = filter kondition kandidaten
  where
    kondition = \k -> maximum k == kleinsterGrößterNenner
    kleinsterGrößterNenner = minimum (map maximum kandidaten)
    kandidaten = gen rat maxN

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
