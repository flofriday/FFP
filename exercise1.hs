{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

-- Task 2.1 implementation -------------------------------------------------------

generier :: RationaleZahl -> Nenner -> MaxNenner -> [Stammbruchsumme]
generier (z, n) minN maxN
  | n > maxN = []
  | minN > maxN = []
  | kandidates == [] = []
  | z == 1 = [[n]]
  | otherwise = ergebnisInkludierend ++ ergebnisExkludierend
  where
    restInkludierend = (generier (restZaehler, restNenner) (kandidat + 1) maxN)
    ergebnisInkludierend =
      if restInkludierend /= []
        then map (kandidat :) restInkludierend
        else []
    ergebnisExkludierend = generier (z, n) (kandidat + 1) maxN
    kandidates = [cn | cn <- [minN .. maxN], cn * z >= n]
    kandidat = head kandidates
    restZaehler = kandidat * z - n
    restNenner = kandidat * n

gen :: RationaleZahl -> MaxNenner -> [Stammbruchsumme]
gen rat maxN = generier rat 2 maxN
