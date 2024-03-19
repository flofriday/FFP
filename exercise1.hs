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

-- succCoins (r,p) = [ (r-c,c:p) | c <- coins, r-c >= 0 ]

calcSuccessor :: (RationaleZahl, [Nenner]) -> [(RationaleZahl, [Nenner])]
calcSuccessor ((z, n), sol) = take 1 [((cn * z - n, cn * n), sol ++ [cn]) | cn <- [2 ..], cn * z >= n]

gierig_search :: RationaleZahl -> [(RationaleZahl, [Nenner])]
gierig_search rat = search_greedy calcSuccessor isSolution initial
  where
    -- nextSuccessor \((z, n), sol) -> [((cn * z - n, cn * n), cn : sol) | cn <- [1 ..], cn * z >= n]
    isSolution = \((z, _), _) -> z == 0
    initial = (rat, [])

gierig :: RationaleZahl -> Stammbruchsumme
gierig rat = result
  where
    [(_, result)] = gierig_search rat