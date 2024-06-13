module ModelCheckingLTL (modelCheckLTL, generatePaths, generatePathsFromState, evaluateLtlFormula, Path) where

{- ORMOLU_DISABLE -}
import TransitionSystem
import LinearTemporalLogic

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
{- ORMOLU_ENABLE -}

type Path = [State]

-- | modelChecking: we validate whether all paths starting from the initial states with a
-- bound of k fullfill the formula
modelCheckLTL  :: TransitionSystem -> LtlFormula -> Int -> Bool
modelCheckLTL ts formula k = all satisfiesAllPaths (Set.toList paths)
  where
    paths = generatePaths ts k
    satisfiesAllPaths path = evaluateLtlFormula formula path ts 0 -- 0 is the start state and NOT k

-- | generate all paths from the initial states
generatePaths :: TransitionSystem -> Int -> Set Path
generatePaths ts k = Set.fromList $ concatMap (\state -> generatePathsFromState ts state k) init_states
  where 
    init_states = (Set.toList (initial_states ts))

-- | generate all paths starting fro mthe path argument
generatePathsFromState :: TransitionSystem -> State -> Int -> [Path]
generatePathsFromState ts state 0 = [[state]]
generatePathsFromState ts state n = [state:path | next <- nextStates, path <- generatePathsFromState ts next (n-1)]
  where nextStates = [s2 | (s1, _, s2) <- transition ts, s1 == state]

-- | evaluate whether this formula holds for the given path, ts and k bound
evaluateLtlFormula :: LtlFormula -> Path -> TransitionSystem -> Int -> Bool
evaluateLtlFormula formula path ts i
  | i >= length path = False
  | otherwise = case formula of
      State_True -> True -- trivially true
      AtomicP p -> (p,True) `elem` (label_functions ts Map.! (path !! i))  -- check if the atomic proposition is true at the start, i.e. `i`
      Not f -> not $ evaluateLtlFormula f path ts i -- check if the formula does not hold for the subformula f
      And f1 f2 -> evaluateLtlFormula f1 path ts i && evaluateLtlFormula f2 path ts i -- check if the formula holds for f1 and f2
      O f -> evaluateLtlFormula f path ts (i + 1) -- check if the formula holds the path i+1, i.e. the next state
      -- iterate over each state and check if f2 holds and if for every path before j f1 holds, if this is the case fpr any combination, the until operator holds
      U f1 f2 -> or [evaluateLtlFormula f2 path ts j && all (\k -> evaluateLtlFormula f1 path ts k) [i..(j-1)] | j <- [i..(length path - 1)]] 
      