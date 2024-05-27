module ModelChecking (modelCheck, satFun) where
{- ORMOLU_DISABLE -}
import TransitionSystem
import ComputationalTreeLogic

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
{- ORMOLU_ENABLE -}

-- modelChecking: we validate whether all initial states are a subset of the states the
-- fullfill the formula
modelCheck :: TransitionSystem -> CtlFormula -> Bool
modelCheck ts formula = init_state `Set.isSubsetOf` satFun ts formula
    where
        init_state = initial_states ts

satFun :: TransitionSystem -> CtlFormula -> Set.Set State
{- 
TRUE
primitive True case -> just return all states of the transition system
-}
satFun ts (StateCtl State_True) = states ts
{- 
** Atomic Prop **
-- take all states and get their atomic propositions and check if the atomic proposition 
-- is present for the state (i.e has (atomic_prop, True)) and build a set from all states that fullfull this
-}
satFun ts (StateCtl (AtomicP p)) = Set.fromList [s | s <- Set.toList (states ts), (p, True) `elem` getLabel ts s]
{- 
** AND **
take the intersection of the states that satisfy f1 and the ones that satisfy f2, i.e. the states
that are present in both
-}
satFun ts (StateCtl (And f1 f2)) = satFun ts (StateCtl f1) `Set.intersection` satFun ts (StateCtl f2)
{- 
** NOT **
take the difference of the all states and the one that satisfy f
-}
satFun ts (StateCtl (Not f)) = states ts `Set.difference` satFun ts (StateCtl f)

getLabel :: TransitionSystem -> State -> [AP]
getLabel ts state = Map.findWithDefault  [] state map
    where
        map = label_functions ts


