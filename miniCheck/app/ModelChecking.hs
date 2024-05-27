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
satFun ts (StateCtl State_True) = states ts




