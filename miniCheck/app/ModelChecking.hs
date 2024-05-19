module ModelChecking (modelCheck) where
{- ORMOLU_DISABLE -}
import TransitionSystem
import ComputationalTreeLogic

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
{- ORMOLU_ENABLE -}

modelCheck :: TransitionSystem -> CtlFormula -> Bool
modelCheck ts ctl = False
