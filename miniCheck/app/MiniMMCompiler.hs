{-# LANGUAGE BlockArguments #-}
module MiniMMCompiler (compileMiniMM) where
import TransitionSystem (TransitionSystem (TransitionSystem), verifyTransitionSystem, fillEmptyAP)
import MiniMM
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.List (find)

type Var = (String, Bool)

type CompileError = String

data CompileState = CompileState {
    transitionSystem :: TransitionSystem,
    vars :: [Var],
    nodeCounter :: Int
} deriving (Show, Eq)


variablePermutations :: [String] -> [[Var]]
variablePermutations [] = [[]]
variablePermutations [v] = [[(v, False)],  [(v, True)]]
variablePermutations (v:vs) = concatMap (\tail -> [(v, False) : tail, (v, True):tail]) rest
    where
        rest = variablePermutations vs

setVariable :: String -> Bool -> [Var] -> [Var]
setVariable name val [] = [(name, val)]
setVariable name val ((vname, vval):rest)
    | vname == name = (name, val):rest
    | otherwise = (vname, vval):setVariable name val rest

setVariableInState :: String -> Bool -> CompileState -> CompileState
setVariableInState name val (CompileState ts vars counter) = (CompileState ts newVars counter)
    where
        newVars = setVariable name val vars

{- |Inserts a node into the transitionsystem. 
    The node will have all active vars set.
    The nodecounter will be increased.
    If the node has a predecessor the right transition will be created otherwise 
    it will be inserted as an initial node.
-}
insertNode :: String -> Maybe String -> CompileState -> CompileState
insertNode name previousName (CompileState ts vars nodeCounter) = CompileState newTs vars (nodeCounter + 1)
    where
        (TransitionSystem initial states actions transitions labelFunc) = ts
        newInitial = case previousName of
            (Nothing) -> Set.insert name initial
            _ -> initial
        newStates = Set.insert name states
        newTransitions = case previousName of
            (Just from) -> (from, "TRUE", name) :  transitions
            _ ->  transitions
        newLabelFunc = Map.insert name ((name, True) : vars) labelFunc
        newTs = TransitionSystem newInitial newStates actions newTransitions newLabelFunc

countedName :: String -> CompileState -> String
countedName name state = name ++ show (nodeCounter state)

evalExpression :: Expression -> CompileState -> Either CompileError Bool
evalExpression (Not inner) state = do
    val <- evalExpression inner state
    return (not val)

evalExpression (Binary left op right) state = do
    lval <- evalExpression left state
    rval <- evalExpression right state
    return case op of
        And -> lval && rval
        Or -> lval || rval
        Xor -> lval /= rval
        Implies -> not (lval && not rval)
        Equal -> lval == rval

evalExpression (Var var) state = case find (\(t, _ ) -> t == var) (vars state) of
    (Just (_, val)) -> Right val
    Nothing -> Left ("Variable `" ++ var ++ "` was used before it's definition")


evalExpression TrueLiteral state = Right True
evalExpression FalseLiteral state = Right False

compileStatements :: [Statement] -> String -> CompileState -> Either String CompileState
compileStatements [] _ state = Right state

compileStatements ((Assign dst expr):statements) previous state = do
    val <- evalExpression expr state
    compileStatements statements name (setVariableInState dst val newState)
    where
        name = countedName "assign" state
        newState =  insertNode name (Just previous) state

compileStatements ((If cond body elseBody):statements) previous state = do
    val <- evalExpression cond state
    newState <- case (val, elseBody) of
        (True, _) -> compileStatements body name insertedState
        (False, Just realElseBody) -> compileStatements realElseBody name insertedState
        _ -> Right insertedState
    compileStatements statements name newState
    where
        name = countedName "if" state
        insertedState =  insertNode name (Just previous) state


compileStatements ((Read dst):statements) previous state = do
    let newStateFalse = setVariableInState dst False newState
    (CompileState ts _ counter) <- compileStatements statements name newStateFalse
    let newStateTrue =  setVariableInState dst False (CompileState ts originalVars counter)
    compileStatements statements name newStateTrue
    where
        (CompileState _ originalVars _) = state
        name = countedName "read" state
        newState =  insertNode name (Just previous) state

compileStatements ((Print expr):statements) previous state = do
    _ <- evalExpression expr state
    compileStatements statements name newState
    where
        name = countedName "print" state
        newState = insertNode name (Just previous) state

compileStatements ((Return var):statements) previous state = do
    _ <- evalExpression (Var var) state
    compileStatements statements name newState
    where
        name = countedName "return" state
        newState = insertNode name (Just previous) state


mainFolder :: [Statement] -> Either CompileError CompileState -> [Var] ->  Either CompileError CompileState
mainFolder statements oldState assignment  = case oldState of
    (Right (CompileState ts _ counter)) -> compileStatements statements (mainName counter) (insertNode (mainName counter) Nothing (CompileState ts assignment counter))

    (Left err) -> Left err
    where
        mainName counter = ("main" ++ show counter)

insertTerminalState :: TransitionSystem -> TransitionSystem
insertTerminalState (TransitionSystem initial states actions transitions labelFunc) = (TransitionSystem initial (Set.insert "terminal" states) actions newTransitions newLabelFunc)
    where
        newLabelFunc = Map.insert "terminal" [("terminal", True)] labelFunc
        newTransitions = transitions ++ [("terminal", "TRUE", "terminal")] ++ (map (\leaf -> (leaf, "TRUE", "terminal")) leafStates )
        --leafStates = Set.toList $ transStates `Set.difference` transSrcStates
        leafStates = Set.toList $ states `Set.difference` transSrcStates
        transSrcStates = Set.fromList $ map (\(s1, _, _) -> s1) transitions
        --transStates = Set.fromList $ concatMap (\(s1, _, s2) -> [s1, s2]) transitions 

compileMiniMM :: MiniMM -> Either CompileError TransitionSystem
compileMiniMM (MiniMM args statements) = do
    state <- foldl (mainFolder statements) (Right emptyState) variableAssignments
    let ts = insertTerminalState (transitionSystem state)
    case verifyTransitionSystem ts of
        (Just err) -> Left ("Internal Error, the compiler generated invalid TS\n" ++ err)
        --(Just err) -> Right ts
        _ -> Right (fillEmptyAP ts)
    where
        emptyTs = TransitionSystem (Set.fromList []) (Set.fromList []) (Set.fromList []) [] (Map.fromList [])
        emptyState = CompileState emptyTs [] 0
        variableAssignments = variablePermutations args

