{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TransitionSystem (parseTransitionSystem, TransitionSystem (..), State, AP, Action, verifyTransitionSystem, fillEmptyAP) where

{- ORMOLU_DISABLE -}
import Control.Applicative hiding (many)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec hiding ((<|>), State)
import Text.Parsec.String (Parser)
import Control.Monad (void)
{- ORMOLU_ENABLE -}

type State = String

type Action = String

type AP = (String, Bool)

data TransitionSystem = TransitionSystem
  { initial_states :: Set State,
    states :: Set State,
    actions :: Set Action,
    transition :: [(State, Action, State)],
    label_functions :: Map State [AP] -- [(State, [AP])]
  }
  deriving (Show, Eq)

comment :: Parser ()
comment = do
  string "--"
  manyTill anyChar (try (void (char '\n') <|> eof))
  return ()

whitespace :: Parser ()
whitespace = skipMany (void (char ' ') <|> void (char '\t'))

emptyLine :: Parser ()
emptyLine = skipMany (void (char ' ') <|> void (char '\t') <|> void (char '\n') <|> comment)

lowerChar :: Parser Char
lowerChar = oneOf (['a' .. 'z'] ++ ['A' .. 'Z'])

identifier :: Parser String
identifier = do
  first <- lowerChar
  rest <- many (lowerChar <|> char '_')
  return (first : rest)

parseInitial :: Parser (Set State)
parseInitial = do
  string "initial states"
  whitespace
  identifiers <- identifier `sepBy1` try (whitespace >> char ',' >> whitespace)
  whitespace
  char '\n'
  return (Set.fromList identifiers)

parseStates :: Parser (Set State)
parseStates = do
  string "states"
  whitespace
  identifiers <- identifier `sepBy1` try (whitespace >> char ',' >> whitespace)
  whitespace
  char '\n'
  return (Set.fromList identifiers)

parseActions :: Parser (Set Action)
parseActions = do
  string "actions"
  whitespace
  identifiers <- identifier `sepBy` try (whitespace >> char ',' >> whitespace)
  whitespace
  char '\n'
  return (Set.fromList identifiers)

parseTransition :: Parser (State, Action, State)
parseTransition = do
  string "trans" -- add stupid flag
  whitespace
  start_state <- identifier
  whitespace
  action <- identifier
  whitespace
  end_state <- identifier
  whitespace
  char '\n'
  return (start_state, action, end_state)

parseAP :: Parser AP
parseAP = do
  minus <- optionMaybe (char '-')
  ident <- identifier
  let flag = case minus of
        Just _ -> False
        Nothing -> True
  return (ident, flag)

parseLabelFunction :: Parser (State, [AP])
parseLabelFunction = do
  string "labels"
  whitespace
  state <- identifier
  char ':'
  whitespace
  aps <- parseAP `sepBy1` try (whitespace >> char ',' >> whitespace)
  whitespace
  void (char '\n') <|> eof
  return (state, aps)

verifyTransitionSystem :: TransitionSystem -> Maybe String
verifyTransitionSystem ts
  | not (initial_states ts `Set.isSubsetOf` states ts) = Just "not all initial states are valid states"
  | transSrcStates /= states ts = Just "Terminal states are forbidden (at least one state has no outgoing edge)"
  | not (transStates `Set.isSubsetOf` states ts) = Just "not all transition states are valid states"
  | not (transActions `Set.isSubsetOf` actions ts) = Just "not all transition actions are valid actions"
  | Map.keysSet (label_functions ts) /= states ts = Just "every state must have a labeling function"
  | not everyStateLabelsItself = Just "Every label function must label the state it is of"
  | otherwise = Nothing
  where
    transSrcStates = Set.fromList $ map (\(s1, _, _) -> s1) (transition ts)
    transStates = Set.fromList $ concatMap (\(s1, _, s2) -> [s1, s2]) (transition ts)
    transActions = Set.fromList $ filter (/= "TRUE") (map (\(_, a, _) -> a) (transition ts))
    doesLabelItself (s, ap) = elem (s, True) ap
    everyStateLabelsItself = all doesLabelItself (Map.toList (label_functions ts))

fillEmptyAP :: TransitionSystem -> TransitionSystem
fillEmptyAP transitionSystem = transitionSystem {label_functions = newLabels}
  where
    oldLabels = label_functions transitionSystem
    newLabels = Map.fromList $ map (\(k, v) -> (k, fill v)) (Map.toList oldLabels)
    fill apList = apList ++ map (\n -> (n, False)) (missingLabels apList)
    missingLabels apList = Set.toList (allLabels `Set.difference` Set.fromList (map fst apList))
    allLabels = Set.fromList $ concatMap (map fst) (Map.elems oldLabels)

parseTransitionSystem :: Parser TransitionSystem
parseTransitionSystem = do
  emptyLine
  initial <- parseInitial
  emptyLine
  stateList <- parseStates
  emptyLine
  actionList <- parseActions
  emptyLine
  transitionList <- many (parseTransition <* emptyLine)
  emptyLine
  labelFunctions <- many (parseLabelFunction <* emptyLine)
  emptyLine
  eof
  let ts = TransitionSystem initial stateList actionList transitionList (Map.fromList labelFunctions)
  case verifyTransitionSystem ts of
    (Just err) -> fail err
    _ -> return (fillEmptyAP ts)
