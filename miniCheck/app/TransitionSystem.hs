module TransitionSystem (parseTransitionSystem, State, TransitionSystem(..)) where

{- ORMOLU_DISABLE -}
import Control.Applicative hiding (many)
import Control.Monad.Identity (Identity)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (ParseError, Parsec)
import Text.Parsec hiding ((<|>), State)
import Text.Parsec.String (Parser)
{- ORMOLU_ENABLE -}

type State = String

type Action = String

type AP = (String, Bool)

data TransitionSystem = TransitionSystem
  { initial_states :: Set.Set State,
    states :: Set State,
    actions :: Set Action,
    transition :: [(State, Action, State)],
    label_functions :: Map State [AP] -- [(State, [AP])]
  }
  deriving (Show, Eq)

whitespace :: Parser ()
whitespace = skipMany (char ' ' <|> char '\t')

emptyLine :: Parser()
emptyLine = skipMany (char ' ' <|> char '\t' <|> char '\n')

lowerChar :: Parser Char
lowerChar = oneOf (['a'..'z'] ++ ['A'..'Z'])

identifier :: Parser String
identifier = do
  first <- lowerChar
  rest <- many (lowerChar <|> char '_')
  return (first:rest)

parseInitial :: Parser (Set State)
parseInitial = do
  string "initial states"
  whitespace
  identifiers <- identifier `sepBy` (whitespace >> char ',' >> whitespace)
  char '\n'
  return (Set.fromList identifiers)

parseStates :: Parser (Set State)
parseStates = do
  string "states"
  whitespace
  identifiers <- identifier `sepBy` (whitespace >> char ',' >> whitespace)
  char '\n'
  return (Set.fromList identifiers)

parseActions :: Parser (Set Action)
parseActions = do
  string "actions"
  whitespace
  identifiers <- identifier `sepBy` (whitespace >> char ',' >> whitespace)
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
  char '\n'
  return (start_state, action, end_state)

parseAP :: Parser AP
parseAP = do
  minus <- optionMaybe (char '-')
  ident <- identifier
  let flag = case minus of
               Just _  -> False
               Nothing -> True
  return (ident, flag)

parseLabelFunction :: Parser (State, [AP])
parseLabelFunction = do
  string "labels"
  whitespace
  state <- identifier
  char ':'
  whitespace
  labels <- parseAP `sepBy` (whitespace >> char ',' >> whitespace)
  return (state, labels)


parseTransitionSystem :: Parser TransitionSystem
parseTransitionSystem = do
  emptyLine
  initial <- parseInitial
  emptyLine
  states <- parseStates
  emptyLine
  actions <- parseActions
  emptyLine
  transitions <- many parseTransition
  emptyLine
  labelFunctions <- many parseLabelFunction
  emptyLine
  return (TransitionSystem initial states actions transitions (Map.fromList labelFunctions))