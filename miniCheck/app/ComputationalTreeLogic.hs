module ComputationalTreeLogic (parseComputationalTreeLogic) where

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

-- parse :: Parsec.Parsec String -> String -> String
-- parse rule text = Parsec.parse rule "(source)" text
data CtlFormula = StateCtl StateFormula | PathCtl PathFormula
  deriving (Eq, Show)

data StateFormula = State_True
  | AtomicP String
  | Not StateFormula
  | And StateFormula StateFormula
  | Or StateFormula StateFormula
  | Implies StateFormula StateFormula
  | Equivalent StateFormula StateFormula
  | Xor StateFormula StateFormula
  | Exists PathFormula
  | Forall PathFormula
  deriving (Eq, Show)

data PathFormula = O StateFormula -- Next
  | U StateFormula StateFormula -- Until
  | E StateFormula -- Eventually
  | A StateFormula -- Always
  deriving (Eq, Show)
  

whitespace :: Parser ()
whitespace = skipMany (char ' ' <|> char '\t')

lowerChar :: Parser Char
lowerChar = oneOf (['a'..'z'])

identifier :: Parser String
identifier = do
  first <- lowerChar
  rest <- many (lowerChar <|> char '_')
  return (first:rest)

lParen :: Parser Char
lParen = char '('

rParen :: Parser Char
rParen = char ')'

atomicProp :: Parser StateFormula
atomicProp = do
  string "AP"
  whitespace
  id <- identifier
  return (AtomicP id)

negation :: Parser StateFormula
negation = do
  string "NOT"
  whitespace
  lParen
  whitespace
  f <- stateFormulaParser
  whitespace
  rParen
  return (Not f)

and_parser :: Parser StateFormula
and_parser = do
  string "AND"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (And f1 f1)

or_parser :: Parser StateFormula
or_parser = do
  string "OR"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (Or f1 f1)

implies :: Parser StateFormula
implies = do
  string "IMPLIES"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (Implies f1 f2)

equivalent :: Parser StateFormula
equivalent = do
  string "EQUIVALENT"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (Equivalent f1 f2)

xor :: Parser StateFormula
xor = do
  string "XOR"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (Xor f1 f2)

exists :: Parser StateFormula
exists = do
  string "EXISTS"
  whitespace
  lParen
  whitespace
  f <- pathFormulaParser
  whitespace
  rParen
  return (Exists f)

forall :: Parser StateFormula
forall = do
  string "FORALL"
  whitespace
  lParen
  whitespace
  f <- pathFormulaParser
  whitespace
  rParen
  return (Forall f)

next :: Parser PathFormula
next = do
  string "O"
  whitespace
  lParen
  whitespace
  f <- stateFormulaParser
  whitespace
  rParen
  return (O f)

until_parser :: Parser PathFormula
until_parser = do
  string "U"
  whitespace
  lParen
  whitespace
  f1 <- stateFormulaParser
  whitespace
  f2 <- stateFormulaParser
  whitespace
  rParen
  return (U f1 f2)

eventually :: Parser PathFormula
eventually = do
  string "E"
  whitespace
  lParen
  whitespace
  f <- stateFormulaParser
  whitespace
  rParen
  return (E f)

always :: Parser PathFormula
always = do
  string "A"
  whitespace
  lParen
  whitespace
  f <- stateFormulaParser
  whitespace
  rParen
  return (A f)

stateFormulaParser :: Parser StateFormula
stateFormulaParser = atomicProp
  <|> negation
  <|> and_parser
  <|> or_parser
  <|> implies
  <|> equivalent
  <|> xor
  <|> exists
  <|> forall

pathFormulaParser :: Parser PathFormula
pathFormulaParser = next
  <|> until_parser
  <|> eventually
  <|> always


parseComputationalTreeLogic :: Parser CtlFormula
parseComputationalTreeLogic = (StateCtl <$> stateFormulaParser) 
  <|> (PathCtl <$> pathFormulaParser)