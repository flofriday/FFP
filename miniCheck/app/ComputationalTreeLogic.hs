module ComputationalTreeLogic (parseComputationalTreeLogic, CtlFormula(..), PathFormula(..), StateFormula(..)) where

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
  rest <- many (lowerChar <|> char '_' <|> digit)
  return (first:rest)

lParen :: Parser Char
lParen = do
  whitespace
  c <- char '('
  whitespace
  return c

rParen :: Parser Char
rParen = do
  whitespace
  c <- char ')'
  whitespace
  return c

atomicProp :: Parser StateFormula
atomicProp = do
  string "AP"
  whitespace
  id <- identifier
  return (AtomicP id)

negation :: Parser StateFormula
negation = do
  string "NOT"
  lParen
  f <- stateFormulaParser
  rParen
  return (Not f)

and_parser :: Parser StateFormula
and_parser = do
  string "AND"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (And f1 f2)

or_parser :: Parser StateFormula
or_parser = do
  string "OR"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (Or f1 f2)

implies :: Parser StateFormula
implies = do
  string "IMPLIES"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (Implies f1 f2)

equivalent :: Parser StateFormula
equivalent = do
  string "EQUIVALENT"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (Equivalent f1 f2)

xor :: Parser StateFormula
xor = do
  string "XOR"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (Xor f1 f2)

exists :: Parser StateFormula
exists = do
  string "EXISTS"
  lParen
  f <- pathFormulaParser
  rParen
  return (Exists f)

forall :: Parser StateFormula
forall = do
  string "FORALL"
  lParen
  f <- pathFormulaParser
  rParen
  return (Forall f)

next :: Parser PathFormula
next = do
  string "O"
  lParen
  f <- stateFormulaParser
  rParen
  return (O f)

until_parser :: Parser PathFormula
until_parser = do
  string "U"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return (U f1 f2)

eventually :: Parser PathFormula
eventually = do
  string "E"
  lParen
  f <- stateFormulaParser
  rParen
  return (E f)

always :: Parser PathFormula
always = do
  string "A"
  lParen
  f <- stateFormulaParser
  rParen
  return (A f)

stateFormulaParser :: Parser StateFormula
stateFormulaParser = try atomicProp
  <|> try negation
  <|> try and_parser
  <|> try or_parser
  <|> try implies
  <|> try equivalent
  <|> try xor
  <|> try exists
  <|> try forall

pathFormulaParser :: Parser PathFormula
pathFormulaParser = try next
  <|> try until_parser
  <|> try eventually
  <|> try always


parseComputationalTreeLogic :: Parser CtlFormula
parseComputationalTreeLogic = (StateCtl <$> stateFormulaParser) 
  <|> (PathCtl <$> pathFormulaParser)