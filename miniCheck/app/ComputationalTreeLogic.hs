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
import Control.Monad (void)
{- ORMOLU_ENABLE -}

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


desugar :: StateFormula -> StateFormula
-- desugaring
desugar (Exists f) = case f of
  (E g) -> Exists (U State_True g)
  (A g) -> Not (Forall (U State_True (Not g)))
  _ -> Exists f
desugar (Forall f) = case f of 
  (E g) -> Not (Exists (A (Not (g))))
  (A g) -> Not (Exists (U State_True (Not g)))
  _ -> Forall f
desugar (Or (f1) (f2)) = (Not (And (Not (f1)) (Not (f2))))
desugar (Implies (f1) (f2)) = desugar (Or (Not (f1)) (f2))
desugar (Equivalent (f1) (f2)) = desugar (And (Implies (f1) (f2)) (Implies (f2) (f1)))
desugar (Xor (f1) (f2)) = desugar (Or (And (f1) (Not (f2))) (And (f2) (Not (f1))))
 -- no desugaring but apply recursively
desugar (And (f1) (f2)) = (And (desugar f1) (desugar f2))
desugar formula = formula


comment :: Parser ()
comment = do
  string "--"
  manyTill anyChar (try (char '\n'))
  return ()

whitespace :: Parser ()
whitespace = skipMany (void (char ' ') <|> void (char '\t') <|> void (char '\n') <|> comment)

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
  return $ desugar (Or (f1) (f2))

implies :: Parser StateFormula
implies = do
  string "IMPLIES"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return $ desugar (Implies f1 f2)

equivalent :: Parser StateFormula
equivalent = do
  string "EQUIVALENT"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return $ desugar (Equivalent f1 f2)

xor :: Parser StateFormula
xor = do
  string "XOR"
  lParen
  f1 <- stateFormulaParser
  rParen
  lParen
  f2 <- stateFormulaParser
  rParen
  return $ desugar (Xor f1 f2)

exists :: Parser StateFormula
exists = do
  string "EXISTS"
  lParen
  f <- pathFormulaParser
  rParen
  return $ desugar (Exists f)


forAll :: Parser StateFormula
forAll = do
  string "FORALL"
  lParen
  f <- pathFormulaParser
  rParen
  return $ desugar (Forall f)

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
  <|> try forAll

pathFormulaParser :: Parser PathFormula
pathFormulaParser = try next
  <|> try until_parser
  <|> try eventually
  <|> try always


parseComputationalTreeLogic :: Parser CtlFormula
parseComputationalTreeLogic = do
  whitespace
  (StateCtl <$> stateFormulaParser)
  <|> (PathCtl <$> pathFormulaParser)