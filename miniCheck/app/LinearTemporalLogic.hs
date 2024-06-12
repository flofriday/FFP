{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module LinearTemporalLogic (parseLinearTemporalLogic, LtlFormula (..)) where

{- ORMOLU_DISABLE -}
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>), State)
import Text.Parsec.String (Parser)
import Control.Monad (void)
{- ORMOLU_ENABLE -}

-- | This datatype represents a single ltl formula
data LtlFormula
  = State_True
  | AtomicP String
  | Not LtlFormula
  | And LtlFormula LtlFormula
  | Or LtlFormula LtlFormula
  | Implies LtlFormula LtlFormula
  | Equivalent LtlFormula LtlFormula
  | Xor LtlFormula LtlFormula
  | O LtlFormula -- Next
  | U LtlFormula LtlFormula -- Until
  | E LtlFormula -- Eventually
  | A LtlFormula -- Always
  deriving (Eq, Show)

desugar :: LtlFormula -> LtlFormula
-- desugaring
desugar (E phi) = U State_True (desugar phi)
desugar (A phi) = Not (desugar (E (Not phi)))
desugar (Or (f1) (f2)) = (Not (And (Not (desugar f1)) (Not (desugar f2))))
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
lowerChar = oneOf (['a' .. 'z'])

identifier :: Parser String
identifier = do
  first <- lowerChar
  rest <- many (lowerChar <|> char '_' <|> digit)
  return (first : rest)

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

atomicProp :: Parser LtlFormula
atomicProp = do
  string "AP"
  whitespace
  name <- identifier
  return (AtomicP name)

trueState :: Parser LtlFormula
trueState = do
  string "TRUE"
  whitespace
  return State_True

negation :: Parser LtlFormula
negation = do
  string "NOT"
  lParen
  f <- ltlFormulaParser
  rParen
  return (Not f)

and_parser :: Parser LtlFormula
and_parser = do
  string "AND"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (And f1 f2)

or_parser :: Parser LtlFormula
or_parser = do
  string "OR"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (Or (f1) (f2))

implies :: Parser LtlFormula
implies = do
  string "IMPLIES"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (Implies f1 f2)

equivalent :: Parser LtlFormula
equivalent = do
  string "EQUIVALENT"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (Equivalent f1 f2)

xor :: Parser LtlFormula
xor = do
  string "XOR"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (Xor f1 f2)

next :: Parser LtlFormula
next = do
  string "O"
  lParen
  f <- ltlFormulaParser
  rParen
  return (O f)

until_parser :: Parser LtlFormula
until_parser = do
  string "U"
  lParen
  f1 <- ltlFormulaParser
  rParen
  lParen
  f2 <- ltlFormulaParser
  rParen
  return (U f1 f2)

eventually :: Parser LtlFormula
eventually = do
  string "E"
  lParen
  f <- ltlFormulaParser
  rParen
  return (E f)

always :: Parser LtlFormula
always = do
  string "A"
  lParen
  f <- ltlFormulaParser
  rParen
  return (A f)

ltlFormulaParser :: Parser LtlFormula
ltlFormulaParser =
  trueState
    <|> try atomicProp
    <|> try negation
    <|> try and_parser
    <|> try or_parser
    <|> try implies
    <|> try equivalent
    <|> try xor
    <|> try next
    <|> try until_parser
    <|> try eventually
    <|> try always

-- | the main function of this module. Takes a string and parses it to a LTL Formula
parseLinearTemporalLogic :: Parser LtlFormula
parseLinearTemporalLogic =
  do
    whitespace
    formula <- ltlFormulaParser
    return (desugar formula)