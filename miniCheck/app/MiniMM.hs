{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module MiniMM (parseMiniMM, MiniMM (MiniMM), Statement (..), Expression (..), Operator (..)) where

{- ORMOLU-DISABLE -}

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)

{- ORMOLU-ENABLE -}

type Identifier = String

-- | The operators used for binary operations.
data Operator = And | Or | Implies | Equal | Xor deriving (Show, Eq)

-- | Expressions don't have sideeffects but always evaluate to a boolean.
data Expression
  = Not Expression
  | Binary Expression Operator Expression
  | Var Identifier
  | TrueLiteral
  | FalseLiteral
  deriving (Show, Eq)

-- | Statements don't evaluate to anything but might have side effects.
data Statement
  = -- | If statements with an optional else
    If
      -- | The condition of an if expression
      Expression
      -- | The condition body
      [Statement]
      -- | The optional body of the else
      (Maybe [Statement])
  | Assign Identifier Expression
  | Print Expression
  | Read Identifier
  | Return Identifier
  deriving (Show, Eq)

data MiniMM = MiniMM
  { arguments :: [Identifier],
    statements :: [Statement]
  }
  deriving (Show, Eq)

-- | Comments in Mini-- are C-style and there are only single line comments not
-- | block ones.
comment :: Parser ()
comment = do
  string "//"
  manyTill anyChar (try (char '\n'))
  return ()

whitespace :: Parser ()
whitespace = skipMany (void (char ' ') <|> void (char '\t') <|> void (char '\n') <|> comment)

parseIdentifier :: Parser Identifier
parseIdentifier = do
  start <- oneOf ['a' .. 'z']
  rest <- many (oneOf (['a' .. 'z'] ++ ['0' .. '9'] ++ ['_']))
  return (start : rest)

-- | Parses the procedure main header up to, including the first curly brace.
parseHeader :: Parser [Identifier]
parseHeader = do
  string "procedure"
  whitespace
  string "main"
  whitespace
  char '('
  args <- parseIdentifier `sepBy1` try (whitespace >> char ',' >> whitespace)
  whitespace
  char ')'
  whitespace
  char '{'
  return args

parseVar :: Parser Expression
parseVar = do
  name <- parseIdentifier
  return (Var name)

parseBool :: Parser Expression
parseBool = do
  (MiniMM.TrueLiteral <$ string "true") <|> (MiniMM.FalseLiteral <$ string "false")

parseNot :: Parser Expression
parseNot = do
  char '!'
  expr <- parseNestedExpression
  return (Not expr)

parseGroup :: Parser Expression
parseGroup = do
  char '('
  whitespace
  expr <- parseExpression
  whitespace
  char ')'
  return expr

parseNestedExpression :: Parser Expression
parseNestedExpression =
  do
    parseBool
    <|> parseVar
    <|> parseGroup

parseRelator :: Parser Operator
parseRelator =
  do
    And <$ char '&'
    <|> (Or <$ char '|')
    <|> (Xor <$ char '^')
    <|> try (Implies <$ string "=>")
    <|> (Equal <$ string "==")

parseBinary :: Parser Expression
parseBinary = do
  left <- parseNestedExpression
  whitespace
  op <- parseRelator
  whitespace
  right <- parseNestedExpression
  return (Binary left op right)

parseExpression :: Parser Expression
parseExpression =
  do
    (try parseBinary)
    <|> parseNot
    <|> parseNestedExpression

parseAssign :: Parser Statement
parseAssign = do
  dst <- parseIdentifier
  whitespace
  char '='
  whitespace
  expr <- parseExpression
  whitespace
  char ';'
  whitespace
  return (Assign dst expr)

-- | This function is called after a if is parsed and will try to parse an
--    following else block.
parseElse :: Parser [Statement]
parseElse = do
  string "else"
  whitespace
  char '{'
  whitespace
  body <- many parseStatement
  whitespace
  char '}'
  whitespace
  return body

parseIf :: Parser Statement
parseIf = do
  string "if"
  whitespace
  char '('
  whitespace
  condition <- parseExpression
  whitespace
  char ')'
  whitespace
  char '{'
  whitespace
  consequence <- many parseStatement
  whitespace
  char '}'
  whitespace

  elseStatements <- optionMaybe parseElse

  return (If condition consequence elseStatements)

parsePrint :: Parser Statement
parsePrint = do
  string "print_bool"
  whitespace
  char '('
  whitespace
  expr <- parseExpression
  whitespace
  char ')'
  whitespace
  char ';'
  whitespace
  return (Print expr)

parseRead :: Parser Statement
parseRead = do
  dst <- parseIdentifier
  whitespace
  char '='
  whitespace
  string "read_bool"
  whitespace
  char '('
  whitespace
  char ')'
  whitespace
  char ';'
  whitespace
  return (Read dst)

-- | Parses a single statement, all except the return
parseStatement :: Parser Statement
parseStatement =
  do
    (try parseRead)
    <|> (try parseAssign)
    <|> try parsePrint
    <|> parseIf

parseReturn :: Parser Statement
parseReturn = do
  string "return"
  whitespace
  var <- parseIdentifier
  whitespace
  char ';'
  whitespace
  return (Return var)

-- | Parses a Mini-- input program into an AST.
parseMiniMM :: Parser MiniMM
parseMiniMM = do
  whitespace
  args <- parseHeader
  whitespace
  statementList <- many (try parseStatement)
  whitespace
  returnStmt <- parseReturn
  whitespace
  char '}'
  whitespace
  eof
  return (MiniMM args (statementList ++ [returnStmt]))
