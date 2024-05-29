module MiniMM (parseMiniMM) where

import Text.Parsec (ParseError, Parsec)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

type Identifier = String

data Operator = And | Or | Implies | Equal | XOR deriving (Show, Eq)

data Expression = Not Expression
                | Binary Expression Operator Expression
                | Var Identifier
                | True
                | False
                deriving (Show, Eq)


data Statement  = If [Statement] (Maybe [Statement])
                | Assign Identifier Expression
                | Print Expression
                | Read Identifier
                | Return Identifier
                deriving (Show, Eq)

data Program = Program
    {   arguments :: [Identifier],
        statements :: [Statement]
    } deriving (Show, Eq)


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
    start <- oneOf ['a'..'z']
    rest <- many (oneOf (['a'..'z'] ++ ['0'..'9']))
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

-- | Parses a single statement
{-
parseStatement :: parser Statement
parseStatement = do
    return (Read "flo")
-}

-- | Parses a Mini-- input program into an AST.
parseMiniMM :: Parser Program
parseMiniMM = do
    whitespace
    args <- parseHeader
    whitespace
    --statements <- many parseStatement
    --whitespace
    let statements = []
    char '}'
    whitespace
    eof
    return (Program args statements)
