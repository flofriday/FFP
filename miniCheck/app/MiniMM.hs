module MiniMM (parseMiniMM, MiniMM(..), Statement(..), Expression(..), Operator(..)) where

import Text.Parsec (ParseError, Parsec)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

type Identifier = String

data Operator = And | Or | Implies | Equal | Xor deriving (Show, Eq)

data Expression = Not Expression
                | Binary Expression Operator Expression
                | Var Identifier
                | TrueLiteral
                | FalseLiteral
                deriving (Show, Eq)


data Statement  = If Expression [Statement] (Maybe [Statement])
                | Assign Identifier Expression
                | Print Expression
                | Read Identifier
                | Return Identifier
                deriving (Show, Eq)

data MiniMM = MiniMM
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

parseVar :: Parser Expression
parseVar = do
    name <- parseIdentifier
    return (Var name)

parseBool :: Parser Expression
parseBool = do
    (MiniMM.TrueLiteral <$ string "true") <|> ( MiniMM.FalseLiteral <$ string "false")

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
parseNestedExpression = do
    parseBool
    <|> parseVar
    <|> parseGroup

parseRelator :: Parser Operator
parseRelator = do
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
parseExpression = do
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

parseElse :: Parser [Statement]
parseElse = do
    string "else"
    whitespace
    char '{'
    whitespace
    statements <- many parseStatement
    whitespace
    char '}'
    whitespace
    return statements

parseIf :: Parser Statement
parseIf = do
    string "if"
    whitespace
    char '('
    whitespace
    cond <- parseExpression
    whitespace
    char ')'
    whitespace
    char '{'
    whitespace
    statements <- many parseStatement
    whitespace
    char '}'
    whitespace

    -- FIXME: Add else branch
    elseStatements <- optionMaybe parseElse

    return (If cond statements elseStatements)


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
parseStatement = do
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
    statements <- many (try parseStatement)
    whitespace
    returnStmt <- parseReturn
    whitespace
    char '}'
    whitespace
    eof
    return (MiniMM args (statements ++ [returnStmt]))
