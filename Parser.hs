module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isLower, isUpper, isDigit)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, rest1) <- p1 input
        (x, rest2) <- p2 rest1
        return (f x, rest2)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (x, rest) <- p input
        parse (f x) rest

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
    (x:xs) | predicate x -> Just (x, xs)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser String
spaces = many $ char ' '

variable :: Parser String
variable = some $ satisfy isLower

macro :: Parser Lambda
macro = Macro <$> some (satisfy (\c -> isUpper c || isDigit c))

parseVar :: Parser Lambda
parseVar = Var <$> variable

parseAbs :: Parser Lambda
parseAbs = do
    _ <- char '\\'
    v <- variable
    _ <- spaces
    _ <- char '.'
    _ <- spaces
    e <- parseLambdaExpr
    return (Abs v e)

parseApp :: Parser Lambda
parseApp = do
    _ <- char '('
    e1 <- parseLambdaExpr
    _ <- spaces
    e2 <- parseLambdaExpr
    _ <- char ')'
    return (App e1 e2)

parseLambdaExpr :: Parser Lambda
parseLambdaExpr = parseAbs <|> parseApp <|> parseVar <|> macro

parseLambda :: String -> Lambda
parseLambda input = case parse (spaces *> parseLambdaExpr <* spaces) input of
    Just (result, "") -> result
    _ -> error "Invalid lambda expression"

parseLine :: String -> Either String Line
parseLine input = case parse (spaces *> (parseBinding <|> parseEval) <* spaces) input of
    Just (result, "") -> Right result
    _ -> Left "Invalid line of code"

parseEval :: Parser Line
parseEval = Eval <$> parseLambdaExpr

parseBinding :: Parser Line
parseBinding = do
    name <- some $ satisfy (\c -> isUpper c || isDigit c)
    _ <- spaces
    _ <- char '='
    _ <- spaces
    expr <- parseLambdaExpr
    return (Binding name expr)
