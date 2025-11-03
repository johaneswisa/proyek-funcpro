-- Functional Expression Evaluator
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Control.Applicative (Alternative(..))

-- 1. Define expression data structure
-- Represents a mathematical expression tree.
-- Each constructor corresponds to an operator or a numeric literal.
data Expr
  = Num Double         -- Numeric constant
  | Add Expr Expr      -- Addition
  | Sub Expr Expr      -- Subtraction
  | Mul Expr Expr      -- Multiplication
  | Div Expr Expr      -- Division
  deriving (Show, Eq)

-- 2. Minimal Parser Type and Instances
-- A simple parser type that takes a String input and
-- returns Maybe (parsedValue, remainingString)
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor instance allows applying a function to the parsed result.
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

-- Applicative instance allows sequencing of parsers.
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    Just (f x, rest2)

-- Monad instance enables dependent sequential parsing.
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
  return = pure

-- Alternative instance provides choice and failure handling.
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

-- 3. Basic Parser Helpers

-- Consumes one character if it satisfies the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x, xs)
  _            -> Nothing

-- Parses a specific character.
charP :: Char -> Parser Char
charP c = satisfy (== c)

-- Parses a span of characters while a predicate holds true.
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (token, rest)

-- Consumes and ignores any surrounding whitespace.
ws :: Parser String
ws = spanP isSpace

-- Parses a floating-point number (e.g., 123, 45.67).
doubleP :: Parser Double
doubleP = fmap read $ ws *> number <* ws
  where
    -- Parses the digits part, optionally followed by a decimal.
    number = (++) <$> some (satisfy isDigit)
                  <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit)) <|> pure "" )

-- 4. Expression Parser (+, -, *, /)

-- Top-level parser for arithmetic expressions (handles + and -).
expr :: Parser Expr
expr = term `chainl1` addop

-- Parses multiplication and division expressions.
term :: Parser Expr
term = factor `chainl1` mulop

-- Parses a number or a parenthesized subexpression.
factor :: Parser Expr
factor = (Num <$> doubleP)
     <|> (charP '(' *> expr <* charP ')')

-- Parses addition or subtraction operators.
addop :: Parser (Expr -> Expr -> Expr)
addop = (Add <$ charP '+') <|> (Sub <$ charP '-')

-- Parses multiplication or division operators.
mulop :: Parser (Expr -> Expr -> Expr)
mulop = (Mul <$ charP '*') <|> (Div <$ charP '/')

-- Helper combinator for parsing left-associative binary operators.
-- Example: parses "1 - 2 - 3" as ((1 - 2) - 3)
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> pure x

-- 5. Test in GHCi
-- Example usage:
-- runParser expr "1+2*3-4"
-- Just (Sub (Add (Num 1.0) (Mul (Num 2.0) (Num 3.0))) (Num 4.0),"")
main :: IO ()
main = putStrLn "Load this file in GHCi and test with runParser expr \"1+2*3-4\""
