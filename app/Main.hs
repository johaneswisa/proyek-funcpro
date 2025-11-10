-- Functional Expression Evaluator
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Char
import Control.Applicative (Alternative(..))

-- 1. Expression data structure (Abstract Syntax Tree)
-- Represents arithmetic expressions as a tree of operations.
data Expr
  = Num Double         -- Numeric literal
  | Add Expr Expr      -- Addition node
  | Sub Expr Expr      -- Subtraction node
  | Mul Expr Expr      -- Multiplication node
  | Div Expr Expr      -- Division node
  deriving (Show, Eq)

-- 2. Result type for evaluation outcomes
-- Encodes either a numeric value or an error message.
data Result
  = Value Double       -- Successful evaluation
  | Error String       -- Error (e.g. division by zero)
  deriving (Show, Eq)

-- 3. Parser definition: turns strings into structured data
-- A parser takes input and returns a parsed value plus leftover text.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor: allows applying a function to a parsed result
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p(input)
    Just (f x, rest)

-- Applicative: allows combining independent parsers
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    Just (f x, rest2)

-- Monad: allows sequencing where later parsers depend on earlier results
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
  return = pure

-- Alternative: supports failure and choice between parsers
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

-- 4. Basic parsing utilities
satisfy :: (Char -> Bool) -> Parser Char
-- Parses a single character if it meets a condition
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x, xs)
  _            -> Nothing

charP :: Char -> Parser Char
-- Parses a specific character
charP c = satisfy (== c)

spanP :: (Char -> Bool) -> Parser String
-- Consumes as many matching characters as possible
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (token, rest)

ws :: Parser String
-- Skips over any whitespace
ws = spanP isSpace

doubleP :: Parser Double
-- Parses a floating-point number (with optional decimal part)
doubleP = fmap read $ ws *> number <* ws
  where
    number = (++) <$> some (satisfy isDigit)
                  <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit)) <|> pure "" )

-- 5. Expression grammar with operator precedence
-- expr   := term   ('+' | '-') term*
-- term   := factor ('*' | '/') factor*
-- factor := number | '(' expr ')'

expr :: Parser Expr
-- Handles + and - (lowest precedence)
expr = term `chainl1` addop

term :: Parser Expr
-- Handles * and / (higher precedence)
term = factor `chainl1` mulop

factor :: Parser Expr
-- Parses numbers or parenthesized expressions
factor = (Num <$> doubleP)
     <|> (charP '(' *> expr <* charP ')')

addop :: Parser (Expr -> Expr -> Expr)
-- Parses + or - and returns corresponding constructor
addop = (Add <$ charP '+') <|> (Sub <$ charP '-')

mulop :: Parser (Expr -> Expr -> Expr)
-- Parses * or / and returns corresponding constructor
mulop = (Mul <$ charP '*') <|> (Div <$ charP '/')

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- Builds left-associative binary operator chains
chainl1 p op = p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> pure x

-- 6. Evaluator with simple error handling
-- Recursively computes the value of an expression tree.
eval :: Expr -> Result
eval = \case
  Num n       -> Value n
  Add a b     -> bin (+) a b
  Sub a b     -> bin (-) a b
  Mul a b     -> bin (*) a b
  Div a b     ->
    case eval b of
      Error e -> Error e
      Value 0 -> Error "Division by zero"
      Value n -> case eval a of
        Error e -> Error e
        Value m -> Value (m / n)
  where
    bin op a b =
      case (eval a, eval b) of
        (Value x, Value y) -> Value (x `op` y)
        (Error e, _) -> Error e
        (_, Error e) -> Error e

-- 7. Example usage
-- runParser expr "1+2*3-4"
--   ==> Just (Sub (Add (Num 1.0) (Mul (Num 2.0) (Num 3.0))) (Num 4.0),"")
-- eval (Sub (Add (Num 1.0) (Mul (Num 2.0) (Num 3.0))) (Num 4.0))
--   ==> Value 3.0
main :: IO ()
main = do
  putStrLn "runParser expr \"1+2*3-4\""
  putStrLn "Then evaluate with: eval <parsed_expr>"
