-- Functional Expression Evaluator
{-# LANGUAGE LambdaCase #-}            -- Allows using \case for pattern-matching lambdas

module Main where                       -- Defines the main module

import Data.Char                        -- Provides isDigit, isAlpha, isSpace
import Control.Applicative (Alternative(..)) -- Gives <|>, empty, some, many

-- 1. Abstract Syntax Tree (defines all expression node types)

data Expr
  = Num Double                          -- literal number
  | Add Expr Expr                       -- a + b
  | Sub Expr Expr                       -- a - b
  | Mul Expr Expr                       -- a * b
  | Div Expr Expr                       -- a / b
  | Pow Expr Expr                       -- a ^ b (exponent)
  | Abs Expr                            -- abs(a)
  | Max Expr Expr                       -- max(a, b)
  | Min Expr Expr                       -- min(a, b)
  deriving (Show, Eq)

-- 2. Result type (evaluation output)

data Result
  = Value Double                        -- successfully computed value
  | Error String                        -- error message
  deriving (Show, Eq)

-- 3. Parser definition (wrapper around a function)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
                                        -- Parser takes a String; returns Maybe (result, remaining)

-- Functor: applies a function to parsed result
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input                -- run p
    Just (f x, rest)                    -- apply f to result

-- Applicative: allows sequencing parsers
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)      -- parser that returns x without consuming input
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input                          -- parse a function
    (x, rest2) <- px rest1                          -- parse an argument
    Just (f x, rest2)                               -- apply function

-- Monad: allows dependent sequencing (p >>= f)
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input                            -- parse a value
    runParser (f x) rest                             -- feed value into next parser
  return = pure

-- Alternative: choice between parsers
instance Alternative Parser where
  empty = Parser $ const Nothing                     -- parser that always fails
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input                            -- try p1, else try p2

-- 4. Basic parsing utilities

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x, xs)                       -- return char if predicate true
  _            -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)                             -- parse specific character

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (tok, rest) = span f input                     -- split longest prefix
   in Just (tok, rest)

ws :: Parser String
ws = spanP isSpace                                   -- consume whitespace

doubleP :: Parser Double
doubleP = fmap read $ ws *> number <* ws             -- parse number with optional spaces
  where
    number =
      (++) <$> some (satisfy isDigit)                -- digits
           <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit))
                <|> pure "" )                        -- optional decimal part

identP :: Parser String
identP = ws *> some (satisfy isAlpha) <* ws          -- parse identifier like "abs", "max"

parseExpr :: String -> Maybe Expr
parseExpr s = case runParser expr s of
  Just (ast, rest) | all isSpace rest -> Just ast    -- succeed only if entire input parsed
  _ -> Nothing

signedDoubleP :: Parser Double
signedDoubleP = (negate <$> (charP '-' *> doubleP))  -- handle negative numbers
             <|> doubleP

-- 5. Grammar with operator precedence

expr :: Parser Expr
expr = term `chainl1` addop                          -- left assoc: +, -

term :: Parser Expr
term = power `chainl1` mulop                         -- left assoc: *, /

power :: Parser Expr
power = factor `chainr1` (Pow <$ charP '^')          -- right assoc: exponent

factor :: Parser Expr
factor =
      funcP                                          -- function call
  <|> (Num <$> signedDoubleP)                        -- number
  <|> (charP '(' *> expr <* charP ')')               -- parenthesized expression

addop :: Parser (Expr -> Expr -> Expr)
addop = (Add <$ charP '+') <|> (Sub <$ charP '-')    -- return AST constructor

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (Mul <$ charP '*') <|> (Div <$ charP '/')    -- return AST constructor

-- chainl1: repeatedly apply left-associative operators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do
      f <- op                                       -- parse operator
      y <- p                                        -- parse next operand
      rest (f x y))                                 -- combine left-associatively
      <|> pure x                                    -- stop if no more ops

-- chainr1: right-associative version
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x =
      (do
        f <- op                                      -- operator
        y <- chainr1 p op                            -- recursively parse right side
        pure (f x y))                                -- build AST
      <|> pure x

-- 6. Function parser: abs(x), max(a,b), min(a,b)

funcP :: Parser Expr
funcP = do
  fname <- identP                                   -- function name
  charP '('
  a <- expr                                         -- first argument
  next <- (charP ',' *> expr >>= \b -> pure (Just b)) -- optional second arg
       <|> pure Nothing
  charP ')'
  case (fname, next) of                             -- build correct constructor
    ("abs", Nothing)   -> pure (Abs a)
    ("max", Just b)    -> pure (Max a b)
    ("min", Just b)    -> pure (Min a b)
    _                  -> empty                     -- unknown function

-- 7. Evaluator

eval :: Expr -> Result
eval = \case
  Num n -> Value n                                   -- literal

  Add a b -> bin (+) a b                             -- binary operators
  Sub a b -> bin (-) a b
  Mul a b -> bin (*) a b

  Div a b ->                                         -- division w/ zero check
    case eval b of
      Value 0 -> Error "Division by zero"
      Value n ->
        case eval a of
          Value m -> Value (m / n)
          Error e -> Error e
      Error e -> Error e

  Pow a b -> bin (**) a b                            -- exponent

  Abs a -> case eval a of
             Value x -> Value (abs x)
             Error e -> Error e

  Max a b -> bin max a b
  Min a b -> bin min a b

  where
    bin op a b =
      case (eval a, eval b) of                       -- helper function
        (Value x, Value y) -> Value (x `op` y)
        (Error e, _) -> Error e
        (_, Error e) -> Error e

-- 8. Main Demo

main :: IO ()
main = do
  putStrLn "Try: runParser expr \"1 + 2 * 3 - 4\""     -- some examples
  putStrLn "Try exponent: runParser expr \"2^3^2\""
  putStrLn "Try functions: runParser expr \"max(3, abs(-5))\""
  putStrLn "Then evaluate the parsed expression with eval."
