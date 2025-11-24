-- Functional Expression Evaluator 
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Control.Applicative (Alternative(..))
import System.IO                   -- for REPL input/output
import Test.QuickCheck  hiding (Result)           -- simple QuickCheck integration

-- 1. Abstract Syntax Tree

data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr                  -- NEW: modulo operator
  | Pow Expr Expr
  | Abs Expr
  | Max Expr Expr
  | Min Expr Expr
  | Floor Expr
  | Ceil Expr

  deriving (Show, Eq)

-- 2. Result type with improved error messages

data Result
  = Value Double
  | Error String
  deriving (Show, Eq)

-- 3. Parser definition

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    Just (f x, rest2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
  return = pure

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

-- 4. Basic utilities

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x,xs)
  _            -> Nothing

charP :: Char -> Parser Char               
charP = satisfy . (==)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (tok, rest) = span f input in Just (tok, rest)

ws :: Parser String
ws = spanP isSpace

doubleP :: Parser Double
doubleP = fmap read $ ws *> number <* ws
  where
    number = (++) <$> some (satisfy isDigit)
                  <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit))
                       <|> pure "" )

signedDoubleP :: Parser Double
signedDoubleP = (negate <$> (charP '-' *> doubleP)) <|> doubleP

identP :: Parser String
identP = ws *> some (satisfy isAlpha) <* ws

parseExpr :: String -> Maybe Expr
parseExpr s = case runParser expr s of
  Just (ast, rest) | all isSpace rest -> Just ast
  _ -> Nothing

-- 5. Grammar with operator precedence

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = power `chainl1` mulop

power :: Parser Expr
power = factor `chainr1` (Pow <$ charP '^')

factor :: Parser Expr
factor =
      funcP
  <|> (Num <$> signedDoubleP)
  <|> (charP '(' *> expr <* charP ')')

addop :: Parser (Expr -> Expr -> Expr)
addop =
      (Add <$ charP '+')
  <|> (Sub <$ charP '-')

mulop :: Parser (Expr -> Expr -> Expr)
mulop =
      (Mul <$ charP '*')
  <|> (Div <$ charP '/')
  <|> (Mod <$ charP '%')      -- NEW: modulo operator

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 pure (f x y))
             <|> pure x

-- 6. Function parser

funcP :: Parser Expr
funcP = do
  fname <- identP
  charP '('
  a <- expr
  next <- (charP ',' *> expr >>= \b -> pure (Just b))
       <|> pure Nothing
  charP ')'
  case (fname, next) of
    ("abs", Nothing)  -> pure (Abs a)
    ("max", Just b)   -> pure (Max a b)
    ("min", Just b)   -> pure (Min a b)
    ("floor", Nothing)-> pure (Floor a)   
    ("ceil", Nothing) -> pure (Ceil a)    
    _ -> empty


-- 7. Evaluator (with improved errors)

eval :: Expr -> Result
eval = \case
  Num n -> Value n

  Add a b -> bin (+) a b
  Sub a b -> bin (-) a b
  Mul a b -> bin (*) a b

  Div a b ->
    case eval b of
      Value 0 -> Error "Error: Division by zero"
      Value n ->
        case eval a of
          Value m -> Value (m / n)
          Error e -> Error e
      Error e -> Error e

  Mod a b ->                      -- NEW: modulo operator
    case (eval a, eval b) of
      (_, Value 0)      -> Error "Error: Modulo by zero"
      (Value x, Value y) -> Value (x - y * fromIntegral (floor (x / y)))
      (Error e, _)       -> Error e
      (_, Error e)       -> Error e

  Pow a b -> bin (**) a b

  Abs a -> case eval a of
    Value x -> Value (abs x)
    Error e -> Error e

  Max a b -> bin max a b
  Min a b -> bin min a b

  Floor a -> case eval a of
    Value x -> Value (fromIntegral (floor x))
    Error e -> Error e

  Ceil a -> case eval a of
    Value x -> Value (fromIntegral (ceiling x))
    Error e -> Error e


  where
    -- shared binary helper
    bin op a b =
      case (eval a, eval b) of
        (Value x, Value y) -> Value (x `op` y)
        (Error e, _) -> Error e
        (_, Error e) -> Error e

-- 8. Simple QuickCheck property 
--    Ensures parsing of simple numbers is correct.

prop_add_comm :: Double -> Double -> Bool
prop_add_comm x y =
  eval (Add (Num x) (Num y))
  == eval (Add (Num y) (Num x))

-- 1. floor parsing test
prop_parseFloor =
  parseExpr "floor(3.9)" == Just (Floor (Num 3.9))

-- 2. ceil parsing test
prop_parseCeil =
  parseExpr "ceil(3.1)" == Just (Ceil (Num 3.1))

-- 3. floor evaluation test
prop_evalFloor =
  eval (Floor (Num 3.9)) == Value 3.0

-- 4. ceil evaluation test
prop_evalCeil =
  eval (Ceil (Num 3.1)) == Value 4.0

-- 5. combined expression
prop_floorCeilExpr =
  (eval <$> parseExpr "floor(2.9) + ceil(1.1)") == Just (Value 4.0)   -- 2 + 2

runAllTests :: IO ()
runAllTests = do
  quickCheck prop_parseFloor
  quickCheck prop_parseCeil
  quickCheck prop_evalFloor
  quickCheck prop_evalCeil
  quickCheck prop_floorCeilExpr
  quickCheck prop_add_comm

-- 9. Interactive REPL 

repl :: IO ()
repl = do
  putStr "expr> "
  hFlush stdout
  line <- getLine
  case line of
    ":quit" -> putStrLn "Goodbye!"
    ":test" -> runAllTests >> repl   -- run QuickCheck test
    _ ->
      case parseExpr line of
        Nothing -> putStrLn "Parse error: invalid expression" >> repl
        Just ast ->
          case eval ast of
            Value v -> print v >> repl
            Error e -> putStrLn e >> repl

-- 10. Main now launches interactive mode

main :: IO ()
main = do
  putStrLn "Expression Evaluator (Week 5)"
  putStrLn "Type any expression to evaluate."
  putStrLn "Commands:"
  putStrLn "  :quit   - exit"
  putStrLn "  :test   - run QuickCheck test"
  repl
