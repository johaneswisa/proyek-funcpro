-- Functional Expression Evaluator 
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Control.Applicative (Alternative(..))
import System.IO                   -- for REPL input/output
import Test.QuickCheck  hiding (Result)           -- simple QuickCheck integration

-- 1. Abstract Syntax Tree (AST)
--    Mendefinisikan bentuk ekspresi yang dapat diparse dan dievaluasi.

data Expr
  = Num Double                    -- literal number
  | Add Expr Expr                 -- a + b
  | Sub Expr Expr                 -- a - b
  | Mul Expr Expr                 -- a * b
  | Div Expr Expr                 -- a / b
  | Mod Expr Expr                 -- a % b (modulo)
  | Pow Expr Expr                 -- a ^ b
  | Abs Expr                      -- abs(x)
  | Max Expr Expr                 -- max(a,b)
  | Min Expr Expr                 -- min(a,b)
  | Floor Expr                    -- floor(x)
  | Ceil Expr                     -- ceil(x)
  deriving (Show, Eq)

-- 2. Result type
--    Merepresentasikan hasil evaluasi atau error.
data Result
  = Value Double                  -- successful evaluation
  | Error String                  -- error message
  deriving (Show, Eq)

-- 3. Parser definition
--    Parser monad sederhana untuk membaca string menjadi Expr.

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Functor instance: map fungsi ke hasil parser.
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

-- Applicative instance: kombinasi beberapa parser berurutan.
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    Just (f x, rest2)

-- Monad instance: memungkinkan parser dependent/sekuensial.
instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    runParser (f x) rest
  return = pure

-- Alternative instance: mendukung p1 <|> p2 (coba parser lain).
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

-- 4. Basic utilities for building parsers

satisfy :: (Char -> Bool) -> Parser Char       -- parser satu char jika memenuhi predicate
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x,xs)
  _            -> Nothing

charP :: Char -> Parser Char                   -- parse char tertentu
charP = satisfy . (==)

spanP :: (Char -> Bool) -> Parser String       -- parse substring selama predicate true
spanP f = Parser $ \input ->
  let (tok, rest) = span f input in Just (tok, rest)

ws :: Parser String                            -- parse whitespace
ws = spanP isSpace

doubleP :: Parser Double                       -- parse angka desimal
doubleP = fmap read $ ws *> number <* ws
  where
    number =
      (++) <$> some (satisfy isDigit)
           <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit)) <|> pure "" )

signedDoubleP :: Parser Double                 -- parse angka dengan optional '-'
signedDoubleP = (negate <$> (charP '-' *> doubleP)) <|> doubleP

identP :: Parser String                        -- parse identifier (nama fungsi)
identP = ws *> some (satisfy isAlpha) <* ws

parseExpr :: String -> Maybe Expr              -- helper: parse string menjadi Expr
parseExpr s = case runParser expr s of
  Just (ast, rest) | all isSpace rest -> Just ast
  _ -> Nothing

-- 5. Grammar with operator precedence
--    Implementasi parser ekspresi lengkap dengan prioritas operator.

expr :: Parser Expr                             -- level 1: +, -
expr = term `chainl1` addop

term :: Parser Expr                             -- level 2: *, /, %
term = power `chainl1` mulop

power :: Parser Expr                            -- level 3: ^ (right associative)
power = factor `chainr1` (Pow <$ charP '^')

factor :: Parser Expr                           -- angka, fungsi, atau (expr)
factor =
      funcP
  <|> (Num <$> signedDoubleP)
  <|> (charP '(' *> expr <* charP ')')

addop :: Parser (Expr -> Expr -> Expr)          -- operator + dan -
addop =
      (Add <$ charP '+')
  <|> (Sub <$ charP '-')

mulop :: Parser (Expr -> Expr -> Expr)          -- operator *, /, %
mulop =
      (Mul <$ charP '*')
  <|> (Div <$ charP '/')
  <|> (Mod <$ charP '%')      

-- left associative chaining
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

-- right associative chaining (untuk exponent)
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 pure (f x y))
             <|> pure x

-- 6. Function parser
--    Mem-parse fungsi unary dan binary seperti abs(x), max(a,b), floor(x)

funcP :: Parser Expr
funcP = do
  fname <- identP
  charP '('
  a <- expr
  next <- (charP ',' *> expr >>= \b -> pure (Just b))
       <|> pure Nothing
  charP ')'
  case (fname, next) of
    ("abs", Nothing)   -> pure (Abs a)
    ("max", Just b)    -> pure (Max a b)
    ("min", Just b)    -> pure (Min a b)
    ("floor", Nothing) -> pure (Floor a)
    ("ceil", Nothing)  -> pure (Ceil a)
    _ -> empty

-- 7. Evaluator
--    Mengevaluasi Expr menjadi nilai Double atau error.

eval :: Expr -> Result
eval = \case
  Num n -> Value n

  Add a b -> bin (+) a b
  Sub a b -> bin (-) a b
  Mul a b -> bin (*) a b

  Div a b ->                         -- pengecekan division by zero
    case eval b of
      Value 0 -> Error "Error: Division by zero"
      Value n ->
        case eval a of
          Value m -> Value (m / n)
          Error e -> Error e
      Error e -> Error e

  Mod a b ->                         -- modulo dengan error handling
    case (eval a, eval b) of
      (_, Value 0)       -> Error "Error: Modulo by zero"
      (Value x, Value y) -> Value (x - y * fromIntegral (floor (x / y)))
      (Error e, _)       -> Error e
      (_, Error e)       -> Error e

  Pow a b -> bin (**) a b

  Abs a -> case eval a of
    Value x -> Value (abs x)
    Error e -> Error e

  Max a b -> bin max a b
  Min a b -> bin min a b

  Floor a -> case eval a of         -- panggil Haskell floor/ceiling
    Value x -> Value (fromIntegral (floor x))
    Error e -> Error e

  Ceil a -> case eval a of
    Value x -> Value (fromIntegral (ceiling x))
    Error e -> Error e

  where
    bin op a b =                    -- helper evaluasi operator biner
      case (eval a, eval b) of
        (Value x, Value y) -> Value (x `op` y)
        (Error e, _) -> Error e
        (_, Error e) -> Error e

-- 8. QuickCheck properties for testing

prop_add_comm :: Double -> Double -> Bool       -- tes: komutativity a + b
prop_add_comm x y =
  eval (Add (Num x) (Num y))
  == eval (Add (Num y) (Num x))

prop_parseFloor =                               -- tes parsing floor()
  parseExpr "floor(3.9)" == Just (Floor (Num 3.9))

prop_parseCeil =                                -- tes parsing ceil()
  parseExpr "ceil(3.1)" == Just (Ceil (Num 3.1))

prop_evalFloor =                                -- tes evaluasi floor()
  eval (Floor (Num 3.9)) == Value 3.0

prop_evalCeil =                                 -- tes evaluasi ceil()
  eval (Ceil (Num 3.1)) == Value 4.0

prop_floorCeilExpr =                            -- tes gabungan floor + ceil
  (eval <$> parseExpr "floor(2.9) + ceil(1.1)") == Just (Value 4.0)

runAllTests :: IO ()                            -- menjalankan semua QuickCheck test
runAllTests = do
  quickCheck prop_parseFloor
  quickCheck prop_parseCeil
  quickCheck prop_evalFloor
  quickCheck prop_evalCeil
  quickCheck prop_floorCeilExpr
  quickCheck prop_add_comm

-- 9. REPL interface
--    Loop input: parse → evaluate → print result atau error.

repl :: IO ()
repl = do
  putStr "expr> "
  hFlush stdout
  line <- getLine
  case line of
    ":quit" -> putStrLn "Goodbye!"               -- command keluar
    ":test" -> runAllTests >> repl               -- jalankan QuickCheck
    _ ->
      case parseExpr line of
        Nothing -> putStrLn "Parse error: invalid expression" >> repl
        Just ast ->
          case eval ast of
            Value v -> print v >> repl
            Error e -> putStrLn e >> repl

-- 10. Main
--     Menampilkan menu dan mulai REPL.

main :: IO ()
main = do
  putStrLn "Expression Evaluator (Week 5)"
  putStrLn "Type any expression to evaluate."
  putStrLn "Commands:"
  putStrLn "  :quit   - exit"
  putStrLn "  :test   - run QuickCheck test"
  repl
