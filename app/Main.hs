-- Functional Expression Evaluator
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Control.Applicative (Alternative(..))
import System.IO                   -- for REPL input/output
import Test.QuickCheck  hiding (Result)
import qualified Test.QuickCheck as QC

-- 1. Abstract Syntax Tree (AST)
data Expr
  = Num Double                    -- literal number
  | Var                           -- variable x
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
  | Deriv Expr Expr               -- deriv(f, point)  -- numeric derivative of f at point
  | Integral Expr Expr Expr       -- integral(f, a, b) -- definite integral from a to b
  deriving (Show, Eq)

-- 2. Result type
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

-- 4. Basic utilities for building parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  (x:xs) | f x -> Just (x,xs)
  _            -> Nothing

charP :: Char -> Parser Char
charP = satisfy . (==)

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (tok, rest) = span f input in Just (tok, rest)

ws :: Parser String
ws = spanP isSpace

doubleP :: Parser Double
doubleP = fmap read $ ws *> number <* ws
  where
    number =
      (++) <$> some (satisfy isDigit)
           <*> ( ((:) <$> charP '.' <*> some (satisfy isDigit)) <|> pure "" )

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
  <|> (Var <$ (ws *> stringP "x" <* ws)) -- parse variable 'x'
  <|> (charP '(' *> expr <* charP ')')

addop :: Parser (Expr -> Expr -> Expr)
addop =
      (Add <$ charP '+')
  <|> (Sub <$ charP '-')

mulop :: Parser (Expr -> Expr -> Expr)
mulop =
      (Mul <$ charP '*')
  <|> (Div <$ charP '/')
  <|> (Mod <$ charP '%')

-- chainl1: left associative
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

-- chainr1: right associative
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 pure (f x y))
             <|> pure x

-- 6. Function parser (including deriv and integral)
-- NOTE: 'a' is first argument, m2 is optional second, m3 optional third.
funcP :: Parser Expr
funcP = do
  fname <- identP
  charP '('
  -- parse first arg
  a <- expr
  -- attempt to parse second arg
  m2 <- (charP ',' *> expr >>= \b -> pure (Just b)) <|> pure Nothing
  -- attempt to parse third arg only if second was present
  m3 <- case m2 of
          Just _  -> (charP ',' *> expr >>= \c -> pure (Just c)) <|> pure Nothing
          Nothing -> pure Nothing
  charP ')'
  case (fname, m2, m3) of
    ("abs", Nothing, Nothing)      -> pure (Abs a)
    ("max", Just b, Nothing)       -> pure (Max a b)
    ("min", Just b, Nothing)       -> pure (Min a b)
    ("floor", Nothing, Nothing)    -> pure (Floor a)
    ("ceil", Nothing, Nothing)     -> pure (Ceil a)
    ("deriv", Just p, Nothing)     -> pure (Deriv a p)                 -- deriv(f, point)
    ("integral", Just lower, Just upper) -> pure (Integral a lower upper) -- integral(f, a, b)
    _                              -> empty

-- 7. Evaluator
--    Primary evaluation (old eval) will error if there is an unbound variable.
--    For calculus operators we implement numeric routines that evaluate functions at numeric points.

-- helper: detect presence of Var in Expr
hasVar :: Expr -> Bool
hasVar = \case
  Var -> True
  Num _ -> False
  Add a b -> hasVar a || hasVar b
  Sub a b -> hasVar a || hasVar b
  Mul a b -> hasVar a || hasVar b
  Div a b -> hasVar a || hasVar b
  Mod a b -> hasVar a || hasVar b
  Pow a b -> hasVar a || hasVar b
  Abs a -> hasVar a
  Max a b -> hasVar a || hasVar b
  Min a b -> hasVar a || hasVar b
  Floor a -> hasVar a
  Ceil a -> hasVar a
  Deriv f p -> hasVar f || hasVar p
  Integral f a b -> hasVar f || hasVar a || hasVar b

-- evaluate expression when variable x has value xv
evalAt :: Expr -> Double -> Result
evalAt e xv = case e of
  Num n -> Value n
  Var -> Value xv
  Add a b -> bin (+) a b
  Sub a b -> bin (-) a b
  Mul a b -> bin (*) a b
  Div a b ->
    case evalAt b xv of
      Value 0 -> Error "Error: Division by zero"
      Value n ->
        case evalAt a xv of
          Value m -> Value (m / n)
          Error e -> Error e
      Error e -> Error e
  Mod a b ->
    case (evalAt a xv, evalAt b xv) of
      (_, Value 0)       -> Error "Error: Modulo by zero"
      (Value x, Value y) -> Value (x - y * fromIntegral (floor (x / y)))
      (Error e, _)       -> Error e
      (_, Error e)       -> Error e
  Pow a b -> bin (**) a b
  Abs a -> case evalAt a xv of
             Value x -> Value (abs x)
             Error e -> Error e
  Max a b -> bin max a b
  Min a b -> bin min a b
  Floor a -> case evalAt a xv of
               Value x -> Value (fromIntegral (floor x))
               Error e -> Error e
  Ceil a -> case evalAt a xv of
               Value x -> Value (fromIntegral (ceiling x))
               Error e -> Error e
  Deriv f p ->                              -- numeric derivative of f at point p
    case evalAt p xv of
      Error e -> Error e
      Value pt -> numericDerivative f pt
  Integral f a b ->
    case (evalAt a xv, evalAt b xv) of
      (Value aa, Value bb) -> numericIntegral f aa bb
      (Error e, _) -> Error e
      (_, Error e) -> Error e
  where
    bin op a b =
      case (evalAt a xv, evalAt b xv) of
        (Value x, Value y) -> Value (x `op` y)
        (Error e, _) -> Error e
        (_, Error e) -> Error e

-- eval: handle calculus forms first, then fallback to evalAt (reject free vars otherwise)
eval :: Expr -> Result
-- handle Deriv and Integral explicitly (they may contain Var inside f)
eval (Deriv f p) =
  case eval p of
    Value pt -> numericDerivative f pt
    Error e  -> Error e

eval (Integral f a b) =
  case (eval a, eval b) of
    (Value aa, Value bb) -> numericIntegral f aa bb
    (Error e, _) -> Error e
    (_, Error e) -> Error e

-- everything else: reject free variable unless it's fully numeric
eval e
  | hasVar e = Error "Error: expression contains free variable 'x'; use deriv/integral with numeric points or evaluate with evalAt"
  | otherwise = evalAt e 0

-- numeric derivative helper (central difference)
numericDerivative :: Expr -> Double -> Result
numericDerivative f pt =
  let h = 1e-6
      evalPoint x = case evalAt f x of
                      Value v -> Right v
                      Error e -> Left e
  in case (evalPoint (pt + h), evalPoint (pt - h)) of
       (Right v1, Right v2) -> Value ((v1 - v2) / (2 * h))
       (Left e, _) -> Error e
       (_, Left e) -> Error e

-- numeric integral helper (composite Simpson's rule)
numericIntegral :: Expr -> Double -> Double -> Result
numericIntegral f a b
  | a == b = Value 0.0
  | otherwise =
    let n = 1000                       -- must be even
        n' = if even n then n else n+1
        h = (b - a) / fromIntegral n'
        x i = a + fromIntegral i * h
        evalX i = case evalAt f (x i) of
                    Value v -> Right v
                    Error e -> Left e
        indices = [0 .. n']
        -- Simpson weights: 1,4,2,4,...,4,1
        weight i
          | i == 0 || i == n' = 1
          | even i = 2
          | otherwise = 4
        accumulate acc i =
          case (acc, evalX i) of
            (Left e, _) -> Left e
            (_, Left e) -> Left e
            (Right s, Right v) -> Right (s + fromIntegral (weight i) * v)
    in case foldl accumulate (Right 0.0) indices of
         Left e -> Error e
         Right s -> Value (h / 3 * s)

-- 8. QuickCheck properties for testing (extended for calculus)
prop_add_comm :: Double -> Double -> Bool
prop_add_comm x y =
  eval (Add (Num x) (Num y))
  == eval (Add (Num y) (Num x))

prop_parseFloor = parseExpr "floor(3.9)" == Just (Floor (Num 3.9))
prop_parseCeil  = parseExpr "ceil(3.1)"  == Just (Ceil (Num 3.1))
prop_evalFloor  = eval (Floor (Num 3.9)) == Value 3.0
prop_evalCeil   = eval (Ceil (Num 3.1))  == Value 4.0

-- calculus tests:
-- parse deriv(x^2, 3)
prop_parseDeriv = parseExpr "deriv(x^2,3)" == Just (Deriv (Pow Var (Num 2.0)) (Num 3.0))
-- parse integral(x^2,0,1)
prop_parseIntegral = parseExpr "integral(x^2,0,1)" == Just (Integral (Pow Var (Num 2.0)) (Num 0.0) (Num 1.0))

-- numeric checks with approximate equality
approxEqual :: Double -> Double -> Double -> Bool
approxEqual eps a b = abs (a - b) <= eps

prop_evalDeriv_x2_at3 :: Bool
prop_evalDeriv_x2_at3 =
  case parseExpr "deriv(x^2,3)" of
    Just ast ->
      case eval ast of
        Value v -> approxEqual 1e-3 v 6.0     -- derivative of x^2 at 3 is 6
        _ -> False
    _ -> False

prop_evalIntegral_x2_0_1 :: Bool
prop_evalIntegral_x2_0_1 =
  case parseExpr "integral(x^2,0,1)" of
    Just ast ->
      case eval ast of
        Value v -> approxEqual 1e-4 v (1/3)   -- integral_0^1 x^2 dx = 1/3
        _ -> False
    _ -> False

runAllTests :: IO ()
runAllTests = do
  quickCheck prop_parseFloor
  quickCheck prop_parseCeil
  quickCheck prop_evalFloor
  quickCheck prop_evalCeil
  quickCheck prop_add_comm
  quickCheck prop_parseDeriv
  quickCheck prop_parseIntegral
  quickCheck prop_evalDeriv_x2_at3
  quickCheck prop_evalIntegral_x2_0_1

-- 9. REPL interface
repl :: IO ()
repl = do
  putStr "expr> "
  hFlush stdout
  line <- getLine
  case line of
    ":quit" -> putStrLn "Goodbye!"
    ":test" -> runAllTests >> repl
    _ -> case parseExpr line of
           Nothing -> putStrLn "Parse error: invalid expression" >> repl
           Just ast ->
             case eval ast of
               Value v -> print v >> repl
               Error e -> putStrLn e >> repl

-- 10. Main
main :: IO ()
main = do
  putStrLn "Expression Evaluator (Week 6) - calculus operators added"
  putStrLn "Examples:"
  putStrLn "  deriv(x^2, 3)           -- numeric derivative of x^2 at x=3 -> ~6"
  putStrLn "  integral(x^2, 0, 1)     -- numeric integral from 0 to 1 -> ~1/3"
  putStrLn "Commands: :quit, :test"
  repl
