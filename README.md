# Functional Expression Evaluator

A small Haskell project that parses and evaluates mathematical expressions. It supports arithmetic operators, functions, variables, numeric differentiation, and numeric integration using common numerical methods.

This project includes:
- A custom parser built from scratch (no external parsing libraries).
- An evaluator for expressions with correct operator precedence.
- A safe evaluation mode that rejects free variables.
- Numeric derivative and numeric integral support.
- A REPL interface.
- QuickCheck property-based tests.

# Features

Supported Operators: 
- Addition, subtraction, multiplication, division, modulo
- Exponentiation
- Parentheses

Supported Built-in Functions:
- abs
- max(a, b)
- min(a, b)
- floor
- ceil

Calculus Operations:

- deriv(f, point) — numeric derivative using central difference
- integral(f, a, b) — numeric definite integral using composite Simpson’s Rule

Variable:
- x can be used in expressions of functions for calculus operations.

# How It Works (High-Level):

1. Abstract Syntax Tree (AST)

Defines the structure of all expressions that can be parsed.
Every expression is represented as a tree (e.g., Add, Sub, Mul, Pow, Abs, Var, Num, etc.).

2. Parser

A small custom parser combinator library built using only Functor, Applicative, Monad, and Alternative.

Responsibilities:

- Tokenizing numbers, identifiers, parentheses, operators.
- Parsing expressions with correct operator precedence.
- Parsing built-in functions with one to three arguments.
- Producing an AST or parse failure.

3. Evaluator

Evaluates an AST and returns either:

- a numeric value, or
- an error string

It includes:

- Arithmetic evaluation
- Function evaluation
- Safe handling of division by zero and modulo by zero
- Numeric derivative
- Numeric integral

Expressions that contain a free variable x are rejected unless they appear inside deriv(...) or integral(...).

Why free variable check exists:

To prevent ambiguous evaluations like evaluating “x + 5” without a specific value.
Users must explicitly evaluate functions inside calculus operators.

4. Numeric Derivative

Computes a numeric approximation using a small step size.
Internally uses two sample evaluations around the given point.

5. Numeric Integral

Computes the numeric approximation using Composite Simpson’s Rule.
Internally splits the interval into many small segments and accumulates weighted evaluations.

6. QuickCheck Tests

The project includes property-based tests for:

- Parser correctness
- Evaluator correctness
- Specific calculus operations

Examples:

- Addition is commutative
- floor and ceil parse and evaluate correctly
- Derivative and integral for x squared are approximately correct

Run tests using the :test command inside the REPL.

7. REPL

Interactive console environment where you can type expressions and get results.

Special commands:

- :quit — exit
- :test — run QuickCheck tests

# Example Inputs and Outputs
Arithmetic
expr> `1 + 2 * 3`
> 7

Functions
expr> `max(10, 3)`
> 10

Derivative
expr> `deriv(x^2, 3)`
> 6.000000000838668

Integral
expr> `integral(x^2, 0, 1)`
> 0.33333333333333315

Free variable error
expr> x + 10

> Error: expression contains free variable 'x'; use deriv/integral with numeric points or evaluate with evalAt

# Running the Project

Clone the repository: 

`git clone https://github.com/johaneswisa/proyek-funcpro.git`

`cd proyek-funcpro`

Run using Cabal: 

`cabal run`


You should see:

Expression Evaluator  
Examples:  
deriv(x^2, 3)  
integral(x^2, 0, 1)  
Commands: :quit, :test  
expr>