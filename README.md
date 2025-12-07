# Functional Expression Evaluator 💻

A small Haskell project that parses and evaluates mathematical expressions. It supports arithmetic operators, functions, variables, numeric differentiation, and numeric integration using common numerical methods.

This project includes:
- A custom parser built from scratch (no external parsing libraries).
- An evaluator for expressions with correct operator precedence.
- A safe evaluation mode that rejects free variables.
- Numeric derivative and numeric integral support.
- A REPL interface.
- QuickCheck property-based tests.

# Features ⚡

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

- deriv(f, point): numeric derivative using central difference
- integral(f, a, b): numeric definite integral using composite Simpson’s Rule

Variable:
- x can be used in expressions of functions for calculus operations.

# How It Works (High-Level): ⚙️

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

Expressions that contain a free variable `x` are rejected unless they appear inside `deriv(...)` or `integral(...)`.

Why free variable check exists:

To prevent ambiguous evaluations like evaluating “`x + 5`” without a specific value.
Users must explicitly evaluate functions inside calculus operators.

4. Numeric Derivative

This project implements numeric differentiation.
That means the system does not rewrite expressions (e.g., it does not convert `(x^2) + 1` into `2x`).
Instead, the derivative is approximated by evaluating the function around a point using a small step size.

Internally, the evaluator treats the expression as a function `f(x)` and computes:   
- `f(x + h)`
- `f(x − h)`

Then it combines both values using the central difference formula, which provides a stable and accurate numerical approximation.

Because this approach works purely by sampling the function, it can differentiate any valid expression, even those that would be complex to differentiate symbolically.

This method is simple, robust, and consistent with the evaluator’s functional design.

5. Numeric Integral

The integration feature also uses numeric integration, not symbolic anti-derivatives.
Thus the system does not compute results like turning `x^2` into `(x^3) / 3`.
Instead, it approximates the area under the curve of the given expression.

The algorithm used is Composite Simpson’s Rule, which:

- Splits the interval `[a, b]` into many small even segments.

- Approximates each pair of segments with a parabola.

- Accumulates the weighted sum of `f(x)` at finely sampled points.

By repeatedly evaluating the expression at many points, the method yields a highly accurate approximation for the definite integral, even when the symbolic integral is complicated or impossible to express in closed form.

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

- `:quit` - exit
- `:test` - run QuickCheck tests

# Example Inputs and Outputs 🎯
Arithmetic  :  
`expr> 1 + 2 * 3`
> 7.0

Functions:  
`expr> max(10, 3)`
> 10.0

Derivative:  
`expr> deriv(x^2, 3)`
> 6.000000000838668

Integral:  
`expr> integral(x^2, 0, 1)`  
> 0.33333333333333315

Free variable error:  
`expr> x + 10`

> Error: expression contains free variable 'x'; use deriv/integral with numeric points or evaluate with evalAt

# Running the Project 🦾

Clone the repository: 

`git clone https://github.com/johaneswisa/proyek-funcpro.git`

`cd proyek-funcpro`

Run using Cabal: 

`cabal run`


You should see:

```
Expression Evaluator  
Examples:  
  deriv(x^2, 3)  
  integral(x^2, 0, 1)  
Commands: :quit, :test  
expr>
```