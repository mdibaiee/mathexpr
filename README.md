mathexpr [![Travis](https://img.shields.io/travis/mdibaiee/mathexpr.svg)]()
========
Ever wanted to take as input a mathematical expression to fill in values and use it? `mathexpr` is exactly what you need.

_I also wrote this in JavaScript some time ago: [Equation](https://github.com/mdibaiee/Equation)_

Examples
--------
Simple evaluation of expressions:

```haskell
import Data.MathExpr

main = do
  expr <- getLine -- a mathematical expression, e.g. sin x + y ^ 2
  -- replace x and y with the specified values and evaluate: sin 2 + 5 ^ 2 = 25.909..
  print $ evaluate def expr [("x", 2), ("y", 5)]
```

Using custom operators and functions:

```haskell
import Data.MathExpr


-- operators are in the form (character, precedence, function)
-- example: ('+', 0, (+)), ('*', 1, (*))
-- the function should have the type (Double -> Double -> Double)
-- the higher the precedence, the sooner the operator operates

-- functions are in the form (name, function)
-- example: ("ln", log)
-- the function should have the type (Double -> Double)

main =
  let avg a b = (a + b) / 2
  let settings = Settings { operators = defaultOperators ++ [('~', 0, avg)]
                          , functions = defaultFunctions ++ [("trunc", fromIntegral . truncate)]
  evaluate settings "3 ~ 5" [] -- 4
  evaluate settings "trunc 1.1" [] -- 1
```
