
# Interactive Calculator

The tool is implemented in Python 3, and it uses only the standard library.
In order to run it, invoke:

```sh
python calc.py
```

To run unit test, invoke:

```sh
python -m unittest tests.basic_tests
```

### Libraries Used

* `enum`
* `functools`
* `math`
* `random`
* `string`

### File Organization

* `calc.py` -- the interactive calculator.
* `symexpr/`
  -   `ast.py`  -- defines the Abstract Syntax Tree, which may represent symbolic arithmetic expression.
  -   `tokenizer.py` -- the parser's front-end, it processes the input and generates a stream of tokens, like num, id, left paren, add, etc... 
  -   `parser.py` -- parsers the stream of tokens and generates the AST.
  -   `evaluator.py` -- a group of tools to manipulate with AST, for example, functions that simplify or expand the AST (expression).
* `tests/`
  -   `basic_tests.py` -- simple `unit/integration-tests`

Such structure has better decoupling of modules, so for example, 
ast-module could be used with other algorithms, 
or we can use different the lexer (front-end parser), 
or we can develop new algorithm for evaluating/simplifying the AST.

### Example of Running

Let's first run it and on the examples explain its capabilities.
Here, we can show partial output, 
in order to focus on most important information.

```
input > 2 + 3
simplified: 5
```

```
input > (2 + 1/5) * (2 - 1/5) + 1/25
simplified: 4.0
```

```
input > 2+3x=x*(5-2)
simplified: 2
```

```
input > 2+3x - x*(5-2)
simplified: 2
```

The calculator can simplify arithmetic expressions, 
as well as ones containing free variables. 
The parser replaces an expression in the form A = B to the equivalent form of A - B, 
and then calculator invokes different simplification algorithms.
It has 3 main ways that it tries to apply each of them one by one:

- `simplify()` -- simplifies products and sums without expanding parentheses. 
- `expand()` -- opens parentheses and transforms `a(b+c)` to `ab + ac`.
- `evalf()` -- compute any numerical expressions approximately.

If a function cannot apply some transformation, 
it leaves the expression as is, so it allows, 
for example, apply `evalf('(0.5+2)*x+x')` that gives `'2.5x+x'`, 
which then could be simplified by other functions to the desired `'3.5x'`.

```
input > (0.5+2)*x+x
simplified: 3.5x
soution x = 0
```

```
input > 2(1+2x)=x*(5-2)
simplified: 2+x
solution: x=-2
```

```
input > (4x + 2) / 2 = x
simplified: 1+x
solution: x = -1
```

```
input > (5x + 2) / 2 = x
simplified: 1+(1.5x)
solution: x = -0.666666666667
```

```
input > (5x*x + 2x) / x = x + log 5
simplified: 2+(-log 5)+(4x)
solution: x = ((-2)+(log 5))*(1/4)
approximate solution: x = -0.09764052189147493
```

Here, we could see that when `calc.py` finds a variable in the simplified expression, 
it considers it as an equation that should be solved.

It does support division and logarithm in this mode, while its capabilities are very limited.
The solver-algorithm is looking for well-known patterns of linear equation and trys to solve it.

```
input > (x-1)*(x+1) = 1
simplified: (-2)+x*x
solution: x*x = 2
```

```
input > x* log 5 = 3*log5 +10/log 3
simplified: (-3log 5)+(-10/(log 3))+(x*log 5)
solution: x = 8.655634303097777
substitutes the solution: 1.7763568394002505e-15
```

Whenever possible it tries to solve very primitive none-linear equations 
(while without full support of power it is impossible 
to present the solution in the form of x=value). 

Also, it tries to self-check by substituting the solution into 
the original expression and then simplifying it in order to get 0 
(or to get very close to zero value).

```
input > x*x*x*z + 5y*x - 2 - x * (b*b+5a) - ((-2)+(5*x*y)+((-5)*a*x)+(-(b*x*b))+(x*z*x*x))
simplified: 0
```

```
input > 2x*y + y = 4
simplified: (-4)+y+(2x*y)
solution: x = (-1/2)+(4/(2y))
approximate solution: x = (-0.5)+(4/(2y))
substitutes the solution to the original expression: 0
```

```
input > 2(a*x-5/z)=4/z
simplified: (2a*x)+(-10/z)+(-4/z)
solution: x = (10/(2a*z))+(4/(2a*z))
substitutes the solution to the original expression: (20/(2z))+(8/(2z))+(-10/z)+(-4/z)
there are still free variables {'z'}
the generated assignment to be substituted is {'z': -993.7582746474194}
random assignment gives 0
```

```
input > exp(((b+a)*(a+1) - a*b - a*a))
parsed expression: exp (((b+a)*(a+1))+(-a*b)+(-a*a))
simplified expression: (exp (-a*b))*(exp (-a*a))*(exp ((b+a)*(1+a)))
```

```
input > expand(exp(((b+a)*(a+1) - a*b - a*a)))
parsed expression: expand (exp (((b+a)*(a+1))+(-a*b)+(-a*a)))
simplified expression: (exp b)*(exp a)
```

```
input > exp (((3+a)*(a+1) - a*3 - a*a - a))
parsed expression: exp (((3+a)*(a+1))+(-a*3)+(-a*a)+(-a))
simplified expression: (exp (-4a))*(exp (-a*a))*(exp ((3+a)*(1+a)))
```

```
input > exp (((3+a)*(a+1) - a*3 - a*a))
parsed expression: exp (((3+a)*(a+1))+(-a*3)+(-a*a))
simplified expression: (exp (-3a))*(exp (-a*a))*(exp ((3+a)*(1+a)))
```

```
input > exp( expand (((3+a)*(a+1) - a*3 - a*a)) )
parsed expression: exp (expand (((3+a)*(a+1))+(-a*3)+(-a*a)))
simplified expression: (exp 3)*(exp a)
```

```
input > evalf( exp( expand (((3+a)*(a+1) - a*3 - a*a)) ) )
parsed expression: evalf (exp (expand (((3+a)*(a+1))+(-a*3)+(-a*a))))
simplified expression: 20.085536923187668exp a
```

```
input > exp expand (((3+a)*(a+1) - a*3 - a*a - a))
parsed expression: exp (expand (((3+a)*(a+1))+(-a*3)+(-a*a)+(-a)))
simplified expression: exp 3
approximate evaluation: 20.085536923187668
```

```
input > evalf( exp( expand (((3+a)*(a+1) - a*3 - a*a - a)) ) )
parsed expression: evalf (exp (expand (((3+a)*(a+1))+(-a*3)+(-a*a)+(-a))))
simplified expression: 20.085536923187668
```

It also can handle expressions (and equations) of multiple free variable.
It runs the same algorithms to simplify/expand/evaluate and then, 
if the expression still has variables, 
it treats it as an equation over single variable (`x`, `y`, `z`, `a`, `b`, `c` are more preferable 
when multiple variables exist). 
If it cannot check the solution (hard to simplify the substitution), 
it generates random assignments to the free variables and calculates the result.

```
input 1 > 2 + x * / 3
2 + x * / 3
        ^   
error @ 8: unexpected token: /, expected tokens: '(',id,num
simplified: 2+(3x)
```

```
input > 2x*x = 4x
simplified: (-4x)+(2x*x)
cannot solve the equation over x
```

It detects invalid input or cases when it cannot solve equations. 
It points to the wrong token and prints the expected input.

## Expression Language

For describing the grammar and the regular expressions, 
we use EBNF syntax notation consisting of: '|', '[', ']', '(', ')', '+', '*', ':=', '-'.

### The Lexer

Let's first define num and id lexemes:

* num := [`0`-`9`]+ [`.`(`0`-`9`)* [`E`|`e`] (`0`-`9`)+]
* id := [`a`-`z`]+
 
### The Grammar

* Expr := E [`=` E]
* E := S
* S := [`+`|`-`] P ((`+`|`-`) P)*
* P := num ((`*`|`/`) P)*
* P := num (id | F | PE) ((`*`|`/`) P)*
* P := (id | F | PE) ((`*`|`/`) P)*
* F := `log` SP
* F := `exp` SP
* F := `expand` SP
* F := `evalf` SP
* F := `diff` DA
* SP := num [id | F]
* SP := id | F | PE
* PE := `(` E `)`
* DA := `(` E `,` id `)`

Examples:

The grammar recognises the following expressions: 

* `2y`, `2(1+3x)`, `2x*y`, `y*(1+z*5)`,
* `2 log 5`, `log x`, `log log 2x`, `log(2x + 1)`.
* Note that `log 2 * x` is recognized to `(log 2) * x`.

It is easy to represent the grammar as LL(1), 
which allows constructing very simple recursive (but without backtracking) parser 
that looks ahead only one symbol/token.

Our `Recursive Descent Parser` consists of a group of functions:

* `parse()` -- recognizes `Expr`
* `parse_expr()` -- recognizes `E`
* `parse_sum()` -- recognizes `S`
* `parse_product()` -- recognizes `P`
* `parse_short_product()` -- recognizes `SP`
* `parse_expr_in_parenthesis()` -- recognizes `PE`

## The AST

The typical definition of AST consists of the following types of nodes: 
* `Add`, `Sub`, `Mul`, `Div` for binary arithmetic operations, 
* `Const` for scalar, 
* `Var` for representing a single variable, 
* `Log` for logarithm.
* `Exp` for exponent.

As example, the following expression: `2x*x*(x+1-y)` will be represented as

* `Mul(Const(2), Mul(Mul(Var(x), Var(x)), Add(Var(x), Add(Const(1), Sub(Var(y))))))`

While such AST is very easy to construct, 
it is not very friendly for manipulations and simplifications. 
As result, I designed different AST that has fewer types of nodes, 
but each node is more powerful: `Add`, `Mul`, `Inv`, `One`, `Log`. 

Each node contains a scalar and a group of variables (with their exponents) 
that forms terms. 
`Mul` and `Add` also contain a list of children that the operation applies to,
while `Inv` (inversion or 1/...), `Log`, and `Exp` has only one child. 
The type `One` node is the simplest one, it has no children.

Then basically `5x*x*y` could be represented by one node: 
`One(5,x:2,y:1)`, and the above expression is represented in the following form:

* `Add(2, x:2, [One(1, x:1), One(1), One(-1, y:1)])`

which is much more compact and much easier to manipulate with, 
for examples, it is easy to find similar terms and combine them into one.

