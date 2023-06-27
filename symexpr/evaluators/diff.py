import collections
import functools
from typing import Callable

import symexpr.ast as ast
from symexpr import evaluators


@functools.singledispatch
def diff(expr, var: str) -> ast.Node:
    """
    Opens parentheses if meets a(b + x)
    :param expr: AST
    :param var: str
    :return: AST
    """
    raise TypeError(f'cannot expand {expr}')


@diff.register
def one_diff(expr: ast.One, var: str) -> ast.One:
    vars = collections.Counter(expr.vars)
    p = vars[var]
    vars[var] = p-1

    return ast.term(coeff=expr.coeff*p, variables=vars)

@diff.register
def add_diff(expr: ast.Add, var: str) -> ast.One:
    vars = collections.Counter(expr.vars)
    p = vars[var]
    vars[var] = p-1

    return ast.term(coeff=expr.coeff*p, variables=vars)
