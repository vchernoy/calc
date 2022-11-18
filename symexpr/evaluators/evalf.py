import math
import functools
import symexpr.ast as ast
from symexpr import evaluators


@functools.singledispatch
def evalf(_) -> ast.Node:
    """
    Computes the expression in floating points, but does not change any variables.s
    :param _: AST
    :return: AST
    """
    raise TypeError('cannot evalf {expr}')


@evalf.register
def _one_evalf(expr: ast.One) -> ast.One:
    return expr


@evalf.register
def _add_evalf(expr: ast.Add) -> ast.Node:
    evaluated = [evalf(n) for n in expr.operands]
    val = sum(n.coeff for n in evaluated if n.numeric())
    terms = []
    terms.append(ast.number(val)) if val != 0 else ...
    terms.extend(n for n in evaluated if not n.numeric())

    return ast.add(terms, coeff=expr.coeff, variables=expr.vars)


@evalf.register
def _mul_evalf(expr: ast.Mul) -> ast.Node:
    evaluated = [evalf(n) for n in expr.operands]
    non_scalars = [n for n in evaluated if not n.numeric()]
    val = math.prod(n.coeff for n in evaluated if n.numeric())

    return ast.mul(non_scalars, coeff=expr.coeff * val, variables=expr.vars)


@evalf.register
def _inv_evalf(expr: ast.Inv) -> ast.Node:
    evaluated = [evalf(n) for n in expr.operands]
    non_scalars = [n for n in evaluated if not n.numeric()]
    if non_scalars:
        return ast.inv(non_scalars[0], coeff=expr.coeff, variables=expr.vars)

    scalars = [n for n in evaluated if n.numeric()]
    if scalars[0].coeff == 0:
        return ast.inv(ast.number(0), coeff=expr.coeff, variables=expr.vars)

    val = expr.coeff // scalars[0].coeff
    fval = expr.coeff / scalars[0].coeff
    if val != fval:
        val = fval

    return ast.term(coeff=val, variables=expr.vars)


@evalf.register
def _log_evalf(expr: ast.Log) -> ast.Node:
    evaluated = [evalf(n) for n in expr.operands]
    non_scalars = [n for n in evaluated if not n.numeric()]
    if non_scalars:
        return ast.log(non_scalars[0], coeff=expr.coeff, variables=expr.vars)

    scalars = [n for n in evaluated if n.numeric()]
    if scalars[0].coeff == 0:
        return ast.log(scalars[0], coeff=expr.coeff, variables=expr.vars)

    return ast.term(coeff=expr.coeff * math.log(scalars[0].coeff), variables=expr.vars)


@evalf.register
def _exp_evalf(expr: ast.Exp) -> ast.Node:
    evaluated = [evalf(n) for n in expr.operands]
    non_scalars = [n for n in evaluated if not n.numeric()]
    if non_scalars:
        return ast.log(non_scalars[0], coeff=expr.coeff, variables=expr.vars)

    scalars = [n for n in evaluated if n.numeric()]

    return ast.term(coeff=expr.coeff * math.exp(scalars[0].coeff), variables=expr.vars)

@evalf.register
def _evalf_evalf(expr: ast.Evalf) -> ast.Node:
    return ast.new_with(expr=evaluators.evalf(expr.operands[0]), variables=expr.vars, coeff=expr.coeff)
