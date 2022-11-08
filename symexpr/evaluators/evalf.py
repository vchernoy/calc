import math
import functools
import symexpr.ast as ast


@functools.singledispatch
def evalf(expr):
    """
    Computes the expression in floating points, but does not change any variables.s
    :param expr: AST
    :return: AST
    """
    raise TypeError('cannot evalf {expr}')


@evalf.register(ast.One)
def _one_evalf(expr):
    return expr


@evalf.register(ast.Add)
def _add_evalf(expr):
    evaluated = [evalf(n) for n in expr.operands]
    val = sum(n.coefficient for n in evaluated if n.numeric())
    terms = []
    terms.append(ast.number(val)) if val != 0 else ...
    terms.extend(n for n in evaluated if not n.numeric())

    return ast.add(terms, coefficient=expr.coefficient, variables=expr.vars)


@evalf.register(ast.Mul)
def _mul_evalf(expr):
    evaluated = [evalf(n) for n in expr.operands]
    non_scalars = [n for n in evaluated if not n.numeric()]
    val = math.prod(n.coefficient for n in evaluated if n.numeric())

    return ast.mul(non_scalars, coefficient=expr.coefficient * val, variables=expr.vars)


@evalf.register(ast.Inv)
def _inv_evalf(expr):
    evaluated = [evalf(n) for n in expr.operands]
    scalars = [n for n in evaluated if n.numeric()]
    if scalars:
        if scalars[0].coefficient == 0:
            return ast.inv(ast.number(0), coefficient=expr.coefficient, variables=expr.vars)

        val = expr.coefficient // scalars[0].coefficient
        fval = expr.coefficient / scalars[0].coefficient
        if val != fval:
            val = fval

        return ast.term(coefficient=val, variables=expr.vars)

    non_scalars = [n for n in evaluated if not n.numeric()]

    return ast.inv(non_scalars[0], coefficient=expr.coefficient, variables=expr.vars)


@evalf.register(ast.Log)
def _log_evalf(expr):
    evaluated = [evalf(n) for n in expr.operands]
    scalars = [n for n in evaluated if n.numeric()]
    non_scalars = [n for n in evaluated if not n.numeric()]
    if scalars:
        if scalars[0].coefficient > 0:
            val = expr.coefficient * math.log(scalars[0].coefficient)

            return ast.term(coefficient=val, variables=expr.vars)

        return ast.log(scalars[0], coefficient=expr.coefficient, variables=expr.vars)

    return ast.log(non_scalars[0], coefficient=expr.coefficient, variables=expr.vars)


@evalf.register(ast.Exp)
def _exp_evalf(expr):
    evaluated = [evalf(n) for n in expr.operands]
    scalars = [n for n in evaluated if n.numeric()]
    non_scalars = [n for n in evaluated if not n.numeric()]
    if scalars:
        val = expr.coefficient * math.exp(scalars[0].coefficient)
        return ast.term(coefficient=val, variables=expr.vars)

    return ast.log(non_scalars[0], coefficient=expr.coefficient, variables=expr.vars)

