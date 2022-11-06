import math
import functools
import symexpr.ast as ast


"""
A set of tools that allow to manipulate with AST
"""

@functools.singledispatch
def evalf(self):
    """
    Computes the expression in floating points, but does not change any variables.s
    :param self: AST
    :return: AST
    """
    raise TypeError("cannot evalf", self)


@evalf.register(ast.One)
def _one_evalf(self):
    return self


@evalf.register(ast.Add)
def _add_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    val = sum(n.coefficient for n in scalars)
    terms = []
    if val != 0:
        terms.append(ast.number(val))

    terms += non_scalars

    return ast.addition(terms, coefficient=self.coefficient, variables=self.vars)


@evalf.register(ast.Mul)
def _mul_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    val = math.prod(n.coefficient for n in scalars)

    return ast.multiplication(non_scalars, coefficient=self.coefficient * val, variables=self.vars)


@evalf.register(ast.Inv)
def _inv_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    if scalars:
        if scalars[0].coefficient == 0:
            return ast.inverse(ast.number(0), coefficient=self.coefficient, variables=self.vars)

        val = self.coefficient // scalars[0].coefficient
        fval = self.coefficient / scalars[0].coefficient
        if val != fval:
            val = fval

        return ast.term(coefficient=val, variables=self.vars)

    return ast.inverse(non_scalars[0], coefficient=self.coefficient, variables=self.vars)


@evalf.register(ast.Log)
def _log_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    if scalars:
        if scalars[0].coefficient > 0:
            val = self.coefficient * math.log(scalars[0].coefficient)

            return ast.term(coefficient=val, variables=self.vars)

        return ast.logarithm(scalars[0], coefficient=self.coefficient, variables=self.vars)

    return ast.logarithm(non_scalars[0], coefficient=self.coefficient, variables=self.vars)


@evalf.register(ast.Exp)
def _exp_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    if scalars:
        val = self.coefficient * math.exp(scalars[0].coefficient)
        return ast.term(coefficient=val, variables=self.vars)

    return ast.logarithm(non_scalars[0], coefficient=self.coefficient, variables=self.vars)

