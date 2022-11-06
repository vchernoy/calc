import functools
import symexpr.ast as ast
from symexpr.expand import expand


"""
A set of tools that allow to manipulate with AST
"""

@functools.singledispatch
def solve(self, var):
    """
    Tries to solve the equation given in the form of AST.
    solve('2x-10') => ('5', 'x') meaning that x=5 is the root of 2x-10=0
    solve('2x*x-y-10') => ('(10+y)/2', 'x*x') meaning that x*x=(10+y)/2 is the root of 2x*x-y-10=0
    solve('x*x+x') => None -- failed to solve
    :param self: AST
    :param var: to find root for this variable
    :return: a pair (solution as AST, variable as AST) or None if failed to solve
    """
    raise TypeError("cannot solve", self)


@solve.register(ast.One)
def _one_solve(self, var):
    if var not in self.variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Mul)
def _mul_solve(self, var):
    if var not in self.variables():
        return None

    for n in self.operands:
        if var in n.variables():
            return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Log)
def _log_solve(self, var):
    if var not in self.variables():
        return None

    if var in self.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)

@solve.register(ast.Exp)
def _exp_solve(self, var):
    if var not in self.variables():
        return None

    if var in self.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Inv)
def _inv_solve(self, var):
    if var not in self.variables():
        return None

    if var in self.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Add)
def _add_solve(self, var):
    if var not in self.variables():
        return None

    var_terms = [t for t in self.operands if var in t.variables()]
    non_var_terms = [t for t in self.operands if var not in t.variables()]

    if self.vars.get(var, 0) < 0:
        return None

    if (self.vars.get(var, 0) > 0) and var_terms:
        return None

    if not var_terms:
        return ast.number(0), ast.variable(var)

    var_power = set()
    for term in var_terms:
        for t in term.operands:
            if var in t.variables():
                return None

        var_power.add(term.vars[var])

    if len(var_power) != 1:
        return None

    power = var_power.pop()

    reduced_var_terms = []
    for term in var_terms:
        reduced_vars = {}
        ast.incby(reduced_vars, term.vars)
        ast.incby(reduced_vars, {var: -power})
        reduced_var_terms.append(
            ast.new(
                operation=term.operation,
                operands=term.operands,
                coefficient=term.coefficient,
                variables=reduced_vars
            )
        )

    term1 = expand(ast.negative(ast.addition(non_var_terms)))
    term2 = ast.inverse(ast.addition(reduced_var_terms))

    return (
        ast.multiplication([term1, term2]),
        ast.term(coefficient=1, variables={var: power})
    )
