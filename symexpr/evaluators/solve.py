import functools
import symexpr.ast as ast
from symexpr.evaluators.expand import expand


@functools.singledispatch
def solve(expr, var):
    """
    Tries to solve the equation given in the form of AST.
    solve('2x-10') => ('5', 'x') meaning that x=5 is the root of 2x-10=0
    solve('2x*x-y-10') => ('(10+y)/2', 'x*x') meaning that x*x=(10+y)/2 is the root of 2x*x-y-10=0
    solve('x*x+x') => None -- failed to solve
    :param expr: AST
    :param var: to find root for this variable
    :return: a pair (solution as AST, variable as AST) or None if failed to solve
    """
    raise TypeError('cannot solve {expr} over {var}')


@solve.register(ast.One)
def _one_solve(expr, var):
    if var not in expr.variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Mul)
def _mul_solve(expr, var):
    if var not in expr.variables():
        return None

    for n in expr.operands:
        if var in n.variables():
            return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Log)
def _log_solve(expr, var):
    if var not in expr.variables():
        return None

    if var in expr.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)

@solve.register(ast.Exp)
def _exp_solve(expr, var):
    if var not in expr.variables():
        return None

    if var in expr.operands[0].variables():
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
def _add_solve(expr, var):
    if var not in expr.variables():
        return None

    var_terms = [t for t in expr.operands if var in t.variables()]
    non_var_terms = [t for t in expr.operands if var not in t.variables()]

    if expr.vars.get(var, 0) < 0:
        return None

    if (expr.vars.get(var, 0) > 0) and var_terms:
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

    term1 = expand(ast.neg(ast.add(non_var_terms)))
    term2 = ast.inv(ast.add(reduced_var_terms))

    return (
        ast.mul([term1, term2]),
        ast.term(coefficient=1, variables={var: power})
    )
