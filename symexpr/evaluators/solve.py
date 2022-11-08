import functools
import symexpr.ast as ast
from symexpr.evaluators.expand import expand


@functools.singledispatch
def solve(expr, var: str) -> ast.Node:
    """
    Tries to solve the equation given in the form of AST.
    solve('2x-10') => ('5', 'x') meaning that x=5 is the root of 2x-10=0
    solve('2x*x-y-10') => ('(10+y)/2', 'x*x') meaning that x*x=(10+y)/2 is the root of 2x*x-y-10=0
    solve('x*x+x') => None -- failed to solve
    :param expr: AST
    :param var: to find root for this variable
    :return: a pair (solution as AST, variable as AST) or None if failed to solve
    """
    raise TypeError(f'cannot solve {expr} over {var}')


@solve.register
def _one_solve(expr: ast.One, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    return ast.number(0), ast.variable(var)


@solve.register
def _mul_solve(expr: ast.Mul, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    for n in expr.operands:
        if var in ast.all_vars(vars):
            return None

    return ast.number(0), ast.variable(var)


@solve.register
def _log_solve(expr: ast.Log, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    if var in ast.all_vars(expr.operands[0]):
        return None

    return ast.number(0), ast.variable(var)

@solve.register
def _exp_solve(expr: ast.Exp, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    if var in ast.all_vars(expr.operands[0]):
        return None

    return ast.number(0), ast.variable(var)


@solve.register
def _inv_solve(expr: ast.Inv, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    if var in ast.all_vars(expr.operands[0]):
        return None

    return ast.number(0), ast.variable(var)


@solve.register
def _add_solve(expr: ast.Add, var: str) -> (ast.Node, ast.Node):
    if var not in ast.all_vars(expr):
        return None

    var_terms = [t for t in expr.operands if var in ast.all_vars(t)]
    non_var_terms = [t for t in expr.operands if var not in ast.all_vars(t)]

    if expr.vars.get(var, 0) < 0:
        return None

    if expr.vars.get(var, 0) > 0 and var_terms:
        return None

    if not var_terms:
        return ast.number(0), ast.variable(var)

    var_power = set()
    for term in var_terms:
        for t in term.operands:
            if var in ast.all_vars(t):
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

    return ast.mul([term1, term2]),\
           ast.term(coefficient=1, variables={var: power})
