import math
import itertools
import symexpr.ast as ast


def subs(expr: ast.Node, assignment: dict[str, ast.Num]) -> ast.Node:
    """
    Substitute the values from the map into the variables of the given AST.
    subs('2x+1', x=3) => '2*3+1'
    :param expr: AST
    :param assignment: a mapping between free variables and their numerical values
    :return: AST
    """
    evaluated = [subs(n, assignment) for n in expr.operands]
    free_vars = {v: p for v, p in expr.vars.items() if v not in assignment}

    return ast.new(
        operation=expr.operation,
        operands=evaluated,
        coefficient=math.prod(
            (assignment[v] ** p for v, p in expr.vars.items() if v in assignment),
            start=expr.coefficient
        ),
        variables=free_vars
    )


def subse(expr: ast.Node, assignment: dict[str, ast.Node]) -> ast.Node:
    """
    Substitute the values (given as ASTs) from the map into the variables of the given AST
    subse('2x+1', x=3y+1) => '2*(3y+1)+1'
    :param expr: AST
    :param assignment: a mapping between free variables and their AST values
    :return: AST
    """
    evaluated = [subse(n, assignment) for n in expr.operands]
    free_vars = {v: p for v, p in expr.vars.items() if v not in assignment}

    return ast.mul(
        list(itertools.chain.from_iterable([assignment[v]] * p for v, p in expr.vars.items() if v in assignment)) +
        [
            ast.new(
                operation=expr.operation,
                operands=evaluated,
                coefficient=expr.coefficient,
                variables=free_vars
            )
        ]
    )
