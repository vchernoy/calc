import math
import symexpr.ast as ast


def subs(node, assignment: dict[str, ast.Num]):
    """
    Substitute the values from the map into the variables of the given AST.
    subs('2x+1', x=3) => '2*3+1'
    :param node: AST
    :param assignment: a mapping between free variables and their numerical values
    :return: AST
    """
    evaluated = [subs(n, assignment) for n in node.operands]
    res_coefficient = math.prod(
        (assignment[v] ** p for v, p in node.vars.items() if v in assignment),
        start=node.coefficient
    )
    res_var = {v: p for v, p in node.vars.items() if v not in assignment}

    return ast.new(operation=node.operation, operands=evaluated, coefficient=res_coefficient, variables=res_var)


def subse(node, assignment: dict[str, ast.Node]):
    """
    Substitute the values (given as ASTs) from the map into the variables of the given AST
    subse('2x+1', x=3y+1) => '2*(3y+1)+1'
    :param node: AST
    :param assignment: a mapping between free variables and their AST values
    :return: AST
    """
    evaluated = [subse(n, assignment) for n in node.operands]
    nodes = []
    for v, p in node.vars.items():
        if v in assignment:
            nodes.extend([assignment[v]] * p)

    res_var = {v: p for v, p in node.vars.items() if v not in assignment}

    node = ast.new(operation=node.operation, operands=evaluated, coefficient=node.coefficient, variables=res_var)
    return ast.multiplication(nodes + [node])
