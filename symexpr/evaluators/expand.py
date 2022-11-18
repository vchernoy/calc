import functools
import symexpr.ast as ast
from symexpr.evaluators.simplify import simplify


@functools.singledispatch
def expand(expr) -> ast.Node:
    """
    Opens parentheses if meets a(b + x)
    :param expr: AST
    :return: AST
    """
    raise TypeError(f'cannot expand {expr}')


@expand.register
def one_expand(expr: ast.One) -> ast.Node:
    return expr


@expand.register(ast.Inv)
@expand.register(ast.Log)
@expand.register(ast.Exp)
def _expand(expr) -> ast.Node:
    evaluated = [expand(n) for n in expr.operands]
    return ast.new(
        operation=expr.operation,
        variables=expr.vars,
        operands=evaluated,
        coeff=expr.coeff
    )


@expand.register
def _add_expand(expr: ast.Add) -> ast.Node:
    term = ast.term(coeff=expr.coeff, variables=expr.vars)
    expanded = [expand(n) for n in expr.operands]

    node = simplify(ast.add(operands=expanded))
    if node.operation != ast.OpKind.add:
        return simplify(ast.mul(operands=[term, node]))

    term1 = ast.term(coeff=node.coeff, variables=node.vars)
    terms = [simplify(ast.mul(operands=[term, t, term1])) for t in node.operands]

    return simplify(ast.add(terms))


@expand.register
def _mul_expand(expr: ast.Mul) -> ast.Node:
    term: ast.Node = ast.term(coeff=expr.coeff, variables=expr.vars)
    expanded: list[ast.Node] = [term] + [expand(n) for n in expr.operands]

    res = [ast.number(1)]
    for term in expanded:
        if term.operation != ast.OpKind.add:
            res = [simplify(ast.mul(operands=[term, t])) for t in res]
        else:
            res2 = []
            for t1 in res:
                for t2 in term.operands:
                    res2.append(simplify(ast.mul(operands=[t1, t2])))

            res = simplify(ast.add(operands=res2)).operands

    return simplify(ast.add(operands=res))
