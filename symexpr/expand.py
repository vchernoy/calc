import functools
import symexpr.ast as ast
from symexpr.simplify import simplify


"""
A set of tools that allow to manipulate with AST
"""


@functools.singledispatch
def expand(self):
    """
    Opens parentheses if meets a(b + x)
    :param self: AST
    :return: AST
    """
    raise TypeError("cannot expand", self)


@expand.register(ast.Inv)
@expand.register(ast.Log)
@expand.register(ast.Exp)
@expand.register(ast.One)
def _expand(self):
    return self


@expand.register(ast.Add)
def _add_expand(self):
    term = ast.term(coefficient=self.coefficient, variables=self.vars)
    expanded = [expand(n) for n in self.operands]

    node = simplify(ast.addition(nodes=expanded))
    if node.operation != ast.OpKind.add:
        return simplify(ast.multiplication(nodes=[term, node]))

    term1 = ast.term(coefficient=node.coefficient, variables=node.vars)

    terms = [simplify(ast.multiplication(nodes=[term, t, term1])) for t in node.operands]
    return simplify(ast.addition(terms))


@expand.register(ast.Mul)
def _mul_expand(self):
    term = ast.term(coefficient=self.coefficient, variables=self.vars)
    expanded = [term] + [expand(n) for n in self.operands]

    res = [ast.number(1)]
    for term in expanded:
        if term.operation != ast.OpKind.add:
            res = [simplify(ast.multiplication(nodes=[term, t])) for t in res]
        else:
            res2 = []
            for t1 in res:
                for t2 in term.operands:
                    res2.append(simplify(ast.multiplication(nodes=[t1, t2])))

            res = simplify(ast.addition(nodes=res2)).operands

    return simplify(ast.addition(nodes=res))
