import functools
import itertools
import symexpr.ast as ast


@functools.singledispatch
def simplify(expr) -> ast.Node:
    """
    simplifies the given AST, performs basic transformation, does not open parentheses in a(b+c),
    but can evaluators a + b + c or a * b * c
    :param expr: AST
    :return: AST
    """
    raise TypeError("cannot evaluators", expr)


@simplify.register
def _one_simplify(expr: ast.One) -> ast.Node:
    return expr


@simplify.register
def _exp_simplify(expr: ast.Exp) -> ast.Node:
    evaluated = simplify(expr.operands[0])

    if evaluated.operation == ast.OpKind.add:
        return ast.mul(
            operands=[
                ast.exp(ast.add([n], variables=evaluated.vars, coefficient=evaluated.coefficient)) for n in evaluated.operands
            ],
            variables=expr.vars,
            coefficient=expr.coefficient
        )

    if evaluated.operation == ast.OpKind.log and not evaluated.vars and evaluated.coefficient == 1:
        return ast.add(
            variables=expr.vars,
            operands=[evaluated.operands[0]],
            coefficient=expr.coefficient
        )

    return ast.new(
        operation=expr.operation,
        variables=expr.vars,
        operands=[evaluated],
        coefficient=expr.coefficient
    )


@simplify.register
def _log_simplify(expr: ast.Log) -> ast.Node:
    evaluated = simplify(expr.operands[0])
    if evaluated.operation in (ast.OpKind.exp, ast.OpKind.mul, ast.OpKind.one, ast.OpKind.inv):
        nodes = []
        nodes.append(ast.log(ast.number(evaluated.coefficient))) if evaluated.coefficient != 1 else ...
        nodes.extend(ast.log(expr=ast.variable(v), coefficient=p) for v,p in evaluated.vars.items())
        nodes.append(evaluated.operands[0]) if evaluated.operation == ast.OpKind.exp else ...
        nodes.extend(ast.log(expr=n) for n in evaluated.operands) if evaluated.operation == ast.OpKind.mul else ...
        nodes.append(ast.neg(ast.log(evaluated.operands[0]))) if evaluated.operation == ast.OpKind.inv else ...

        return ast.add(
            variables=expr.vars,
            operands=nodes,
            coefficient=expr.coefficient
        )

    return ast.new(
        operation=expr.operation,
        variables=expr.vars,
        operands=[evaluated],
        coefficient=expr.coefficient
    )


@simplify.register
def _add_simplify(expr: ast.Add) -> ast.Node:
    evaluated0 = [simplify(n) for n in expr.operands]
    evaluated = []
    for n in evaluated0:
        if n.operation == ast.OpKind.add and n.coefficient == 1 and not n.vars:
            evaluated += n.operands
        else:
            evaluated.append(n)

    terms = [t for t in evaluated if t.operation == ast.OpKind.one]
    non_terms = [t for t in evaluated if t.operation != ast.OpKind.one]

    d = {}
    for t in terms:
        k = t.footprint()
        if k in d:
            d[k] = ast.term(d[k].coefficient + t.coefficient, t.vars)
        else:
            d[k] = t

    evaluated = [t for t in d.values() if t.coefficient != 0] + non_terms

    pos_degree = {}
    for t in evaluated:
        if t.degree() >= 0:
            pos_degree.setdefault(t.degree(), []).append(t)

    return ast.add(
        operands=list(itertools.chain.from_iterable(l for _, l in sorted(pos_degree.items())))\
                 + [t for t in evaluated if t.degree() < 0],
        coefficient=expr.coefficient,
        variables=expr.vars
    )


@simplify.register
def _mul_simplify(expr: ast.Mul) -> ast.Node:
    evaluated0 = [simplify(n) for n in expr.operands]
    evaluated1 = []

    res_coefficient = expr.coefficient
    res_vars = {}
    ast.incby(res_vars, expr.vars)

    for n in evaluated0:
        if n.operation == ast.OpKind.mul:
            ast.incby(res_vars, n.vars)
            res_coefficient *= n.coefficient
            evaluated1 += n.operands
        else:
            evaluated1.append(n)

    evaluated2 = []
    for n in evaluated1:
        assert n.operation != ast.OpKind.mul

        ast.incby(res_vars, n.vars)
        res_coefficient *= n.coefficient

        if n.operation != ast.OpKind.one:
            evaluated2.append(simplify(ast.new(operation=n.operation, operands=n.operands)))

    evaluated3 = [n for n in evaluated2 if n.operation != ast.OpKind.inv]
    inv_coefficient = 1
    for n in evaluated2:
        assert n.operation in [ast.OpKind.inv, ast.OpKind.add, ast.OpKind.log, ast.OpKind.exp]
        assert n.coefficient == 1
        assert not n.vars

        if n.operation == ast.OpKind.inv:
            t = n.operands[0]
            ast.incby(res_vars, {v: -p for v, p in t.vars.items()})
            inv_coefficient *= t.coefficient
            if t.operation != ast.OpKind.one:
                evaluated3.append(ast.inv(ast.new(operation=t.operation, operands=t.operands)))

    evaluated = []
    for n in evaluated3:
        assert n.operation in [ast.OpKind.inv, ast.OpKind.add, ast.OpKind.log, ast.OpKind.exp]
        assert n.coefficient == 1
        assert not n.vars

        evaluated.append(n)

    pos_vars = {v: p for v, p in res_vars.items() if p > 0}
    inv_vars = {v: -p for v, p in res_vars.items() if p < 0}

    n = ast.inv(expr=ast.term(inv_coefficient, inv_vars), variables=pos_vars, coefficient=res_coefficient)

    return ast.mul(evaluated, coefficient=n.coefficient, variables=n.vars) if n.operation == ast.OpKind.one \
        else ast.mul(evaluated+[n])


@simplify.register
def _inv_simplify(expr: ast.Inv) -> ast.Node:
    evaluated = simplify(expr.operands[0])

    res_vars = {}
    ast.incby(res_vars, expr.vars)
    ast.incby(res_vars, {v: -p for v, p in evaluated.vars.items()})

    pos_vars = {v: p for v, p in res_vars.items() if p > 0}
    inv_vars = {v: -p for v, p in res_vars.items() if p < 0}

    if evaluated.operation == ast.OpKind.one:
        return ast.inv(
            ast.term(evaluated.coefficient, inv_vars),
            coefficient=expr.coefficient,
            variables=pos_vars
        )

    if evaluated.operation == ast.OpKind.inv:
        return simplify(
            ast.mul(
                operands=[
                    ast.inv(
                        ast.term(evaluated.coefficient, inv_vars),
                        coefficient=expr.coefficient,
                        variables=pos_vars
                    ),
                    evaluated.operands[0]
                ]
            )
        )

    if evaluated.operation == ast.OpKind.mul:
        inversed_terms = [ast.inv(t) for t in evaluated.operands if t.operation == ast.OpKind.inv]
        other_terms = [t for t in evaluated.operands if t.operation != ast.OpKind.inv]
        return simplify(
            ast.mul(
                operands=[
                    ast.inv(
                        ast.mul(other_terms, coefficient=evaluated.coefficient, variables=inv_vars),
                        coefficient=expr.coefficient,
                        variables=pos_vars
                    )
                ] + inversed_terms
            )
        )

    return ast.inv(
        simplify(
            ast.new(
                operation=evaluated.operation,
                variables=inv_vars,
                operands=evaluated.operands,
                coefficient=evaluated.coefficient
            )
        ),
        coefficient=expr.coefficient,
        variables=pos_vars
    )
