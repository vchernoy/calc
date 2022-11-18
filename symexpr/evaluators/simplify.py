import collections
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
    raise TypeError(f'cannot evaluators" {expr}')


@simplify.register
def _one_simplify(expr: ast.One) -> ast.Node:
    return expr


@simplify.register
def _exp_simplify(expr: ast.Exp) -> ast.Node:
    evaluated = simplify(expr.operands[0])

    if evaluated.operation == ast.OpKind.add:
        return ast.mul(
            coeff=expr.coeff,
            variables=expr.vars,
            operands=[
                ast.exp(ast.add([n], variables=evaluated.vars, coeff=evaluated.coeff)) for n in evaluated.operands
            ],
        )

    if evaluated.operation == ast.OpKind.log and not evaluated.vars and evaluated.coeff == 1:
        return ast.add(
            coeff=expr.coeff,
            variables=expr.vars,
            operands=[evaluated.operands[0]],
        )

    return ast.new(
        operation=expr.operation,
        coeff=expr.coeff,
        variables=expr.vars,
        operands=[evaluated],
    )


@simplify.register
def _log_simplify(expr: ast.Log) -> ast.Node:
    evaluated = simplify(expr.operands[0])
    if evaluated.operation in (ast.OpKind.exp, ast.OpKind.mul, ast.OpKind.one, ast.OpKind.inv):
        nodes = []
        nodes.append(ast.log(ast.number(evaluated.coeff))) if evaluated.coeff != 1 else ...
        nodes.extend(ast.log(expr=ast.variable(v), coeff=p) for v, p in evaluated.vars.items())
        nodes.append(evaluated.operands[0]) if evaluated.operation == ast.OpKind.exp else ...
        nodes.extend(ast.log(expr=n) for n in evaluated.operands) if evaluated.operation == ast.OpKind.mul else ...
        nodes.append(ast.neg(ast.log(evaluated.operands[0]))) if evaluated.operation == ast.OpKind.inv else ...

        return ast.add(
            variables=expr.vars,
            operands=nodes,
            coeff=expr.coeff
        )

    return ast.new(
        operation=expr.operation,
        variables=expr.vars,
        operands=[evaluated],
        coeff=expr.coeff
    )


@simplify.register
def _add_simplify(expr: ast.Add) -> ast.Node:
    evaluated0 = [simplify(n) for n in expr.operands]
    evaluated = []
    for n in evaluated0:
        if n.operation == ast.OpKind.add and n.coeff == 1 and not n.vars:
            evaluated += n.operands
        else:
            evaluated.append(n)

    terms = [t for t in evaluated if t.operation == ast.OpKind.one]
    non_terms = [t for t in evaluated if t.operation != ast.OpKind.one]

    d = {}
    for t in terms:
        k = t.footprint()
        if k in d:
            d[k] = ast.term(d[k].coeff + t.coeff, t.vars)
        else:
            d[k] = t

    evaluated = [t for t in d.values() if t.coeff != 0] + non_terms

    pos_degree = {}
    for t in evaluated:
        if t.degree() >= 0:
            pos_degree.setdefault(t.degree(), []).append(t)

    return ast.add(
        operands=list(
            itertools.chain.from_iterable(l for _, l in sorted(pos_degree.items()))
        ) + [t for t in evaluated if t.degree() < 0],
        coeff=expr.coeff,
        variables=expr.vars
    )


@simplify.register
def _mul_simplify(expr: ast.Mul) -> ast.Node:
    evaluated0 = [simplify(n) for n in expr.operands]
    evaluated1 = []

    res_coeff = expr.coeff
    res_vars = collections.Counter(expr.vars)

    for n in evaluated0:
        if n.operation == ast.OpKind.mul:
            res_vars.update(n.vars)
            res_coeff *= n.coeff
            evaluated1 += n.operands
        else:
            evaluated1.append(n)

    evaluated2 = []
    for n in evaluated1:
        assert n.operation != ast.OpKind.mul

        res_vars.update(n.vars)
        res_coeff *= n.coeff

        if n.operation != ast.OpKind.one:
            evaluated2.append(simplify(ast.new(operation=n.operation, operands=n.operands)))

    evaluated3 = [n for n in evaluated2 if n.operation != ast.OpKind.inv]
    inv_coeff = 1
    for n in evaluated2:
        assert n.operation in [ast.OpKind.inv, ast.OpKind.add, ast.OpKind.log, ast.OpKind.exp]
        assert n.coeff == 1
        assert not n.vars

        if n.operation == ast.OpKind.inv:
            t = n.operands[0]
            res_vars.update({v: -p for v, p in t.vars.items()})
            inv_coeff *= t.coeff
            if t.operation != ast.OpKind.one:
                evaluated3.append(ast.inv(ast.new(operation=t.operation, operands=t.operands)))

    evaluated = []
    for n in evaluated3:
        assert n.operation in [ast.OpKind.inv, ast.OpKind.add, ast.OpKind.log, ast.OpKind.exp]
        assert n.coeff == 1
        assert not n.vars

        evaluated.append(n)

    pos_vars = collections.Counter({v: p for v, p in res_vars.items() if p > 0})
    inv_vars = collections.Counter({v: -p for v, p in res_vars.items() if p < 0})

    n = ast.inv(expr=ast.term(inv_coeff, inv_vars), variables=pos_vars, coeff=res_coeff)

    return ast.mul(evaluated, coeff=n.coeff, variables=n.vars) if n.operation == ast.OpKind.one \
        else ast.mul(evaluated+[n])


@simplify.register
def _inv_simplify(expr: ast.Inv) -> ast.Node:
    evaluated = simplify(expr.operands[0])
    res_vars = collections.Counter(expr.vars)
    res_vars.update({v: -p for v, p in evaluated.vars.items()})

    pos_vars = collections.Counter({v: p for v, p in res_vars.items() if p > 0})
    inv_vars = collections.Counter({v: -p for v, p in res_vars.items() if p < 0})

    if evaluated.operation == ast.OpKind.one:
        return ast.inv(
            ast.term(evaluated.coeff, inv_vars),
            coeff=expr.coeff,
            variables=pos_vars
        )

    if evaluated.operation == ast.OpKind.inv:
        return simplify(
            ast.mul(
                operands=[
                    ast.inv(
                        ast.term(evaluated.coeff, inv_vars),
                        coeff=expr.coeff,
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
                        ast.mul(other_terms, coeff=evaluated.coeff, variables=inv_vars),
                        coeff=expr.coeff,
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
                coeff=evaluated.coeff
            )
        ),
        coeff=expr.coeff,
        variables=pos_vars
    )
