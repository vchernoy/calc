import collections
import functools
from typing import Callable

import symexpr.ast as ast
from symexpr import evaluators


def _var_from_node(var_node: ast.Node) -> str | None:
    """Extract variable name from a variable AST node (e.g. ast.variable('x'))."""
    if var_node.operation == ast.OpKind.one and len(var_node.vars) == 1:
        name, power = next(iter(var_node.vars.items()))
        if power == 1:
            return name
    return None


@functools.singledispatch
def diff(expr: ast.Node, var: str) -> ast.Node:
    """
    Symbolic differentiation: d(expr)/d(var).
    :param expr: AST to differentiate
    :param var: variable to differentiate with respect to
    :return: derivative as AST
    """
    raise TypeError(f'cannot diff {expr} with respect to {var}')


@diff.register
def _one_diff(expr: ast.One, var: str) -> ast.Node:
    if var not in expr.vars:
        return ast.number(0)

    vars_copy = collections.Counter(expr.vars)
    p = vars_copy[var]
    if p == 1:
        del vars_copy[var]
    else:
        vars_copy[var] = p - 1

    return ast.term(coeff=expr.coeff * p, variables=vars_copy if vars_copy else collections.Counter())


@diff.register
def _add_diff(expr: ast.Add, var: str) -> ast.Node:
    # d/dx (A + B + ...) = dA/dx + dB/dx + ...
    derivatives = [evaluators.diff(n, var) for n in expr.operands]
    term = ast.term(coeff=expr.coeff, variables=expr.vars)
    return ast.mul([term, ast.add(derivatives)])


@diff.register
def _mul_diff(expr: ast.Mul, var: str) -> ast.Node:
    # d/dx (A * B * ...) = (dA/dx)*B*... + A*(dB/dx)*... + ...
    operands = expr.operands
    terms = []
    for i, op in enumerate(operands):
        d_op = evaluators.diff(op, var)
        if d_op.coeff == 0 and not d_op.vars and not d_op.operands:
            continue
        # Build product: coeff * vars * (all operands with op[i] replaced by d_op)
        other_ops = list(operands)
        other_ops[i] = d_op
        terms.append(ast.mul(other_ops, coeff=expr.coeff, variables=expr.vars))

    if not terms:
        return ast.number(0)
    return ast.add(terms)


@diff.register
def _inv_diff(expr: ast.Inv, var: str) -> ast.Node:
    # d/dx (1/A) = -(dA/dx) / A^2
    a = expr.operands[0]
    da = evaluators.diff(a, var)
    neg_da = ast.neg(da)
    if neg_da is None:
        raise ValueError('neg(da) returned None')
    return ast.mul([
        neg_da,
        ast.inv(ast.mul([a, a]))
    ], coeff=expr.coeff, variables=expr.vars)


@diff.register
def _log_diff(expr: ast.Log, var: str) -> ast.Node:
    # d/dx (log A) = (dA/dx) / A
    a = expr.operands[0]
    da = evaluators.diff(a, var)
    return ast.mul([
        ast.inv(a),
        da
    ], coeff=expr.coeff, variables=expr.vars)


@diff.register
def _exp_diff(expr: ast.Exp, var: str) -> ast.Node:
    # d/dx (exp A) = exp(A) * (dA/dx)
    a = expr.operands[0]
    da = evaluators.diff(a, var)
    return ast.mul([
        ast.exp(a),
        da
    ], coeff=expr.coeff, variables=expr.vars)


@diff.register(ast.Evalf)
@diff.register(ast.Expand)
def _special_diff(expr: ast.Node, var: str) -> ast.Node:
    apply: dict[ast.OpKind, Callable[[ast.Node], ast.Node]] = {
        ast.OpKind.evalf: evaluators.evalf,
        ast.OpKind.expand: evaluators.expand,
    }
    arg = apply[expr.operation](expr.operands[0])
    arg = evaluators.simplify(arg)
    return evaluators.diff(ast.new_with(expr=arg, variables=expr.vars, coeff=expr.coeff), var)


@diff.register(ast.Diff)
def _diff_diff(expr: ast.Diff, var: str) -> ast.Node:
    # Second derivative: d/dx (d/dy f) - differentiate the inner expr w.r.t. var
    inner_expr = expr.operands[0]
    inner_var_node = expr.operands[1]
    inner_var = _var_from_node(inner_var_node)
    if inner_var is None:
        raise TypeError(f'Diff node has invalid variable operand: {inner_var_node}')

    # d/dx (d/dy f) = d²f/dxdy - differentiate inner result w.r.t. outer var
    inner_deriv = evaluators.diff(inner_expr, inner_var)
    return evaluators.diff(
        ast.new_with(expr=inner_deriv, variables=expr.vars, coeff=expr.coeff),
        var
    )
