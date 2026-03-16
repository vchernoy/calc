import functools
import itertools

import symexpr.ast as ast


@functools.singledispatch
def stringify(expr: ast.Node, in_parenthesis: bool = False) -> str:
    """
    :param expr: AST
    :param in_parenthesis: whether to wrap result in parentheses
    :return: string representation
    """
    raise TypeError(f"cannot stringify {expr}, in_parenthesis={in_parenthesis}")


def _coeff_str(c: float) -> str:
    return str(int(c)) if c == int(c) else str(c)


def _vars_to_str(expr: ast.Node, _: bool = False) -> str:
    return "*".join(
        itertools.chain.from_iterable([v] * d for v, d in sorted(expr.vars.items()))
    )


@stringify.register
def _add_stringify(self: ast.Add, in_parenthesis: bool = False) -> str:
    res = ""
    if self.coeff == -1:
        res = "-"
    elif self.coeff != 1:
        res = _coeff_str(self.coeff)

    if self.vars:
        res += _vars_to_str(self) + "*"

    extra_parenthesis = res != ""
    if extra_parenthesis:
        res += "("

    res += "+".join(stringify(n, False) for n in self.operands)
    if extra_parenthesis:
        res += ")"

    return f"({res})" if in_parenthesis else res


@stringify.register
def _mul_stringify(self: ast.Mul, in_parenthesis: bool = False) -> str:
    res = ""
    if self.coeff == -1:
        res = "-"
    elif self.coeff != 1:
        res = _coeff_str(self.coeff)

    if self.vars:
        res += _vars_to_str(self) + "*("

    res += "*".join(stringify(n, True) for n in self.operands)
    if self.vars:
        res += ")"

    return f"({res})" if in_parenthesis else res


@stringify.register
def _inv_stringify(self: ast.Inv, in_parenthesis: bool = False) -> str:
    if self.coeff == -1 and self.vars:
        res = "-"
    elif self.coeff != 1 or not self.vars:
        res = _coeff_str(self.coeff)
    else:
        res = ""

    res += _vars_to_str(self)
    res += "/" + stringify(self.operands[0], True)

    return f"({res})" if in_parenthesis else res


@stringify.register
def _one_stringify(self: ast.One, in_parenthesis: bool = False) -> str:
    res = ""
    if self.coeff == -1 and self.vars:
        res = "-"
    elif self.coeff != 1 or not self.vars:
        res = _coeff_str(self.coeff)

    res += _vars_to_str(self)
    # Add parens when used as function arg to avoid ambiguity (e.g. exp x*x vs exp(x*x))
    needs_parens = (
        self.vars
        and (len(self.vars) > 1 or any(p != 1 for p in self.vars.values()))
    ) or (self.coeff != 1 and self.vars) or self.coeff < 0
    around_parenthesis = in_parenthesis and needs_parens
    return f"({res})" if around_parenthesis else res


@stringify.register(ast.Diff)
def _diff_stringify(self: ast.Diff, in_parenthesis: bool = False) -> str:
    res = ""
    if self.coeff == -1:
        res = "-"
    elif self.coeff != 1:
        res = _coeff_str(self.coeff)

    if self.vars:
        res += _vars_to_str(self) + "*"

    res += (
        "diff("
        + stringify(self.operands[0], True)
        + ", "
        + stringify(self.operands[1], True)
        + ")"
    )

    return f"({res})" if in_parenthesis else res


@stringify.register(ast.Exp)
@stringify.register(ast.Log)
@stringify.register(ast.Evalf)
@stringify.register(ast.Expand)
def _stringify(self: ast.Log, in_parenthesis: bool = False) -> str:
    res = ""
    if self.coeff == -1:
        res = "-"
    elif self.coeff != 1:
        res = _coeff_str(self.coeff)

    if self.vars:
        res += _vars_to_str(self) + "*"

    res += self.operation.name + " " + stringify(self.operands[0], True)

    return f"({res})" if in_parenthesis else res
