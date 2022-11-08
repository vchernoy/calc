import functools
import itertools
import symexpr.ast as ast


@functools.singledispatch
def stringify(expr, in_parenthesis: bool = False) -> str:
    """
    :param in_parenthesis:
    :param expr: AST
    :return: AST
    """
    raise TypeError('cannot stringify {expr}, {in_parenthesis=}')


def _vars_to_str(expr: ast.Node) -> str:
    return '*'.join(itertools.chain.from_iterable([v] * d for v,d in sorted(expr.vars.items())))


@stringify.register
def _add_stringify(self: ast.Add, in_parenthesis: bool = False) -> str:
    res = ''
    if self.coefficient == -1:
        res = '-'
    elif self.coefficient != 1:
        res = str(self.coefficient)

    if self.vars:
        res += _vars_to_str(self) + '*'

    extra_parenthesis = res != ''
    if extra_parenthesis:
        res += '('

    res += '+'.join(stringify(n, True) for n in self.operands)
    if extra_parenthesis:
        res += ')'

    return f'({res})' if in_parenthesis else res


@stringify.register
def _mul_stringify(self: ast.Mul, in_parenthesis: bool = False) -> str:
    res = ''
    if self.coefficient == -1:
        res = '-'
    elif self.coefficient != 1:
        res = str(self.coefficient)

    if self.vars:
        res += _vars_to_str(self) + '*('

    res += '*'.join(stringify(n, True) for n in self.operands)
    if self.vars:
        res += ')'

    return f'({res})' if in_parenthesis else res


@stringify.register
def _inv_stringify(self: ast.Inv, in_parenthesis: bool = False) -> str:
    if (self.coefficient == -1) and self.vars:
        res = '-'
    elif (self.coefficient != 1) or not self.vars:
        res = str(self.coefficient)
    else:
        res = ''

    res += _vars_to_str(self)
    res += '/' + stringify(self.operands[0], True)

    return f'({res})' if in_parenthesis else res


@stringify.register
def _one_stringify(self: ast.One, in_parenthesis: bool = False) -> str:
    res = ''
    if (self.coefficient == -1) and self.vars:
        res = '-'
    elif (self.coefficient != 1) or not self.vars:
        res = str(self.coefficient)

    res += _vars_to_str(self)
    around_parenthesis = in_parenthesis and ((self.coefficient != 1 and self.vars) or (self.coefficient < 0))
    return f'({res})' if around_parenthesis else res


@stringify.register
def _log_stringify(self: ast.Log, in_parenthesis: bool = False) -> str:
    res = ''
    if self.coefficient == -1:
        res = '-'
    elif self.coefficient != 1:
        res = str(self.coefficient)

    if self.vars:
        res += _vars_to_str(self) + '*'

    res += 'log ' + stringify(self.operands[0], True)

    return f'({res})' if in_parenthesis else res


@stringify.register
def _exp_stringify(self:ast.Exp, in_parenthesis: bool = False) -> str:
    res = ''
    if self.coefficient == -1:
        res = '-'
    elif self.coefficient != 1:
        res = str(self.coefficient)

    if self.vars:
        res += _vars_to_str(self) + '*'

    res += 'exp ' + stringify(self.operands[0], True)

    return f'({res})' if in_parenthesis else res

