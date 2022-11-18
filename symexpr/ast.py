import math
import enum
import itertools
import typing

"""
It defines immutable AST that represents arithmetic/symbolic expressions.
"""


class OpKind(enum.Enum):
    """
    The AST could be designed without this enum, but it is much harder.
    So for now, having in each node the information about its type is redundant,
    but it is much simpler to manipulate with such nodes.
    """
    add = 1
    one = 2
    mul = 3
    inv = 4
    log = 5
    exp = 6


Num: typing.TypeAlias = float
VarTerm: typing.TypeAlias = dict[str, int]
Vars: typing.TypeAlias = set[str]
Nodes: typing.TypeAlias = list['Node']


class Node:
    def __init__(
            self,
            operation: OpKind,
            coefficient: Num = 1,
            variables: VarTerm = None,
            operands: Nodes = None
    ):
        assert type(operation) == OpKind
        assert type(coefficient) in (float, int)

        self.vars: VarTerm = {}
        incby(self.vars, variables)

        self.operation: OpKind = operation
        self.coefficient: Num = coefficient
        self.operands: Nodes = operands if operands else []

        assert type(self.operands) == list

    def degree(self, var: str = None) -> int:
        raise NotImplemented()

    def numeric(self) -> bool:
        raise NotImplemented()

    def is_term(self) -> bool:
        raise NotImplemented()

    def footprint(self) -> str:
        return ','.join(f'{v}^{p}' for v,p in sorted(self.vars.items()))

    def __repr__(self) -> str:
        r = [f'{self.operation}', f'{self.coefficient}', f'{self.vars}']
        if self.operands:
            r.extend(repr(o) for o in self.operands)

        return f'[{r}]'

    def __str__(self, in_parenthesis: bool = False) -> str:
        import symexpr.evaluators as evaluators

        return evaluators.stringify(self, in_parenthesis)
        # raise NotImplemented()


class Add(Node):
    """
    Represents coefficient * {x^k for x,k in vars} * sum(operands).
    For examples:
      2x*x*y*(expr1 + expr2 + ... + exprn) could be represented by one Add-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None):
        super().__init__(OpKind.add, coefficient, variables, operands)
        assert len(self.operands) >= 2

    def degree(self, var: str = None) -> int:
        return _degree(self, var) + max(n.degree(var) for n in self.operands)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return False


class Mul(Node):
    """
    Represents coefficient * {x^k for x,k in vars} * product(operands).
    For examples:
      2x*x*y * expr1 * expr2 * ... * exprn could be represented by one Mul-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None):
        super().__init__(OpKind.mul, coefficient, variables, operands)
        assert len(self.operands) >= 2

    def degree(self, var: str = None) -> int:
        return _degree(self, var) + sum(n.degree(var) for n in self.operands)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return False


class Inv(Node):
    """
    Represents coefficient * {x^k for (x,k) in vars} / operands[0].
    For examples:
      2x*x*y / expr could be represented by one Inv-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None):
        super().__init__(OpKind.inv, coefficient, variables, operands)
        assert len(self.operands) == 1

    def degree(self, var: str = None) -> int:
        return _degree(self, var) - self.operands[0].degree(var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class One(Node):
    """
    Represents coefficient * {x^k for x,k in vars}.
    For examples:
      2x*x*y could be represented by one One-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None):
        super().__init__(OpKind.one, coefficient, variables)

    def degree(self, var: str = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return not self.vars

    def is_term(self) -> bool:
        return True


class Log(Node):
    """
    Represents coefficient * {x^k for x,k in vars} * log operands[0].
    For examples:
      2x*x*y * log expr could be represented by one Log-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None):
        super().__init__(OpKind.log, coefficient, variables, operands)
        assert len(self.operands) == 1

    def degree(self, var: str = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class Exp(Node):
    """
    Represents coefficient * {x^k for x,k in vars} * exp operands[0].
    For examples:
      2x*x*y * exp expr could be represented by one Exp-node
    """
    def __init__(self, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None):
        super().__init__(OpKind.exp, coefficient, variables, operands)
        assert len(self.operands) == 1

    def degree(self, var: str = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


def new(operation: OpKind, coefficient: Num = 1, variables: VarTerm = None, operands: Nodes = None) -> Node:
    """
    Factory methods to create nodes of different types.
    """
    if operation == OpKind.one:
        assert not operands
        return term(coefficient, variables)

    if operation == OpKind.add:
        return add(operands, variables, coefficient)

    if operation == OpKind.mul:
        return mul(operands, variables, coefficient)

    if operation == OpKind.inv:
        return inv(operands[0], variables, coefficient)

    if operation == OpKind.log:
        return log(operands[0], variables, coefficient)

    if operation == OpKind.exp:
        return exp(operands[0], variables, coefficient)

    assert False


def term(coefficient: Num, variables: VarTerm = None) -> One:
    if coefficient == 0:
        return number(0)

    return One(coefficient=coefficient, variables=variables) if variables else number(coefficient)


def number(value: Num) -> One:
    return One(coefficient=value)


def variable(name: str, power: int = 1) -> One:
    return One(variables={name: power})


def new_with(expr: Node, variables: VarTerm = None, coefficient: Num = 1) -> Node:
    if coefficient == 0:
        return number(0)

    if not variables:
        res_vars = expr.vars
    else:
        res_vars = {}
        incby(res_vars, variables)
        incby(res_vars, expr.vars)

    return new(operation=expr.operation, coefficient=coefficient * expr.coefficient, variables=res_vars, operands=expr.operands)


def add(operands: Nodes, variables: VarTerm = None, coefficient: Num = 1) -> Node:
    if coefficient == 0:
        return number(0)

    operands = [n for n in operands if n]
    if not operands:
        return number(0)

    return Add(operands=operands, variables=variables, coefficient=coefficient) if len(operands) > 1 \
        else new_with(operands[0], variables, coefficient)


def mul(operands: Nodes, variables: VarTerm = None, coefficient: Num = 1) -> Node:
    if coefficient == 0:
        return number(0)

    operands = [n for n in operands if n]
    if not operands:
        return term(coefficient=coefficient, variables=variables)

    return Mul(operands=operands, variables=variables, coefficient=coefficient) if len(operands) > 1 \
        else new_with(operands[0], variables, coefficient)


def neg(expr: Node) -> Node | None:
    return new_with(expr=expr, coefficient=-1) if expr else None


def inv(expr: Node, variables: VarTerm = None, coefficient: Num = 1) -> One | Inv:
    if coefficient == 0:
        return number(0)

    if not expr:
        return term(coefficient=coefficient, variables=variables)

    if expr.coefficient == 0 or expr.vars or expr.operation != OpKind.one:
        return Inv(operands=[expr], variables=variables, coefficient=coefficient)

    if type(coefficient) != int or type(expr.coefficient) != int:
        return term(variables=variables, coefficient=coefficient / expr.coefficient)

    gcd_val = math.gcd(coefficient, expr.coefficient)
    res_coefficient = coefficient // gcd_val
    inv_coefficient = expr.coefficient // gcd_val
    if inv_coefficient < 0:
        res_coefficient = -res_coefficient
        inv_coefficient = -inv_coefficient

    if inv_coefficient == 1:
        return term(variables=variables, coefficient=res_coefficient)

    return Inv(operands=[number(inv_coefficient)], variables=variables, coefficient=res_coefficient)


def log(expr: Node, variables: VarTerm = None, coefficient: Num = 1) -> One | Log:
    return Log(operands=[expr], variables=variables, coefficient=coefficient) if expr \
        else term(coefficient=coefficient, variables=variables)


def exp(expr: Node, variables: VarTerm = None, coefficient: Num = 1) -> One | Exp:
    return Exp(operands=[expr], variables=variables, coefficient=coefficient) if expr \
        else term(coefficient=coefficient, variables=variables)


def _degree(expr: Node, var: str = None) -> int:
    return expr.vars.get(var, 0) if var else sum(expr.vars.values())


def incby(acc_vars: VarTerm, added_vars: VarTerm) -> None:
    if not added_vars:
        return

    for v, p in added_vars.items():
        acc_vars[v] = acc_vars.get(v, 0) + p
        if acc_vars[v] == 0:
            del acc_vars[v]


def all_vars(expr: Node) -> Vars:
    res = set(expr.vars.keys())
    for n in expr.operands:
        res.update(all_vars(n))

    return res

