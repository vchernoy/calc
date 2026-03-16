"""
Immutable AST for arithmetic and symbolic expressions.
"""

from __future__ import annotations

import collections
import enum
import math


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
    evalf = 7
    expand = 8
    diff = 9


Num = float
VarTerm = collections.Counter[str]
Vars = set[str]
Nodes = list["Node"]


class Node:
    def __init__(
        self,
        operation: OpKind,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        if not isinstance(operation, OpKind):
            raise ValueError(f"operation must be OpKind, got {type(operation)}")
        if not isinstance(coeff, (float, int)):
            raise ValueError(f"coeff must be float or int, got {type(coeff)}")

        self.vars = (
            collections.Counter({v: p for v, p in variables.items() if p != 0})
            if variables
            else collections.Counter()
        )

        self.operation: OpKind = operation
        self.coeff: Num = coeff
        self.operands: Nodes = operands if operands else []

        if not isinstance(self.operands, list):
            raise ValueError(f"operands must be list, got {type(self.operands)}")

    def degree(self, var: str | None = None) -> int:
        raise NotImplementedError()

    def numeric(self) -> bool:
        raise NotImplementedError()

    def is_term(self) -> bool:
        raise NotImplementedError()

    def footprint(self) -> str:
        return ",".join(f"{v}^{p}" for v, p in sorted(self.vars.items()))

    def __repr__(self) -> str:
        r = [self.operation.name]
        if self.coeff != 1:
            r.append(f"{self.coeff}")

        if self.vars:
            r.append("(" + " ".join(f"{v}^{p}" for v, p in self.vars.items()) + ")")

        if self.operands:
            r.extend(repr(o) for o in self.operands)

        return "[" + ", ".join(r) + "]"

    def __str__(self, in_parenthesis: bool = False) -> str:
        import symexpr.evaluators as evaluators

        return evaluators.stringify(self, in_parenthesis)


class Add(Node):
    """
    Represents coeff * {x^k for x,k in vars} * sum(operands).
    For examples:
      2x*x*y*(expr1 + expr2 + ... + exprn) could be represented by one Add-node
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.add, coeff, variables, operands)
        if len(self.operands) < 2:
            raise ValueError("Add requires at least 2 operands")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var) + max(n.degree(var) for n in self.operands)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return False


class Mul(Node):
    """
    Represents coeff * {x^k for x,k in vars} * product(operands).
    For examples:
      2x*x*y * expr1 * expr2 * ... * exprn could be represented by one Mul-node
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.mul, coeff, variables, operands)
        if len(self.operands) < 2:
            raise ValueError("Mul requires at least 2 operands")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var) + sum(n.degree(var) for n in self.operands)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return False


class Inv(Node):
    """
    Represents coeff * {x^k for (x,k) in vars} / operands[0].
    For examples:
      2x*x*y / expr could be represented by one Inv-node
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.inv, coeff, variables, operands)
        if len(self.operands) != 1:
            raise ValueError("Inv requires exactly 1 operand")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var) - self.operands[0].degree(var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class One(Node):
    """
    Represents coeff * {x^k for x,k in vars}.
    For examples:
      2x*x*y could be represented by one One-node
    """

    def __init__(self, coeff: Num = 1, variables: VarTerm | None = None):
        super().__init__(OpKind.one, coeff, variables)

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return not self.vars

    def is_term(self) -> bool:
        return True


class Log(Node):
    """
    Represents coeff * {x^k for x,k in vars} * log operands[0].
    For examples:
      2x*x*y * log expr could be represented by one Log-node
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.log, coeff, variables, operands)
        if len(self.operands) != 1:
            raise ValueError("Log requires exactly 1 operand")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class Exp(Node):
    """
    Represents coeff * {x^k for x,k in vars} * exp operands[0].
    For examples:
      2x*x*y * exp expr could be represented by one Exp-node
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.exp, coeff, variables, operands)
        if len(self.operands) != 1:
            raise ValueError("Exp requires exactly 1 operand")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class Evalf(Node):
    """
    Represents coeff * {x^k for x,k in vars} * evalf(operands[0]).
    Wrapper that forces numerical evaluation of the inner expression.
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.evalf, coeff, variables, operands)
        if len(self.operands) != 1:
            raise ValueError("Evalf requires exactly 1 operand")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class Expand(Node):
    """
    Represents coeff * {x^k for x,k in vars} * expand(operands[0]).
    Wrapper that forces expansion of parentheses in the inner expression.
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        operands: Nodes | None = None,
    ):
        super().__init__(OpKind.expand, coeff, variables, operands)
        if len(self.operands) != 1:
            raise ValueError("Expand requires exactly 1 operand")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


class Diff(Node):
    """
    Represents coeff * {x^k for x,k in vars} * d(operands[0])/d(operands[1]).
    operands[0] is the expression to differentiate, operands[1] is the variable.
    """

    def __init__(
        self,
        coeff: Num = 1,
        variables: VarTerm | None = None,
        args: Nodes | None = None,
    ):
        super().__init__(OpKind.diff, coeff, variables, args)
        if len(self.operands) != 2:
            raise ValueError("Diff requires exactly 2 operands (expr, var)")

    def degree(self, var: str | None = None) -> int:
        return _degree(self, var)

    def numeric(self) -> bool:
        return False

    def is_term(self) -> bool:
        return self.operands[0].numeric()


def new(
    operation: OpKind,
    coeff: Num = 1,
    variables: VarTerm | None = None,
    operands: Nodes | None = None,
) -> Node:
    """
    Factory methods to create nodes of different types.
    """
    if operation == OpKind.one:
        if operands:
            raise ValueError("One node must not have operands")
        return term(coeff, variables)

    if operation == OpKind.add:
        return add(operands or [], variables, coeff)

    if operation == OpKind.mul:
        return mul(operands or [], variables, coeff)

    if not operands:
        raise ValueError(f"{operation} requires operands")
    if operation == OpKind.inv:
        return inv(operands[0], variables, coeff)

    if operation == OpKind.log:
        return log(operands[0], variables, coeff)

    if operation == OpKind.exp:
        return exp(operands[0], variables, coeff)

    if operation == OpKind.evalf:
        return evalf(operands[0], variables, coeff)

    if operation == OpKind.expand:
        return expand(operands[0], variables, coeff)

    if operation == OpKind.diff:
        return diff(operands, variables, coeff)

    raise ValueError(f"unknown operation: {operation}")


def term(coeff: Num, variables: VarTerm | None = None) -> One:
    if coeff == 0:
        return number(0)

    return One(coeff=coeff, variables=variables) if variables else number(coeff)


def number(value: Num) -> One:
    return One(coeff=value)


def variable(name: str, power: int = 1) -> One:
    return One(variables=collections.Counter({name: power}))


def new_with(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    if coeff == 0:
        return number(0)

    if not variables:
        res_vars = expr.vars
    else:
        res_vars = collections.Counter(expr.vars)
        res_vars.update(variables)

    return new(
        operation=expr.operation,
        coeff=coeff * expr.coeff,
        variables=res_vars,
        operands=expr.operands,
    )


def add(operands: Nodes, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    if coeff == 0:
        return number(0)

    operands = [n for n in operands if n]
    if not operands:
        return number(0)

    return (
        Add(operands=operands, variables=variables, coeff=coeff)
        if len(operands) > 1
        else new_with(operands[0], variables, coeff)
    )


def mul(operands: Nodes, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    if coeff == 0:
        return number(0)

    operands = [n for n in operands if n]
    if not operands:
        return term(coeff=coeff, variables=variables)

    return (
        Mul(operands=operands, variables=variables, coeff=coeff)
        if len(operands) > 1
        else new_with(operands[0], variables, coeff)
    )


def neg(expr: Node) -> Node | None:
    return new_with(expr=expr, coeff=-1) if expr else None


def inv(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> One | Inv:
    if coeff == 0:
        return number(0)

    if not expr:
        return term(coeff=coeff, variables=variables)

    if expr.coeff == 0 or expr.vars or expr.operation != OpKind.one:
        return Inv(operands=[expr], variables=variables, coeff=coeff)

    if not isinstance(coeff, int) or not isinstance(expr.coeff, int):
        return term(variables=variables, coeff=coeff / expr.coeff)

    gcd_val = math.gcd(int(coeff), int(expr.coeff))
    res_coeff = coeff // gcd_val
    inv_coeff = expr.coeff // gcd_val
    if inv_coeff < 0:
        res_coeff = -res_coeff
        inv_coeff = -inv_coeff

    if inv_coeff == 1:
        return term(variables=variables, coeff=res_coeff)

    return Inv(operands=[number(inv_coeff)], variables=variables, coeff=res_coeff)


def log(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> One | Log:
    return (
        Log(operands=[expr], variables=variables, coeff=coeff)
        if expr
        else term(coeff=coeff, variables=variables)
    )


def exp(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> One | Exp:
    return (
        Exp(operands=[expr], variables=variables, coeff=coeff)
        if expr
        else term(coeff=coeff, variables=variables)
    )


def evalf(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    return (
        Evalf(operands=[expr], variables=variables, coeff=coeff)
        if expr
        else term(coeff=coeff, variables=variables)
    )


def expand(expr: Node, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    return (
        Expand(operands=[expr], variables=variables, coeff=coeff)
        if expr
        else term(coeff=coeff, variables=variables)
    )


def diff(args: Nodes, variables: VarTerm | None = None, coeff: Num = 1) -> Node:
    return (
        Diff(args=args, variables=variables, coeff=coeff)
        if args
        else term(coeff=coeff, variables=variables)
    )


def _degree(expr: Node, var: str | None = None) -> int:
    return expr.vars.get(var, 0) if var else sum(expr.vars.values())


def all_vars(expr: Node) -> Vars:
    res = set(expr.vars.keys())
    for n in expr.operands:
        res.update(all_vars(n))

    return res
