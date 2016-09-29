import math
import enum


class Type(enum.Enum):
    add = 1
    one = 2
    mul = 3
    inv = 4
    log = 5


class Add:
    def __init__(self, coefficient=1, variables=None, operands=None):
        self.vars = {}
        incby(self.vars, variables)

        self.coefficient = coefficient
        self.operation = Type.add
        self.operands = operands if operands else []

        assert type(self.vars) == dict
        assert type(self.operands) == list
        assert type(self.coefficient) in [float, int]

        assert type(self.operands) == list
        assert len(self.operands) >= 2

    def degree(self, var=None):
        d = self.vars.get(var, 0) if var else sum(self.vars.values())

        return d + max([n.degree(var) for n in self.operands])

    def variables(self):
        return _variables(self)

    def is_number(self):
        return False

    def is_term(self):
        return False

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + str(self.operation) + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join(
            [repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        res = ''
        if self.coefficient == -1:
            res = '-'
        elif self.coefficient != 1:
            res = str(self.coefficient)

        if self.vars:
            res += s_vars + '*'

        in_extra_parenthesis = res != ''
        if in_extra_parenthesis:
            res += '('

        res += '+'.join([n.to_str(True) for n in self.operands])
        if in_extra_parenthesis:
            res += ')'

        if in_parenthesis:
            res = '(' + res + ')'

        return res


class Mul:
    def __init__(self, coefficient=1, variables=None, operands=None):
        self.vars = {}
        incby(self.vars, variables)

        self.coefficient = coefficient
        self.operation = Type.mul
        self.operands = operands if operands else []

        assert type(self.vars) == dict
        assert type(self.operands) == list
        assert type(self.coefficient) in [float, int]
        assert len(self.operands) >= 2

    def degree(self, var=None):
        d = self.vars.get(var, 0) if var else sum(self.vars.values())

        return d + sum([n.degree(var) for n in self.operands])

    def variables(self):
        return _variables(self)

    def is_number(self):
        return False

    def is_term(self):
        return False

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + str(self.operation) + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join(
            [repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        res = ''
        if self.coefficient == -1:
            res = '-'
        elif self.coefficient != 1:
            res = str(self.coefficient)

        if self.vars:
            res += s_vars + '*('

        res += '*'.join([n.to_str(True) for n in self.operands])
        if self.vars:
            res += ')'

        if in_parenthesis:
            res = '(' + res + ')'

        return res


class Inv:
    def __init__(self, coefficient=1, variables=None, operands=None):
        self.vars = {}
        incby(self.vars, variables)

        self.coefficient = coefficient
        self.operation = Type.inv
        self.operands = operands if operands else []

        assert type(self.vars) == dict
        assert type(self.operands) == list
        assert type(self.coefficient) in [float, int]

        assert len(self.operands) == 1

    def degree(self, var=None):
        d = self.vars.get(var, 0) if var else sum(self.vars.values())

        return d - self.operands[0].degree(var)

    def variables(self):
        return _variables(self)

    def is_number(self):
        return False

    def is_term(self):
        return self.operands[0].is_number()

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + str(self.operation) + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join(
            [repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        if (self.coefficient == -1) and self.vars:
            res = '-' + s_vars
        elif (self.coefficient != 1) or not self.vars:
            res = str(self.coefficient) + s_vars
        else:
            res = s_vars

        res += '/' + self.operands[0].to_str(True)

        if in_parenthesis:
            res = '(' + res + ')'

        return res


class One:
    def __init__(self, coefficient=1, variables=None):
        self.vars = {}
        incby(self.vars, variables)

        self.coefficient = coefficient
        self.operation = Type.one
        self.operands = []

        assert type(self.vars) == dict
        assert type(self.coefficient) in [float, int]

    def degree(self, var=None):
        return self.vars.get(var, 0) if var else sum(self.vars.values())

    def variables(self):
        return _variables(self)

    def is_number(self):
        return not self.vars

    def is_term(self):
        return True

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + str(self.operation) + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        around_parenthesis = in_parenthesis and (((self.coefficient != 1) and self.vars) or (self.coefficient < 0))

        res = ''
        if (self.coefficient == -1) and self.vars:
            res = '-'
        elif (self.coefficient != 1) or not self.vars:
            res = str(self.coefficient)

        res += s_vars

        if around_parenthesis:
            res = '(' + res + ')'

        return res


class Log:
    def __init__(self, coefficient=1, variables=None, operands=None):
        self.vars = {}
        incby(self.vars, variables)

        self.coefficient = coefficient
        self.operation = Type.log
        self.operands = operands if operands else []

        assert type(self.vars) == dict
        assert type(self.operands) == list
        assert type(self.coefficient) in [float, int]

        assert len(self.operands) == 1

    def degree(self, var=None):
        return self.vars.get(var, 0) if var else sum(self.vars.values())

    def variables(self):
        return _variables(self)

    def is_number(self):
        return False

    def is_term(self):
        return self.operands[0].is_number()

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + str(self.operation) + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join(
            [repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        res = ''
        if self.coefficient == -1:
            res = '-'
        elif self.coefficient != 1:
            res = str(self.coefficient)

        if self.vars:
            res += s_vars + '*'

        res += 'log ' + self.operands[0].to_str(True)

        if in_parenthesis:
            res = '(' + res + ')'

        return res


def new(operation, coefficient=1, variables=None, operands=None):
    if operation == Type.one:
        assert not operands
        return term(coefficient, variables)

    if operation == Type.add:
        return addition(operands, variables, coefficient)

    if operation == Type.mul:
        return multiplication(operands, variables, coefficient)

    if operation == Type.inv:
        return inverse(operands[0], variables, coefficient)

    if operation == Type.log:
        return logarithm(operands[0], variables, coefficient)

    assert False


def term(coefficient, variables):
    if coefficient == 0:
        return number(0)

    if not variables:
        return number(coefficient)

    return One(coefficient=coefficient, variables=variables)


def number(value):
    return One(coefficient=value)


def variable(name):
    return One(variables={name: 1})


def addition(nodes, variables=None, coefficient=1):
    if coefficient == 0:
        return number(0)

    nodes = [n for n in nodes if n]
    if not nodes:
        return number(0)

    res_vars = {}
    incby(res_vars, variables)

    if len(nodes) == 1:
        n = nodes[0]
        incby(res_vars, n.vars)
        return new(operation=n.operation, coefficient=coefficient * n.coefficient, variables=res_vars,
                   operands=n.operands)

    return Add(operands=nodes, variables=variables, coefficient=coefficient)


def multiplication(nodes, variables=None, coefficient=1):
    if coefficient == 0:
        return number(0)

    nodes = [n for n in nodes if n]
    if not nodes:
        return term(coefficient=coefficient, variables=variables)

    res_vars = {}
    incby(res_vars, variables)

    if len(nodes) == 1:
        n = nodes[0]
        incby(res_vars, n.vars)
        return new(operation=n.operation, coefficient=coefficient * n.coefficient, variables=res_vars,
                   operands=n.operands)

    return Mul(operands=nodes, variables=variables, coefficient=coefficient)


def negative(node):
    return new(operation=node.operation, coefficient=-node.coefficient, operands=node.operands,
               variables=node.vars) if node else None


def inverse(node, variables=None, coefficient=1):
    if coefficient == 0:
        return number(0)

    if not node:
        return term(coefficient=coefficient, variables=variables)

    if (node.coefficient != 0) and (not node.vars) and (node.operation == Type.one):
        if (type(coefficient) == int) and (type(node.coefficient) == int):
            gcd_val = math.gcd(coefficient, node.coefficient)
            res_coefficient = coefficient // gcd_val
            inv_coefficient = node.coefficient // gcd_val
            if inv_coefficient < 0:
                res_coefficient = -res_coefficient
                inv_coefficient = -inv_coefficient

            if inv_coefficient == 1:
                return term(variables=variables, coefficient=res_coefficient)

            return Inv(operands=[number(inv_coefficient)], variables=variables, coefficient=res_coefficient)

        return term(variables=variables, coefficient=coefficient / node.coefficient)

    return Inv(operands=[node], variables=variables, coefficient=coefficient)


def logarithm(node, variables=None, coefficient=1):
    if not node:
        return term(coefficient=coefficient, variables=variables)

    return Log(operands=[node], variables=variables, coefficient=coefficient)


def _variables(node):
    res = set(node.vars.keys())
    for n in node.operands:
        res.update(n.variables())

    return res


def incby(acc_vars, added_vars):
    if not added_vars:
        return

    for (var, power) in added_vars.items():
        acc_vars[var] = acc_vars.setdefault(var, 0) + power
        if acc_vars[var] == 0:
            del acc_vars[var]
