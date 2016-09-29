import symexpr.ast as ast
import math
import functools

"""
A set of tools that allow to manipulate with AST
"""

@functools.singledispatch
def simplify(self):
    """
    simplifies the given AST, performars basic transformation, does not open parentheses in a(b+c),
    but can simplify a + b + c or a * b * c
    :param self: AST
    :return: AST
    """
    raise TypeError("cannot simplify", self)


@simplify.register(ast.One)
@simplify.register(ast.Log)
def _simplify(self):
    return self


@simplify.register(ast.Add)
def _add_simplify(self):
    evaluated0 = [simplify(n) for n in self.operands]
    evaluated = []
    for n in evaluated0:
        if (n.operation == ast.Type.add) and (n.coefficient == 1) and not n.vars:
            evaluated += n.operands
        else:
            evaluated.append(n)

    terms = [t for t in evaluated if t.operation == ast.Type.one]
    non_terms = [t for t in evaluated if t.operation != ast.Type.one]

    d = {}
    for t in terms:
        k = t.footprint()
        if k in d:
            d[k] = ast.term(d[k].coefficient + t.coefficient, t.vars)
        else:
            d[k] = t

        if d[k].coefficient == 0:
            del d[k]

    evaluated = list(d.values()) + non_terms

    neg_degree = [t for t in evaluated if t.degree() < 0]
    pos_degree = {}
    for t in evaluated:
        if t.degree() >= 0:
            pos_degree.setdefault(t.degree(), []).append(t)

    terms = []
    for d, l in sorted(pos_degree.items()):
        terms += l

    return ast.addition(terms + neg_degree, coefficient=self.coefficient, variables=self.vars)


@simplify.register(ast.Mul)
def _mul_simplify(self):
    evaluated0 = [simplify(n) for n in self.operands]
    evaluated1 = []

    res_coefficient = self.coefficient
    res_vars = {}
    ast.incby(res_vars, self.vars)

    for n in evaluated0:
        if n.operation == ast.Type.mul:
            ast.incby(res_vars, n.vars)
            res_coefficient *= n.coefficient
            evaluated1 += n.operands
        else:
            evaluated1.append(n)

    evaluated2 = []
    for n in evaluated1:
        assert n.operation != ast.Type.mul

        ast.incby(res_vars, n.vars)
        res_coefficient *= n.coefficient

        if n.operation != ast.Type.one:
            evaluated2.append(simplify(ast.new(operation=n.operation, operands=n.operands)))

    evaluated3 = [n for n in evaluated2 if n.operation != ast.Type.inv]
    inv_coefficient = 1
    for n in evaluated2:
        assert n.operation in [ast.Type.inv, ast.Type.add, ast.Type.log]
        assert n.coefficient == 1
        assert not n.vars

        if n.operation == ast.Type.inv:
            t = n.operands[0]
            ast.incby(res_vars, {v: -p for (v, p) in t.vars.items()})
            inv_coefficient *= t.coefficient
            if t.operation != ast.Type.one:
                evaluated3.append(ast.inverse(ast.new(operation=t.operation, operands=t.operands)))

    evaluated = []
    for n in evaluated3:
        assert n.operation in [ast.Type.inv, ast.Type.add, ast.Type.log]
        assert n.coefficient == 1
        assert not n.vars

        evaluated.append(n)

    pos_vars = {v: p for (v, p) in res_vars.items() if p > 0}
    inv_vars = {v: -p for (v, p) in res_vars.items() if p < 0}

    n = ast.inverse(node=ast.term(inv_coefficient, inv_vars), variables=pos_vars, coefficient=res_coefficient)
    if n.operation == ast.Type.one:
        return ast.multiplication(evaluated, coefficient=n.coefficient, variables=n.vars)

    evaluated += [n]
    return ast.multiplication(evaluated)


@simplify.register(ast.Inv)
def _inv_simplify(self):
    evaluated = simplify(self.operands[0])

    res_vars = {}
    ast.incby(res_vars, self.vars)
    ast.incby(res_vars, {v: -p for (v, p) in evaluated.vars.items()})

    pos_vars = {v: p for (v, p) in res_vars.items() if p > 0}
    inv_vars = {v: -p for (v, p) in res_vars.items() if p < 0}

    if evaluated.operation == ast.Type.one:
        return ast.inverse(
            ast.term(evaluated.coefficient, inv_vars),
            coefficient=self.coefficient,
            variables=pos_vars
        )

    if evaluated.operation == ast.Type.inv:
        return simplify(
            ast.multiplication(
                nodes=[
                    ast.inverse(
                        ast.term(evaluated.coefficient, inv_vars),
                        coefficient=self.coefficient,
                        variables=pos_vars
                    ),
                    evaluated.operands[0]
                ]
            )
        )

    if evaluated.operation == ast.Type.mul:
        inversed_terms = [ast.inverse(t) for t in evaluated.operands if t.operation == ast.Type.inv]
        other_terms = [t for t in evaluated.operands if t.operation != ast.Type.inv]
        return simplify(
            ast.multiplication(
                nodes=[
                          ast.inverse(
                              ast.multiplication(other_terms, coefficient=evaluated.coefficient, variables=inv_vars),
                              coefficient=self.coefficient,
                              variables=pos_vars
                          )
                      ] + inversed_terms
            )
        )

    return ast.inverse(
        simplify(
            ast.new(operation=evaluated.operation, variables=inv_vars, operands=evaluated.operands,
                    coefficient=evaluated.coefficient)
        ),
        coefficient=self.coefficient,
        variables=pos_vars
    )

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
@expand.register(ast.One)
def _expand(self):
    return self

@expand.register(ast.Add)
def _add_expand(self):
    term = ast.term(coefficient=self.coefficient, variables=self.vars)
    expanded = [expand(n) for n in self.operands]

    node = simplify(ast.addition(nodes=expanded))
    if node.operation != ast.Type.add:
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
        if term.operation != ast.Type.add:
            res = [simplify(ast.multiplication(nodes=[term, t])) for t in res]
        else:
            res2 = []
            for t1 in res:
                for t2 in term.operands:
                    res2.append(simplify(ast.multiplication(nodes=[t1, t2])))

            res = simplify(ast.addition(nodes=res2)).operands

    return simplify(ast.addition(nodes=res))


@functools.singledispatch
def solve(self, var):
    """
    Tries to solve the equation given in the form of AST.
    solve('2x-10') => ('5', 'x') meaning that x=5 is the root of 2x-10=0
    solve('2x*x-y-10') => ('(10+y)/2', 'x*x') meaning that x*x=(10+y)/2 is the root of 2x*x-y-10=0
    solve('x*x+x') => None -- failed to solve
    :param self: AST
    :param var: to find root for this variable
    :return: a pair (solution as AST, variable as AST) or None if failed to solve
    """
    raise TypeError("cannot solve", self)


@solve.register(ast.One)
def _one_solve(self, var):
    if var not in self.variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Mul)
def _mul_solve(self, var):
    if var not in self.variables():
        return None

    for n in self.operands:
        if var in n.variables():
            return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Log)
def _log_solve(self, var):
    if var not in self.variables():
        return None

    if var in self.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Inv)
def _inv_solve(self, var):
    if var not in self.variables():
        return None

    if var in self.operands[0].variables():
        return None

    return ast.number(0), ast.variable(var)


@solve.register(ast.Add)
def _add_solve(self, var):
    if var not in self.variables():
        return None

    var_terms = [t for t in self.operands if var in t.variables()]
    non_var_terms = [t for t in self.operands if var not in t.variables()]

    if (self.vars.get(var, 0) < 0):
        return None

    if (self.vars.get(var, 0) > 0) and var_terms:
        return None

    if not var_terms:
        return ast.number(0), ast.variable(var)

    var_power = set()
    for term in var_terms:
        for t in term.operands:
            if var in t.variables():
                return None

        var_power.add(term.vars[var])

    if len(var_power) != 1:
        return None

    power = var_power.pop()

    reduced_var_terms = []
    for term in var_terms:
        reduced_vars = {}
        ast.incby(reduced_vars, term.vars)
        ast.incby(reduced_vars, {var: -power})
        reduced_var_terms.append(
            ast.new(operation=term.operation, operands=term.operands, coefficient=term.coefficient,
                    variables=reduced_vars))

    term1 = expand(ast.negative(ast.addition(non_var_terms)))
    term2 = ast.inverse(ast.addition(reduced_var_terms))

    return (
        ast.multiplication([term1, term2]),
        ast.term(coefficient=1, variables={var: power})
    )


@functools.singledispatch
def evalf(self):
    """
    Computes the expression in floating points, but does not change any variables.s
    :param self: AST
    :return: AST
    """
    raise TypeError("cannot evalf", self)


@evalf.register(ast.One)
def _one_evalf(self):
    return self


@evalf.register(ast.Add)
def _add_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    val = sum([n.coefficient for n in scalars])
    terms = []
    if val != 0:
        terms.append(ast.number(val))

    terms += non_scalars

    return ast.addition(terms, coefficient=self.coefficient, variables=self.vars)


@evalf.register(ast.Mul)
def _mul_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    val = 1
    for n in scalars:
        val *= n.coefficient

    return ast.multiplication(non_scalars, coefficient=self.coefficient * val, variables=self.vars)


@evalf.register(ast.Inv)
def _inv_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    if scalars:
        if scalars[0].coefficient == 0:
            return ast.inverse(ast.number(0), coefficient=self.coefficient, variables=self.vars)

        val = self.coefficient // scalars[0].coefficient
        fval = self.coefficient / scalars[0].coefficient
        if val != fval:
            val = fval

        return ast.term(coefficient=val, variables=self.vars)

    return ast.inverse(non_scalars[0], coefficient=self.coefficient, variables=self.vars)


@evalf.register(ast.Log)
def _log_evalf(self):
    evaluated = [evalf(n) for n in self.operands]
    scalars = [n for n in evaluated if n.is_number()]
    non_scalars = [n for n in evaluated if not n.is_number()]
    if scalars:
        if scalars[0].coefficient > 0:
            val = self.coefficient * math.log(scalars[0].coefficient)

            return ast.term(coefficient=val, variables=self.vars)

        return ast.logarithm(scalars[0], coefficient=self.coefficient, variables=self.vars)

    return ast.logarithm(non_scalars[0], coefficient=self.coefficient, variables=self.vars)


def subs(node, assignment):
    """
    Substitute the values from the map into the variables of the given AST.
    subs('2x+1', x=3) => '2*3+1'
    :param node: AST
    :param assignment: a mapping between free variables and their numerical values
    :return: AST
    """
    evaluated = [subs(n, assignment) for n in node.operands]
    res_coefficient = node.coefficient
    for var, val in assignment.items():
        if var in node.vars:
            power = node.vars[var]
            res_coefficient *= val ** power

    res_var = {v: p for (v, p) in node.vars.items() if v not in assignment}

    return ast.new(operation=node.operation, operands=evaluated, coefficient=res_coefficient, variables=res_var)


def subse(node, assignment):
    """
    Substitute the values (given as ASTs) from the map into the variables of the given AST
    subse('2x+1', x=3y+1) => '2*(3y+1)+1'
    :param node: AST
    :param assignment: a mapping between free variables and their AST values
    :return: AST
    """
    evaluated = [subse(n, assignment) for n in node.operands]
    nodes = []
    for var, val in assignment.items():
        if var in node.vars:
            power = node.vars[var]
            for i in range(power):
                nodes.append(val)

    res_var = {v: p for (v, p) in node.vars.items() if v not in assignment}

    node = ast.new(operation=node.operation, operands=evaluated, coefficient=node.coefficient, variables=res_var)
    return ast.multiplication(nodes + [node])
