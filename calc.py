import string
import random
from fractions import gcd


class Token:
    l_paren = '('
    r_paren = ')'
    number = 'num'
    id = 'id'
    add = '+'
    sub = '-'
    mul = '*'
    div = '/'
    equal = '='
    error = 'err'
    eol = 'eol'

    def __init__(self, loc, typ, lit=None, num=None, err=None, expected_chars = None, parsed_chars=None, next_chars=None):
        self.loc = loc
        self.typ = typ
        self.lit = lit
        self.number = num
        # for errors only:
        self.err = err
        self.expected_chars = expected_chars
        self.parsed_chars = parsed_chars
        self.next_chars = next_chars

    def __repr__(self):
        return '({}:{})'.format(','.join([str(t) for t in [self.typ, self.lit, self.number, self.err] if t]), self.loc)

    def __str__(self):
        if self.typ == Token.number:
            return str(self.number)

        if self.typ == Token.id:
            return self.lit

        if self.typ == Token.error:
            return self.typ

        return self.typ


class Tokenizer:
    def __init__(self):
        pass

    def tokenize(self, reader):
        while reader.has_next():
            loc = reader.location()
            if reader.expected_next(
                    [Token.l_paren, Token.r_paren, Token.equal, Token.add, Token.sub, Token.mul, Token.div]):
                tok = reader.move_next()
                yield Token(loc, tok)
            elif reader.expected_next(string.digits):
                is_int = True

                num = reader.move_next()
                while reader.expected_next(string.digits):
                    num += reader.move_next()

                if reader.expected_next('.'):
                    is_int = False
                    num += reader.move_next()

                    while reader.expected_next(string.digits):
                        num += reader.move_next()

                if reader.expected_next('Ee'):
                    is_int = False
                    num += reader.move_next()
                    if reader.expected_next([Token.add, Token.sub]):
                        num += reader.move_next()

                    if reader.expected_next(string.digits):
                        num += reader.move_next()
                        while reader.expected_next(string.digits):
                            num += reader.move_next()

                    else:
                        if reader.has_next():
                            yield Token(reader.location(), Token.error,
                                        err='expected digit, while received: ' + reader.look_next(),
                                        expected_chars=['0-9'],
                                        parsed_chars=num,
                                        next_chars=reader.look_next())

                            reader.move_next()
                        else:
                            yield Token(reader.location(), Token.error,
                                        err='expected digit, while reached: EOL',
                                        expected_chars=['0-9'],
                                        parsed_chars=num,
                                        next_chars=Token.eol)

                        continue

                try:
                    if is_int:
                        yield Token(loc, Token.number, num=int(num))
                    else:
                        yield Token(loc, Token.number, num=float(num))
                except ValueError:
                    yield Token(loc, Token.error,
                                err='unexpected error in parsing number',
                                expected_chars=['(0-9)+[.(0-9)*][[E|e][+|-](0-9)+]'],
                                parsed_chars=num,
                                next_chars=reader.look_next())

            elif reader.expected_next(string.letters):
                lit = reader.move_next()
                while reader.expected_next(string.letters):
                    lit += reader.move_next()

                yield Token(loc, Token.id, lit)

            elif reader.expected_next(' \t'):
                reader.move_next()

            else:
                yield Token(loc, Token.error,
                            err='unexpected character: ' + reader.look_next(),
                            expected_chars='(0-9)|(a-z)|(|)|+|-|*|/|=',
                            parsed_chars='',
                            next_chars=reader.look_next())
                reader.move_next()

        yield Token(reader.location(), Token.eol)


class Reader:
    def __init__(self, source):
        self.src = source
        self.loc = -1

    def location(self):
        return self.loc + 1

    def look_next(self):
        if not self.has_next():
            raise Exception("no input")

        return self.src[self.loc + 1]

    def move_next(self):
        tok = self.look_next()
        self.loc += 1
        return tok

    def has_next(self):
        return self.loc + 1 < len(self.src)

    def expected_next(self, expected_vals):
        return self.has_next() and (self.look_next() in expected_vals)


class TokenReader:
    def __init__(self, source):
        self.src = source
        self.loc = -1

    def look_next(self):
        if self.loc + 1 >= len(self.src):
            raise Exception("no input")

        return self.src[self.loc + 1]

    def move_next(self):
        tok = self.look_next()
        self.loc += 1
        return tok


def incby(acc_vars, added_vars):
    if not added_vars:
        return

    for (var, power) in added_vars.iteritems():
        acc_vars[var] = acc_vars.setdefault(var, 0) + power
        if acc_vars[var] == 0:
            del acc_vars[var]


class Node:
    add = '+'
    mul = '*'
    inv = '/'
    one = '1'

    @staticmethod
    def new(operation, coefficient=1, vars=None, operands=None):
        if operation == Node.one:
            assert not operands
            return Node.term(coefficient, vars)

        if operation == Node.add:
            return Node.addition(operands, vars=vars, coefficient=coefficient)

        if operation == Node.mul:
            return Node.multiplication(operands, vars=vars, coefficient=coefficient)

        if operation == Node.inv:
            return Node.inverse(operands[0], vars=vars, coefficient=coefficient)

        assert False

    @staticmethod
    def term(coefficient, vars):
        if coefficient == 0:
            return Node.number(0)

        if not vars:
            return Node.number(coefficient)

        return Node(operation=Node.one, coefficient=coefficient, vars=vars)

    @staticmethod
    def number(val):
        return Node(operation=Node.one, coefficient=val)

    @staticmethod
    def variable(name):
        return Node(operation=Node.one, vars={name: 1})

    @staticmethod
    def addition(nodes, vars=None, coefficient=1):
        if coefficient == 0:
            return Node.number(0)

        nodes = [n for n in nodes if n]
        if not nodes:
            return Node.number(0)

        res_vars = {}
        incby(res_vars, vars)

        if len(nodes) == 1:
            n = nodes[0]
            incby(res_vars, n.vars)
            return Node.new(operation=n.operation, coefficient=coefficient * n.coefficient, vars=res_vars,
                            operands=n.operands)

        return Node(operation=Node.add, operands=nodes, vars=vars, coefficient=coefficient)

    @staticmethod
    def multiplication(nodes, vars=None, coefficient=1):
        if coefficient == 0:
            return Node.number(0)

        nodes = [n for n in nodes if n]
        if not nodes:
            #return None
            return Node.term(coefficient=coefficient, vars=vars)

        res_vars = {}
        incby(res_vars, vars)

        if len(nodes) == 1:
            n = nodes[0]
            incby(res_vars, n.vars)
            return Node.new(operation=n.operation, coefficient=coefficient * n.coefficient, vars=res_vars,
                            operands=n.operands)

        return Node(operation=Node.mul, operands=nodes, vars=vars, coefficient=coefficient)

    @staticmethod
    def negative(node):
        return Node.new(operation=node.operation, coefficient=-node.coefficient, operands=node.operands,
                        vars=node.vars) if node else None

    @staticmethod
    def inverse(node, vars=None, coefficient=1):
        if coefficient == 0:
            return Node.number(0)

        if not node:
            return Node.term(coefficient=coefficient, vars=vars)

        if (node.coefficient != 0) and (not node.vars) and (node.operation == Node.one):
            if (type(coefficient) == int) and (type(node.coefficient) == int):
                gcd_val = gcd(coefficient, node.coefficient)
                res_coefficient = coefficient / gcd_val
                inv_coefficient = node.coefficient / gcd_val
                if inv_coefficient < 0:
                    res_coefficient = -res_coefficient
                    inv_coefficient = -inv_coefficient

                if inv_coefficient == 1:
                    return Node.term(vars=vars, coefficient=res_coefficient)

                return Node(operation=Node.inv, operands=[Node.number(inv_coefficient)], vars=vars,
                            coefficient=res_coefficient)

            return Node.term(vars=vars, coefficient=float(coefficient) / node.coefficient)

        return Node(operation=Node.inv, operands=[node], vars=vars, coefficient=coefficient)

    def __init__(self, operation, coefficient=1, vars=None, operands=None):
        self.vars = {}
        incby(self.vars, vars)

        self.coefficient = coefficient
        self.operation = operation
        self.operands = operands if operands else []

        assert self.operation in [Node.one, Node.mul, Node.add, Node.inv]
        assert type(self.vars) == dict
        assert type(self.operands) == list
        assert type(self.coefficient) in [float, int, long]

        if self.operation in [Node.add, Node.mul]:
            assert type(self.operands) == list
            assert len(self.operands) >= 2

        if self.operation in [Node.inv]:
            assert len(self.operands) == 1

        if self.operation in [Node.one]:
            assert len(self.operands) == 0

    def __str__(self):
        return self.to_str()

    def footprint(self):
        return ','.join([str(v) for v in sorted(self.vars.items())])

    def __repr__(self):
        return '[' + self.operation + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join(
            [repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis=False):
        s_vars = '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        around_parenthesis = in_parenthesis and (
            (self.operation != Node.one) or ((self.coefficient != 1) and self.vars) or (self.coefficient < 0))

        res = ''
        if self.operation == Node.one:
            if (self.coefficient == -1) and self.vars:
                res = '-'
            elif (self.coefficient != 1) or not self.vars:
                res = str(self.coefficient)

            res += s_vars

        elif self.operation == Node.add:
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

        elif self.operation == Node.mul:
            if self.coefficient == -1:
                res = '-'
            elif self.coefficient != 1:
                res = str(self.coefficient)

            if self.vars:
                res += s_vars + '*('

            res += '*'.join([n.to_str(True) for n in self.operands])
            if self.vars:
                res += ')'

        elif self.operation == Node.inv:
            if (self.coefficient == -1) and self.vars:
                res = '-' + s_vars
            elif (self.coefficient != 1) or not self.vars:
                res = str(self.coefficient) + s_vars
            else:
                res = s_vars

            res += '/' + self.operands[0].to_str(True)
        else:
            assert False

        if around_parenthesis:
            res = '(' + res + ')'

        return res

    def simplify(self):
        if self.operation == Node.one:
            return self

        if self.operation == Node.add:
            evaluated0 = [n.simplify() for n in self.operands]
            evaluated = []
            for n in evaluated0:
                if (n.operation == Node.add) and (n.coefficient == 1) and not n.vars:
                    evaluated += n.operands
                else:
                    evaluated.append(n)

            terms = [t for t in evaluated if t.operation == Node.one]
            complex = [t for t in evaluated if t.operation != Node.one]

            d = {}
            for t in terms:
                k = t.footprint()
                if k in d:
                    d[k] = Node.term(d[k].coefficient + t.coefficient, t.vars)
                else:
                    d[k] = t

                if d[k].coefficient == 0:
                    del d[k]

            evaluated = d.values() + complex

            neg_degree = [t for t in evaluated if t.degree() < 0]
            pos_degree = {}
            for t in evaluated:
                if t.degree() >= 0:
                    pos_degree.setdefault(t.degree(), []).append(t)

            terms = []
            for d, l in sorted(pos_degree.items()):
                terms += l

            return Node.addition(terms + neg_degree, coefficient=self.coefficient, vars=self.vars)

        if self.operation == Node.mul:
            evaluated0 = [n.simplify() for n in self.operands]
            evaluated1 = []

            res_coefficient = self.coefficient
            res_vars = {}
            incby(res_vars, self.vars)

            for n in evaluated0:
                if n.operation == Node.mul:
                    incby(res_vars, n.vars)
                    res_coefficient *= n.coefficient
                    evaluated1 += n.operands
                else:
                    evaluated1.append(n)

            evaluated2 = []
            for n in evaluated1:
                assert n.operation != Node.mul

                incby(res_vars, n.vars)
                res_coefficient *= n.coefficient

                if n.operation != Node.one:
                    evaluated2.append(Node.new(operation=n.operation, operands=n.operands).simplify())

            # return Node.multiplication(nodes=evaluated2, coefficient=res_coefficient, vars=res_vars)

            evaluated3 = [n for n in evaluated2 if n.operation != Node.inv]
            inv_coefficient = 1
            for n in evaluated2:
                assert n.operation in [Node.inv, Node.add]
                assert n.coefficient == 1
                assert not n.vars

                if n.operation == Node.inv:
                    t = n.operands[0]
                    incby(res_vars, {v: -p for (v, p) in t.vars.iteritems()})
                    inv_coefficient *= t.coefficient
                    if t.operation != Node.one:
                        evaluated3.append(Node.inverse(Node.new(operation=t.operation, operands=t.operands)))

            # evaluated3 = evaluated2

            evaluated = []
            for n in evaluated3:
                assert n.operation in [Node.inv, Node.add]
                assert n.coefficient == 1
                assert not n.vars

                evaluated.append(n)

            pos_vars = {v: p for (v, p) in res_vars.iteritems() if p > 0}
            inv_vars = {v: -p for (v, p) in res_vars.iteritems() if p < 0}

            n = Node.inverse(node=Node.term(inv_coefficient, inv_vars), vars=pos_vars, coefficient=res_coefficient)
            if (n.operation != Node.one) or (n.coefficient != 1) or n.vars:
                evaluated += [n]

            return Node.multiplication(evaluated)

        if self.operation == Node.inv:
            evaluated = self.operands[0].simplify()

            res_vars = {}
            incby(res_vars, self.vars)
            incby(res_vars, {v: -p for (v, p) in evaluated.vars.iteritems()})

            pos_vars = {v: p for (v, p) in res_vars.iteritems() if p > 0}
            inv_vars = {v: -p for (v, p) in res_vars.iteritems() if p < 0}

            # 2x*x / node
            # node = 5a*b / n
            # 2x*x / 5a*b * n

            if evaluated.operation == Node.one:
                return Node.inverse(
                    Node.term(evaluated.coefficient, inv_vars),
                    coefficient=self.coefficient,
                    vars=pos_vars
                )

            if evaluated.operation == Node.inv:
                return Node.multiplication(
                    nodes=[
                        Node.inverse(
                            Node.term(evaluated.coefficient, inv_vars),
                            coefficient=self.coefficient,
                            vars=pos_vars
                        ),
                        evaluated.operands[0]
                    ]
                ).simplify()

            if evaluated.operation == Node.mul:
                inversed_terms = [Node.inverse(t) for t in evaluated.operands if t.operation == Node.inv]
                other_terms = [t for t in evaluated.operands if t.operation != Node.inv]
                return Node.multiplication(
                    nodes=[
                              Node.inverse(
                                  Node.multiplication(other_terms, coefficient=evaluated.coefficient, vars=inv_vars),
                                  coefficient=self.coefficient,
                                  vars=pos_vars
                              )
                          ] + inversed_terms
                ).simplify()

            return Node.inverse(
                Node.new(operation=evaluated.operation, vars=inv_vars, operands=evaluated.operands,
                         coefficient=evaluated.coefficient).simplify(),
                coefficient=self.coefficient,
                vars=pos_vars
            )

        assert False

    def expand(self):
        if self.operation == Node.one:
            return self

        if self.operation == Node.add:
            term = Node.term(coefficient=self.coefficient, vars=self.vars)
            expanded = [n.expand() for n in self.operands]

            node = Node.addition(nodes=expanded).simplify()
            if node.operation != Node.add:
                return Node.multiplication(nodes=[term, node]).simplify()

            term1 = Node.term(coefficient=node.coefficient, vars=node.vars)

            terms = [Node.multiplication(nodes=[term, t, term1]).simplify() for t in node.operands]
            return Node.addition(terms).simplify()

        if self.operation == Node.mul:
            term = Node.term(coefficient=self.coefficient, vars=self.vars)
            expanded = [term] + [n.expand() for n in self.operands]

            res = [Node.number(1)]
            for term in expanded:
                if term.operation != Node.add:
                    res = [Node.multiplication(nodes=[term, t]).simplify() for t in res]
                else:
                    res2 = []
                    for t1 in res:
                        for t2 in term.operands:
                            res2.append(Node.multiplication(nodes=[t1, t2]).simplify())

                    res = Node.addition(nodes=res2).simplify().operands

            return Node.addition(nodes=res).simplify()

        if self.operation == Node.inv:
            return self

        assert False

    def solve(self, var):
        if var not in self.variables():
            return None

        if self.operation == Node.one:
            return (Node.number(0), Node.variable(var))

        if self.operation == Node.inv:
            if var in self.operands[0].variables():
                return None

            return (Node.number(0), Node.variable(var))

        if self.operation == Node.mul:
            for n in self.operands:
                if var in n.variables():
                    return None

            return (Node.number(0), Node.variable(var))

        if self.operation != Node.add:
            assert False

        if self.vars.get(var, 0) != 0:
            return None

        var_terms = [t for t in self.operands if var in t.variables()]
        non_var_terms = [t for t in self.operands if var not in t.variables()]
        if not var_terms:
            return None

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
            incby(reduced_vars, term.vars)
            incby(reduced_vars, {var: -power})
            reduced_var_terms.append(
                Node.new(operation=term.operation, operands=term.operands, coefficient=term.coefficient,
                         vars=reduced_vars))

        term1 = Node.negative(Node.addition(non_var_terms)).expand()
        term2 = Node.inverse(Node.addition(reduced_var_terms))

        # print 'solve', var, ':', str(term1), ' -- ', str(term2)
        # 3y(2x + x*a + 5y + 3z + 1) == 0
        # 2x + x*a == -(5y + 3z + 1)
        return (
            Node.multiplication([term1, term2]),
            Node.term(coefficient=1, vars={var: power})
        )

        # return Node.multiplication([Node.negative(self.operands[0]), Node.inverse(self.operands[1], vars={var:self.operands[-1].vars[var]})]).simplify()

    def evalf(self):
        evaluated = [n.evalf() for n in self.operands]
        scalars = [n for n in evaluated if n.is_number()]
        non_scalars = [n for n in evaluated if not n.is_number()]

        if self.operation == Node.one:
            return self

        if self.operation == Node.add:
            val = sum([n.coefficient for n in scalars] + [0])
            terms = []
            if val != 0:
                terms.append(Node.number(val))

            terms += non_scalars

            return Node.addition(terms, coefficient=self.coefficient, vars=self.vars)

        if self.operation == Node.mul:
            val = 1
            for n in scalars:
                val *= n.coefficient

            return Node.multiplication(non_scalars, coefficient=self.coefficient * val, vars=self.vars)

        if self.operation == Node.inv:
            if scalars:
                # print self.coefficient , '----', scalars[0].coefficient
                if scalars[0].coefficient == 0:
                    return Node.inverse(Node.number(0), coefficient=self.coefficient, vars=self.vars)

                val = self.coefficient / scalars[0].coefficient
                fval = float(self.coefficient) / scalars[0].coefficient
                if val != fval:
                    val = fval

                return Node.term(coefficient=val, vars=self.vars)

            return Node.inverse(non_scalars[0], coefficient=self.coefficient, vars=self.vars)

        assert False

    def subs(self, assignment):
        evaluated = [n.subs(assignment) for n in self.operands]
        res_coefficient = self.coefficient
        for var, val in assignment.iteritems():
            if var in self.vars:
                pow = self.vars[var]
                res_coefficient *= val ** pow

        res_var = {v: p for (v, p) in self.vars.iteritems() if v not in assignment}

        return Node.new(operation=self.operation, operands=evaluated, coefficient=res_coefficient, vars=res_var)

    def subse(self, assignment):
        evaluated = [n.subse(assignment) for n in self.operands]
        nodes = []
        for var, val in assignment.iteritems():
            if var in self.vars:
                pow = self.vars[var]
                for i in xrange(pow):
                    nodes.append(val)

        res_var = {v: p for (v, p) in self.vars.iteritems() if v not in assignment}

        node = Node.new(operation=self.operation, operands=evaluated, coefficient=self.coefficient, vars=res_var)
        return Node.multiplication(nodes + [node])

    def variables(self):
        res = set(self.vars.keys())
        for n in self.operands:
            res.update(n.variables())

        return res

    def degree(self, var=None):
        d = self.vars.get(var, 0) if var else sum(self.vars.values() + [0])

        if self.operation == Node.one:
            return d

        if self.operation == Node.inv:
            return -self.operands[0].degree(var)

        if self.operation == Node.add:
            return max([n.degree(var) for n in self.operands])

        if self.operation == Node.mul:
            return sum([n.degree(var) for n in self.operands])

    def is_number(self):
        return (self.operation == Node.one) and not self.vars

    def is_term(self):
        if self.operation == Node.one:
            return True

        if self.operation in [Node.add, Node.mul]:
            return False

        if self.operation == Node.inv:
            return self.operands[0].is_number()


class Error:
    def __init__(self, expected_toks, received_tok):
        self.loc = received_tok.loc
        self.expected_toks = expected_toks
        self.received_tok = received_tok

        if received_tok.typ == Token.error:
            self.msg = "error @ {}: invalid character: '{}', expected tokens: {} (characters: {})".format(
                self.loc, received_tok.next_chars, ",".join(expected_toks), received_tok.expected_chars)
        else:
            self.msg = "error @ {}: unexpected token: {}, expected tokens: {}".format(
                self.loc, received_tok, ",".join(expected_toks))



    def __str__(self):
        return repr(self)

    def __repr__(self):
        return self.msg


class Parser:
    expr_paren_starts = {Token.l_paren}
    prod_starts = expr_paren_starts | {Token.number, Token.id}
    sum_starts = prod_starts | {Token.add, Token.sub}
    expr_starts = sum_starts

    all = {Token.l_paren, Token.r_paren, Token.number, Token.id, Token.add, Token.sub, Token.mul, Token.div, Token.equal}

    def __init__(self):
        pass

    def parse(self, reader, errors):
        # EE := E [= E]

        tree = self.parse_expr(reader, errors)
        if self.find_expected(reader, [Token.equal, Token.eol], errors):
            if reader.look_next().typ == Token.equal:
                reader.move_next()
                tree2 = self.parse_expr(reader, errors)
                self.find_expected(reader, [Token.eol], errors)

                tree = Node.addition([tree, Node.negative(tree2)])

        return tree

    def parse_expr(self, reader, errors):
        # E := S

        return self.parse_sum(reader, errors)

    def parse_sum(self, reader, errors):
        # S := ['+'|'-'] P ('+'|'-' P)*

        if not self.find_expected(reader, Parser.sum_starts, errors):
            return None

        neg = reader.look_next().typ == Token.sub
        if reader.look_next().typ in [Token.add, Token.sub]:
            reader.move_next()

        tree = self.parse_product(reader, errors)
        if neg:
            tree = Node.negative(tree)

        operands = [tree]

        if self.find_expected(reader, Parser.all|{Token.eol}, errors):
            while reader.look_next().typ in [Token.add, Token.sub]:
                neg = reader.look_next().typ == Token.sub
                reader.move_next()
                tree = self.parse_product(reader, errors)
                if neg:
                    tree = Node.negative(tree)

                operands.append(tree)

        return Node.addition(operands)

    def parse_product(self, reader, errors):
        # P := num ('*'|'/' P)*
        # P := num (ID | F | PE) ('*'|'/' P)*
        # P := (ID | F | PE) ('*'|'/' P)*

        operation = None
        operands = []
        while True:
            if not self.find_expected(reader, Parser.prod_starts, errors):
                break

            prev_tok = reader.look_next()
            if prev_tok.typ == Token.id:
                tree = Node.variable(prev_tok.lit)
                reader.move_next()
            elif prev_tok.typ == Token.number:
                tree = Node.number(prev_tok.number)
                reader.move_next()
            elif prev_tok.typ == Token.l_paren:
                tree = self.parse_expr_in_parenthesis(reader, errors)
            else:
                assert False # never happens

            if operation == Node.mul:
                operands.append(tree)
            elif operation == Node.inv:
                operands.append(Node.inverse(tree))
            else:
                operands.append(tree)

            if (prev_tok.typ == Token.number) and (operation in [None, Node.mul]):
                if not self.find_expected(reader, Parser.all - {Token.number} | {Token.eol}, errors):
                    break
            else:
                if not self.find_expected(reader, Parser.all - {Token.number, Token.id, Token.l_paren} | {Token.eol}, errors):
                    break


            tok = reader.look_next()
            if tok.typ == Token.mul:
                operation = Node.mul
                reader.move_next()
            elif tok.typ == Token.div:
                operation = Node.inv
                reader.move_next()
            elif (prev_tok.typ == Token.number) and (tok.typ in [Token.id, Token.l_paren]) and \
                    (operation in [None, Node.mul]):
                operation = Node.mul
            else:
                break

        return Node.multiplication(operands)

    def parse_expr_in_parenthesis(self, reader, errors):
        # E := '(' E ')'

        if not self.find_expected(reader, Parser.expr_paren_starts, errors):
            return None

        reader.move_next()
        tree = self.parse_expr(reader, errors)

        if self.find_expected(reader, [Token.r_paren], errors):
            reader.move_next()

        return tree

    def find_expected(self, reader, expected_toks, errors):
        if reader.look_next().typ not in expected_toks:
            errors.append(
                Error(expected_toks=expected_toks, received_tok=reader.look_next())
            )
            while (reader.look_next().typ != Token.eol) and reader.look_next().typ not in expected_toks:
                reader.move_next()

        return reader.look_next().typ in expected_toks

# inp = '(-2 + 3.5 + x + abc) - 2 - 4'
# inp = '2 * 3 + 5z*(2+3x) + 5(2+3)/4*x + 2/3'

# inp = 'x/2+ x/3 + 3/5*(1+x) = 12'
# inp = '10 + 5z*(2+3x) + 10/2*x'
# inp = '(-2 + 3.5 + x + abc) - 2 - 4'

# inp = '-2 * 3(10-5) / 2 *3r=2r/r + 2/(1/(10/(1/10))) + 2/(1/x) + (-(-(-(-x)))) + x*(1+x)*(2+3x) '
# inp = '2/(1/x)'
# inp = '1/(2/x)'

# inp = '10(1+5)-34*20/34/2*3x=10(1+x)'
# inp = '-3(x+1)*(2x-5)*(-x-2)+6x'

# inp = '2/(1/(10/(1/10)))'
# inp = '2/(1/x)'
# inp = '1/x'
# inp = '2 * 3 + 5x*(2+3x) + 5(2+3.5)*4x + 2/3'
#
#
# inp = 'a*(2+1)'
#
# inp = 'x*x*x*z + 5y*x - 2 - x * (b*b+5a) - ((-2)+(5*x*y)+((-5)*a*x)+(-(b*x*b))+(x*z*x*x))'
# inp = 'x*x*x*z + 5y*x - 2 - x * b*b+5a*(-x) - (-2)-(5*x*y)-((-5)*a*x)-(-(b*x*b))-(x*z*x*x)'
# inp = '(a + b) * (a + b)'
# inp = 'x * (1+a)'
# #
# inp = 'x * (b*b+5a - 2*b*b)'
# inp = '-(-x+z)'
#
# inp = '(a - b) * (a - b) * (a - b)'
# inp = '(3+(4-1))*5'
# inp = '2 * x + 0.5 = 1'
# inp = '2x + 1 = 2(1-x)'
# inp = '2(a*x-5/z)=4/z'
# inp = 'x*x/20/w=z+5'
# inp = 'a/(10/b)=w'
#
# inp = 'a*(1/x*(3/(5b)))=30'
# inp = '30*(a/(3a*((1/b)*(1/x)*(1/5))))'
#
# inp = '30*(1/(3*(1/b)))'
# inp = '(1/(3*(1/b)))'
# inp = '1/(3*(1/b))'

# inp = 'x  = 2/2'
# inp = '1 * 1'

tokenizer = Tokenizer()
parser = Parser()

n_line = 1
while True:
    inp = raw_input('input ' + str(n_line) + ' > ')
    toks = tokenizer.tokenize(Reader(inp))
    tok_list = [t for t in toks]
    if (len(tok_list) == 1) and (tok_list[0].typ == Token.eol):
        break

    tok_reader = TokenReader(tok_list)
    errors = []
    ast = parser.parse(tok_reader, errors)

    if errors:
        errors.sort(key=lambda err: err.loc)
        buf = [' '] * (len(inp)+1)
        for err in errors:
            buf[err.loc] = '^'
            err_loc = err.loc

        print inp
        print ''.join(buf)

        for err in errors:
            print err

        if ast:
            print 'parsed expression:', ast

    if ast:
        attempts = [
            ast.simplify(),
            ast.simplify().expand(),
            ast.simplify().expand().evalf(),
            ast.simplify().evalf().simplify()
        ]
        simplified = sorted([(len(str(n)), n) for n in attempts])[0][1]
        print 'simplified expression:', simplified

        vars = simplified.variables()

        if vars:
            print 'equation on', '\'' + '\', \''.join(vars) + '\'', 'is detected'
            var = vars.pop()
            vars.add(var)
            if len(vars) > 1:
                var = [v for v in ['x', 'y', 'z', 'a', 'b', 'c', var] if v in vars][0]

            print 'trying to solve equation on variable', var

            sols = []
            sol = simplified.solve(var)
            print sol[0], sol[1]
            print sol[0].simplify()
            print sol[0].expand()
            if sol:
                sols.append(sol)
                sols.append((sol[0].simplify(), sol[1]))
                sols.append((sol[0].simplify().expand(), sol[1]))
                sols.append((sol[0].simplify().evalf().simplify(), sol[1]))
                sols.append((sol[0].simplify().expand().evalf().simplify(), sol[1]))

            if sols:
                sol = sorted([(len(str(sol[0])), sol) for sol in sols])[0][1]
                print 'solution:', sol[1], '=', sol[0]

                if sol[1].degree() == 1:
                    print 'checking the solution...'
                    subs_ast = simplified.subse({var: sol[0]})
                    # print 'substituted the solution to the expression: ', subs_ast
                    subs_attempts = [
                        subs_ast.simplify().evalf(),
                        subs_ast.simplify().expand().evalf(),
                        subs_ast.simplify().evalf().simplify().evalf()
                    ]
                    simplified_subs = sorted([(len(str(n)), n) for n in subs_attempts])[0][1]
                    print 'substitutes the solution to the original expression:', simplified_subs
                    if simplified_subs.is_number() and (simplified_subs.coefficient == 0):
                        print 'correct, 0 is expected'
                    elif simplified_subs.is_number() and (abs(simplified_subs.coefficient) < 1e-10):
                        print 'correct, close to 0 is expected'
                    elif simplified_subs.variables():
                        free_vars = simplified_subs.variables()
                        print 'there are still free variables, let\'s generate random assignments for them...'
                        assignment = {v: random.uniform(-1000, 1000) for v in free_vars}
                        final_ast = simplified_subs.subs(assignment)
                        final_attempts = [
                            final_ast.simplify().evalf(),
                            final_ast.simplify().expand().evalf(),
                            final_ast.simplify().evalf().simplify().evalf()
                        ]
                        final_subs = sorted([(len(str(n)), n) for n in final_attempts])[0][1]
                        print 'random assignment gives', final_subs
                        if final_subs.is_number() and (final_subs.coefficient == 0):
                            print 'correct, 0 is expected'
                        elif final_subs.is_number() and (abs(final_subs.coefficient) < 1e-10):
                            print 'correct, close to 0 is expected'
                        else:
                            print 'ups... something wrong happened, test it more!'

                    else:
                        print 'ups... something wrong happened, test it more!'

            else:
                print 'cannot solve the equation over', var

    n_line += 1
    print

print 'thank you for being with us'
