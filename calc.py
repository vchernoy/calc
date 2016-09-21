import string

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

    def __init__(self, loc, typ, lit=None, num=None, err=None):
        self.loc = loc
        self.typ = typ
        self.lit = lit
        self.number = num
        self.err = err

    def __repr__(self):
        return '({}:{})'.format(','.join([str(t) for t in [self.typ, self.lit, self.number, self.err] if t]), self.loc)

    def __str__(self):
        if self.typ in Token.number:
            return str(self.number)
        if self.typ == Token.id:
            return self.lit
        return self.typ

class Tokenizer:
    def __init__(self):
        pass

    def tokenize(self, reader):
        while reader.has_next():
            loc = reader.location()
            if reader.expected_next([Token.l_paren, Token.r_paren, Token.equal, Token.add, Token.sub, Token.mul, Token.div]):
                tok = reader.move_next()
                yield Token(loc, tok)
            elif reader.expected_next(string.digits):
                num = reader.move_next()
                while reader.expected_next(string.digits):
                    num += reader.move_next()

                if reader.expected_next('.'):
                    num += reader.move_next()

                while reader.expected_next(string.digits):
                    num += reader.move_next()

                if reader.expected_next('Ee'):
                    num += reader.move_next()
                    if reader.expected_next([Token.add, Token.sub]):
                        num += reader.move_next()

                    if reader.expected_next(string.digits):
                        num += reader.move_next()
                        while reader.expected_next(string.digits):
                            num += reader.move_next()

                    else:
                        if reader.has_next():
                            yield Token(reader.location(), Token.error, err = 'expected digit, while received: ' + reader.look_next())
                        else:
                            yield Token(reader.location(), Token.error, err = 'expected digit, while reached: EOL')

                        return

                try:
                    yield Token(loc, Token.number, num=int(num))
                except ValueError:
                    yield Token(loc, Token.number, num=float(num))

            elif reader.expected_next(string.letters):
                lit = reader.move_next()
                while reader.expected_next(string.letters):
                    lit += reader.move_next()

                yield Token(loc, Token.id, lit)

            elif reader.expected_next(' \t'):
                reader.move_next()

            else:
                yield Token(loc, Token.error, err = 'unexpected character: ' + reader.look_next())
                return

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

        return self.src[self.loc+1]

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

        return self.src[self.loc+1]

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
    # num = 'num.'
    # var = 'var'
    add = '+.'
    # neg = '-.'
    mul = '*.'
    inv = '/.'
    one = 'one'

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
        return Node(operation=Node.one, vars={name:1})

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
            return Node(operation=n.operation, coefficient=coefficient*n.coefficient, vars=res_vars, operands=n.operands)

        return Node(operation=Node.add, operands=nodes, vars=vars, coefficient=coefficient)

    @staticmethod
    def multiplication(nodes, vars=None, coefficient=1):
        if coefficient == 0:
            return Node.number(0)

        nodes = [n for n in nodes if n]
        if not nodes:
            return Node.term(coefficient=coefficient, vars=vars)

        res_vars = {}
        incby(res_vars, vars)

        if len(nodes) == 1:
            n = nodes[0]
            incby(res_vars, n.vars)
            return Node(operation=n.operation, coefficient=coefficient*n.coefficient, vars=res_vars, operands=n.operands)

        return Node(operation=Node.mul, operands=nodes, vars=vars, coefficient=coefficient)

    @staticmethod
    def negative(node):
        return Node(operation=node.operation, coefficient=-node.coefficient, operands=node.operands, vars=node.vars) if node else None

    @staticmethod
    def inverse(node, vars=None, coefficient=1):
        if coefficient == 0:
            return Node.number(0)

        if not node:
            return Node.term(coefficient=coefficient, vars=vars)

        if (node.coefficient != 0) and (not node.vars) and (node.operation == Node.one):
            return Node.term(vars=vars, coefficient=float(coefficient)/node.coefficient)

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
        assert type(self.coefficient) in [float, int]

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
        return '[' + self.operation + ',' + str(self.coefficient) + ',' + str(self.vars) + ',' + ','.join([repr(o) for o in self.operands]) + ']'

    def to_str(self, in_parenthesis = False):
        res = ''
        if (self.coefficient == -1) and (self.vars or self.operands or (self.operation != Node.one)):
            res += '-'
        elif self.coefficient != 1:
            res += str(self.coefficient)

        if self.vars:
            res += '*'.join(['*'.join([v[0]] * v[1]) for v in sorted(self.vars.items())])

        if self.operation == Node.one:
            if not res:
                res = str(self.coefficient)

        elif self.operation == Node.add:
            if self.vars:
                res += '*'
            in_extra_parenthesis = res != ''
            if in_extra_parenthesis:
                res += '('

            res += '+'.join([n.to_str(True) for n in self.operands])
            if in_extra_parenthesis:
                res += ')'

        elif self.operation == Node.mul:
            if self.vars:
                res += '*('

            res += '*'.join([n.to_str(True) for n in self.operands])
            if self.vars:
                res += ')'

        elif self.operation == Node.inv:
            if not res:
                res = '1'
            res += '/' + self.operands[0].to_str(True)
        else:
            # error
            pass

        if in_parenthesis:
            if (self.operation != Node.one) or ((self.coefficient != 1) and self.vars) or (self.coefficient < 0):
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

            return Node.addition(terms+neg_degree, coefficient=self.coefficient, vars=self.vars)

        if self.operation == Node.mul:
            res_coefficient = self.coefficient
            res_vars = {}
            incby(res_vars, self.vars)
            evaluated0 = [n.simplify() for n in self.operands]
            evaluated = []
            for n in evaluated0:
                incby(res_vars, n.vars)
                res_coefficient *= n.coefficient

                if n.operation == Node.mul:
                    evaluated += n.operands
                elif n.operation == Node.add:
                    evaluated.append(Node.addition(nodes=n.operands))
                elif n.operation == Node.inv:
                    evaluated.append(Node.inverse(node=n.operands[0]))

            return Node.multiplication(evaluated, coefficient=res_coefficient, vars=res_vars)

        if self.operation == Node.inv:
            evaluated = self.operands[0].simplify()

            res_vars = {}
            incby(res_vars, self.vars)
            incby(res_vars, {v:-p for (v, p) in evaluated.vars.iteritems()})

            pos_vars = {v:p for (v,p) in res_vars.iteritems() if p > 0}
            inv_vars = {v:-p for (v,p) in res_vars.iteritems() if p < 0}

            res_coefficient = float(self.coefficient) / evaluated.coefficient

            return Node.inverse(Node(operation=evaluated.operation, vars=inv_vars, operands=evaluated.operands), coefficient=res_coefficient, vars=pos_vars)

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
                            res2.append(Node.multiplication(nodes=[t1,t2]).simplify())

                    res = Node.addition(nodes=res2).simplify().operands

            return Node.addition(nodes=res).simplify()

        if self.operation == Node.inv:
            return self

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

class Error:
    def __init__(self, loc, msg):
        self.loc = loc
        self.msg = msg

    def __str__(self):
        return repr(self)

    def __repr__(self):
        return 'error @ {}: {}'.format(self.loc, self.msg)

class Parser:
    def __init__(self):
        pass

    def parse(self, reader, errors):
        # EE := E [= E]

        tree = self.parse_expr(reader, errors)
        if reader.look_next().typ == Token.equal:
            reader.move_next()
            tree2 = self.parse_expr(reader, errors)
            if reader.look_next().typ != Token.eol:
                errors.append(Error(reader.look_next().loc, 'expected EOL, received token \'{}\''.format(reader.look_next())))

            return Node.addition([tree, Node.negative(tree2)])

        if reader.look_next().typ != Token.eol:
            errors.append(Error(reader.look_next().loc, 'expected \'=\' or EOL, received token \'{}\''.format(reader.look_next())))

        return tree

    def parse_expr(self, reader, errors):
        # E := S

        return self.parse_sum(reader, errors)

    def parse_sum(self, reader, errors):
        # S := ['+'|'-'] P ('+'|'-' P)*

        neg = reader.look_next().typ == Token.sub
        if reader.look_next().typ in [Token.add, Token.sub]:
            reader.move_next()

        tree = self.parse_product(reader, errors)
        if neg:
            tree = Node.negative(tree)

        operands = [tree]

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
                errors.append(Error(prev_tok.loc, 'expected P-token, received token \'{}\''.format(prev_tok)))
                return None

            if operation == Node.mul:
                operands.append(tree)
            elif operation == Node.inv:
                operands.append(Node.inverse(tree))
            else:
                operands.append(tree)

            tok = reader.look_next()
            if tok.typ == Token.mul:
                operation = Node.mul
                reader.move_next()
            elif tok.typ == Token.div:
                operation = Node.inv
                reader.move_next()
            elif (prev_tok.typ == Token.number) and (tok.typ in [Token.id, Token.l_paren]) and (operation in [None, Node.mul]):
                operation = Node.mul
            else:
                break

        return Node.multiplication(operands)

    def parse_expr_in_parenthesis(self, reader, errors):
        # E := '(' E ')'

        if reader.look_next().typ != Token.l_paren:
            errors.append(Error(reader.look_next().loc, 'expected \'(\' or EOL, received token \'{}\''.format(reader.look_next())))
            return None

        reader.move_next()
        tree = self.parse_expr(reader, errors)
        if reader.look_next().typ != Token.r_paren:
            errors.append(Error(reader.look_next().loc, 'expected \')\' or EOL, received token \'{}\''.format(reader.look_next())))
            return tree

        reader.move_next()
        return tree

def parse(s, errors):
    toks = Tokenizer().tokenize(Reader(s))
    tok_reader = TokenReader([t for t in toks])
    return Parser().parse(tok_reader, errors)

tokenizer = Tokenizer()
input = '(-2 + 3.5 + x + abc) - 2 - 4'
# input = '2 * 3 + 5x*(2+3x) + 5(2+3)/4x + 2/3'

# input = '-2 * 3(10-5) / 2 *3r=2r/r + 2/(1/(10/(1/10))) + 2/(1/x) + (-(-(-(-x)))) + x*(1+x)*(2+3x) +'
# reader = Reader('2/(1/x)')
# reader = Reader('1/(2/x)')
# reader = Reader('2/(1/x)')
# reader = Reader('1/(2/x)')
# input = '10(1+5)-34*20/34/2*3x=10(1+x)'
# input = '-3(x+1)*(2x-5)*(-x-2)+6x'
# input = 'x*x*x*z + 5y*x - 2 - x * (b*b+5a) - ((-2)+(5*x*y)+((-5)*a*x)+(-(b*x*b))+(x*z*x*x))'
# input = '2/(1/(10/(1/10)))'
# input = '2/(1/x)'
# input = '1/x'
# input = '2 * 3 + 5x*(2+3x) + 5(2+3.5)*4x + 2/3'


# input = 'a*(2+1)'

# input = 'x*x*x*z + 5y*x - 2 - x * b*b+5a*(-x) - (-2)-(5*x*y)-((-5)*a*x)-(-(b*x*b))-(x*z*x*x)'
# input = '(a + b) * (a + b)'
# input = 'x * (1+a)'
#
# input = 'x * (b*b+5a - 2*b*b)'
# input = '-(-x+z)'

input = '(a - b) * (a - b) * (a - b)'
input = '(3+(4-1))*5'
input = '2 * x + 0.5 = 1'
input = '2x + 1 = 2(1-x)'

toks = tokenizer.tokenize(Reader(input))

tok_list = [t for t in toks]
print 'tokens:', tok_list

tok_reader = TokenReader(tok_list)
parser = Parser()
errors = []

ast = parser.parse(tok_reader, errors)

if errors:
    for err in errors:
        print err
        print input
        err_loc = err.loc
        print ' ' * err_loc + '^'

if ast:
    print 'input:', input

    print ast
    print repr(ast)

    print ast.variables()
    print ast.degree()
    print ast.degree('x')

    print ast.simplify()
    print ast.simplify().simplify()

    print ast.variables()
    print ast.simplify().variables()

    print ast.expand()
    print ast.expand().expand()

    print ast.expand().simplify()

    print 'ast'
    t = ast
    print t
    print
    print 'ast.simplify()'
    t = ast.simplify()
    print t
    print
    print 'ast.expand()'
    t = ast.expand()
    print t
    print
    print 'ast.expand().simplify()'
    t = ast.expand().simplify()
    print t
    print

    print 'ast.simplify().expand()'
    t = ast.simplify().expand()
    print t
    print

    print 'ast.simplify().expand().simplify()'
    t = ast.simplify().expand().simplify()
    print t
    print

    print ast.simplify().expand().simplify()
    print ast.simplify().expand().expand().simplify()
    print parse(str(ast.simplify().expand().expand().simplify()), errors).simplify()
    print errors
