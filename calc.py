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
            return self.id
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

        yield Token(loc, Token.eol)

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

class Node:
    num = 'num.'
    var = 'var'
    add = '+.'
    neg = '-.'
    mul = '*.'
    inv = '/.'

    @staticmethod
    def number(val):
        return Node(operation=Node.num, operands=val)

    @staticmethod
    def variable(name):
        return Node(operation=Node.var, operands=name)

    @staticmethod
    def addition(operands):
        operands = [operand for operand in operands if operand]
        return Node(operation=Node.add, operands=operands) if len(operands) >= 2 else operands[0] if operands else None

    @staticmethod
    def multiplication(operands):
        operands = [operand for operand in operands if operand]
        return Node(operation=Node.mul, operands=operands) if len(operands) >= 2 else operands[0] if operands else None

    @staticmethod
    def negative(operand):
        return Node(operation=Node.neg, operands=operand) if operand else None

    @staticmethod
    def inverse(operand):
        return Node(operation=Node.inv, operands=operand) if operand else None

    def __init__(self, operation, operands):
        self.operation = operation
        self.operands = operands
        if self.operation == Node.var:
            assert type(operands) == str

        if self.operation == Node.num:
            assert type(operands) in [float, int]

        if self.operation in [Node.add, Node.mul]:
            assert type(operands) == list
            assert len(operands) >= 2

        if self.operation in [Node.neg, Node.inv]:
            assert type(operands) != list

    def __str__(self):
        return self.to_str()

    def to_str(self, in_parenthesis = False):
        if self.operation == Node.var:
            res = self.operands
        elif self.operation == Node.num:
            res = str(self.operands)
        elif self.operation == Node.add:
            res = '+'.join([n.to_str(True) for n in self.operands])
        elif self.operation == Node.neg:
            res = '-' + self.operands.to_str(True)
        elif self.operation == Node.mul:
            res = '*'.join([n.to_str(True) for n in self.operands])
        elif self.operation == Node.inv:
            res = '1/' + self.operands.to_str(True)
        else:
            # error
            pass

        if in_parenthesis:
            if self.operation in [Node.add, Node.neg, Node.mul, Node.inv] or ((self.operation == Node.num) and (self.operands < 0)):
                res = '(' + res + ')'

        return res

    def simplify(self):
        if self.operation == Node.var:
            res = self
        elif self.operation == Node.num:
            res = self
        elif self.operation == Node.add:
            evaluated = []
            for n in self.operands:
                evaluated_operand = n.simplify()
                if evaluated_operand.operation == Node.add:
                    evaluated = evaluated + evaluated_operand.operands
                else:
                    evaluated.append(evaluated_operand)

            simple = []
            complex = []
            for n in evaluated:
                if n.as_term(False) == None:
                    complex.append(n)
                else:
                    simple.append(n)

            terms = Node.addition(simple).as_term()
            simple = []
            for t in terms:
                simple.append(Node.multiplication([Node.number(t[0])] + [Node.variable(var) for var in t[1]]))

            evaluated = [Node.addition(simple)] + complex

            neg_degree = []
            pos_degree = {}
            val = 0
            for n in evaluated:
                if n.operation == Node.num:
                    val += n.operands
                elif n.degree() < 0:
                    neg_degree.append(n)
                else:
                    pos_degree.setdefault(n.degree(), []).append(n)


            symbolic = []
            x = [(d, pos_degree[d]) for d in pos_degree]
            x.sort()
            for d,l in x:
                symbolic = symbolic + l

            symbolic += neg_degree


            if symbolic and (val == 0):
                res = Node.addition(symbolic)
            elif symbolic and (val != 0):
                res = Node.addition([Node.number(val)] + symbolic)
            else:
                res = Node.number(val)

        elif self.operation == Node.neg:
            evaluated = self.operands.simplify()
            if evaluated.operation == Node.num:
                res = Node.number(-evaluated.operands)
            elif evaluated.operation == Node.neg:
                res = evaluated.operands
            elif (evaluated.operation == Node.mul) and (evaluated.operands[0].operation == Node.num):
                res = Node.multiplication([Node.number(-evaluated.operands[0].operands)] + evaluated.operands[1:])
            else:
                res = Node.negative(evaluated)

        elif self.operation == Node.mul:
            evaluated = []
            for n in self.operands:
                evaluated_operand = n.simplify()
                if evaluated_operand.operation == Node.mul:
                    evaluated = evaluated + evaluated_operand.operands
                else:
                    evaluated.append(evaluated_operand)

            val = 1
            vars = []
            others = []
            for n in evaluated:
                if n.operation == Node.num:
                    val *= n.operands
                elif n.operation == Node.var:
                    vars.append((n.operands, n))
                else:
                    others.append(n)

            vars.sort()
            symbolic = [v[1] for v in vars] + others

            if symbolic and (val == 1):
                res = Node.multiplication(symbolic)
            elif symbolic and (val != 1):
                res = Node.multiplication([Node.number(val)] + symbolic)
            else:
                res = Node.number(val)

        elif self.operation == Node.inv:
            evaluated = self.operands.simplify()
            if evaluated.operation == Node.num:
                res = Node.number(1. / float(evaluated.operands))
            elif (evaluated.operation == Node.inv) and len(evaluated.vars()) == 0:
                res = evaluated.operands
            else:
                res = Node.inverse(evaluated)

        return res


    def expand(self):
        if self.operation == Node.var:
            res = self
        elif self.operation == Node.num:
            res = self
        elif self.operation == Node.add:
            res = Node.addition([n.expand().simplify() for n in self.operands])
        elif self.operation == Node.neg:
            if self.operands.operation == Node.add:
                res = Node.addition([Node.negative(n).simplify().expand() for n in self.operands.operands])
            else:
                res = Node.negative(self.operands.expand()).simplify()
        elif self.operation == Node.mul:
            expanded = []
            for n in self.operands:
                operand = n.simplify().expand().simplify()
                expanded.append(operand)

            simple = []
            complex = []
            for n in expanded:
                if n.as_term() == None:
                    complex.append(n)
                else:
                    simple.append(n)

            terms = Node.multiplication(simple).as_term()
            simple = []
            for t in terms:
                simple.append(Node.multiplication([Node.number(t[0])] + [Node.variable(var) for var in t[1]]))

            return Node.multiplication([Node.addition(simple)] + complex).simplify()

        elif self.operation == Node.inv:
            res = self

        return res


    def vars(self):
        if self.operation == Node.num:
            return set()

        if self.operation == Node.var:
            return set(self.var)

        if self.operation in [Node.neg, Node.inv]:
            return self.operands.vars()

        res = set()
        for n in self.operands:
            res.update(n.vars())

        return res

    def degree(self, var=None):
        if self.operation == Node.num:
            return 0

        if self.operation == Node.var:
            return 1 if not var or (var == self.operands) else 0

        if self.operation == Node.neg:
            return self.operands.degree(var)

        if self.operation == Node.inv:
            return -self.operands.degree(var)

        if self.operation == Node.add:
            return max([n.degree(var) for n in self.operands])

        if self.operation == Node.mul:
            return sum([n.degree(var) for n in self.operands])

    def as_term(self, expand=True):
        if self.operation == Node.num:
            return [(self.operands, [])]

        if self.operation == Node.var:
            return [(1, [self.operands])]

        if self.operation == Node.neg:
            terms = self.operands.as_term(expand)
            if terms == None:
                return None

            return [(-t[0], t[1]) for t in terms]

        if self.operation == Node.inv:
            terms = self.operands.as_term(expand)
            if not terms or (len(terms) > 1) or terms[0][1]:
                return None

            return [(1./terms[0][0], [])]

        if self.operation == Node.add:
            terms = []
            for n in self.operands:
                t = n.as_term(expand)
                if t == None:
                    return None

                terms = terms + t

            str_terms = {}
            for t in terms:
                s = ','.join(t[1])
                if s in str_terms:
                    str_terms[s] = (str_terms[s][0] + t[0], t[1])
                else:
                    str_terms[s] = t

                if str_terms[s][0] == 0:
                    del str_terms[s]

            return str_terms.values() if str_terms else [(0, [])]

        if self.operation == Node.mul:
            terms_list = []
            for n in self.operands:
                terms = n.as_term(expand)
                if terms == None:
                    return None

                terms_list.append(terms)

            # 2x 4y = 8x y
            # 2x (z + 3y) = 2x*z + 6x*y
            # 2x (z + 3y) (2a + b) =
            def expand_tems(terms_list, expand):
                if not terms_list:
                    return []

                res_terms = [(1,[])]
                for terms in terms_list:
                    expanded_terms = []
                    if expand or ((len(res_terms) == 1) and (len(terms) == 1)):
                        for t1 in res_terms:
                            for t2 in terms:
                                expanded_terms.append((t1[0]*t2[0], sorted(t1[1]+t2[1])))
                    else:
                        return None

                    res_terms = expanded_terms

                return res_terms

            terms = expand_tems(terms_list, expand)
            if terms == None:
                return None

            str_terms = {}
            for t in terms:
                s = ','.join(t[1])
                if s in str_terms:
                    str_terms[s] = (str_terms[s][0] + t[0], t[1])
                else:
                    str_terms[s] = t

                if str_terms[s][0] == 0:
                    del str_terms[s]

            return str_terms.values() if str_terms else [(0, [])]

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
            if reader.look_next().typ == Token.id:
                tree = Node.variable(reader.look_next().lit)
                reader.move_next()
            elif reader.look_next().typ == Token.number:
                tree = Node.number(reader.look_next().number)
                reader.move_next()
            elif reader.look_next().typ == Token.l_paren:
                tree = self.parse_expr_in_parenthesis(reader, errors)
            else:
                errors.append(Error(reader.look_next().loc, 'expected P-token, received token \'{}\''.format(reader.look_next())))
                return None

            if operation == Node.mul:
                operands.append(tree)
            elif operation == Node.inv:
                operands.append(Node.inverse(tree))
            else:
                operands.append(tree)

            if reader.look_next().typ == Token.mul:
                operation = Node.mul
                reader.move_next()
            elif reader.look_next().typ == Token.div:
                operation = Node.inv
                reader.move_next()
            elif (tree.operation == Node.num) and (reader.look_next().typ in [Token.id, Token.l_paren]) and (operation in [None, Node.mul]):
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
# reader = Reader('(-2 + 3.5 + x + abc) - 2 - 4')
# reader = Reader('2 * 3 + 5x*(2+3x) + 5(2+3)/4x + 2/3')
# reader = Reader('2 * 3 + 5x*(2+3x) + 5(2+3.5)*4x + 2/3')
# reader = Reader('-2 * 3(10-5) / 2 *3r=2r/r + 2/(1/(10/(1/10))) + 2/(1/x) + (-(-(-(-x)))) + x*(1+x)*(2+3x) +')

input = '-2 * 3(10-5) / 2 *3r=2r/r + 2/(1/(10/(1/10))) + 2/(1/x) + (-(-(-(-x)))) + x*(1+x)*(2+3x) +'
# reader = Reader('2/(1/(10/(1/10)))')
# reader = Reader('2/(1/x)')
# reader = Reader('1/(2/x)')
# reader = Reader('2/(1/x)')
# reader = Reader('1/(2/x)')
input = '10(1+5)-34*20/34/2*3x=10(1+x)'
input = '-3(x+1)*(2x-5)*(-x-2)+6x'
input = 'x*x*x*z + 5y*x - 2 - x * (b*b+5a) - ((-2)+(5*x*y)+((-5)*a*x)+(-(b*x*b))+(x*z*x*x))'
# input = '(a + b) * (a + b)'
# input = 'x * (1+a)'

# input = 'x * (b*b+5a - 2*b*b)'
toks = tokenizer.tokenize(Reader(input))
tok_reader = TokenReader([t for t in toks])
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
    print input

    print ast
    print ast.degree()
    print ast.degree('x')

    print ast.simplify()
    print ast.simplify().simplify()

    print ast.vars()
    print ast.simplify().vars()

    print ast.expand()
    print ast.expand().expand()

    print ast.expand().simplify()

    print 'ast'
    t = ast
    print t
    print t.as_term()
    print t.as_term(False)
    print
    print 'ast.simplify()'
    t = ast.simplify()
    print t
    print t.as_term()
    print t.as_term(False)
    print
    print 'ast.expand()'
    t = ast.expand()
    print t
    print t.as_term()
    print t.as_term(False)
    print
    print 'ast.expand().simplify()'
    t = ast.expand().simplify()
    print t
    print t.as_term()
    print t.as_term(False)
    print

    print 'ast.simplify().expand()'
    t = ast.simplify().expand()
    print t
    print t.as_term()
    print t.as_term(False)
    print

    print 'ast.simplify().expand().simplify()'
    t = ast.simplify().expand().simplify()
    print t
    print t.as_term()
    print t.as_term(False)
    print

    print ast.simplify().expand().simplify()
    print ast.simplify().expand().expand().simplify()
    print parse(str(ast.simplify().expand().expand().simplify()), errors).simplify()
    print errors
