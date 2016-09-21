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

        if res and in_parenthesis and self.operation in [Node.add, Node.neg, Node.mul, Node.inv]:
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

            numeric = [n.operands for n in evaluated if n.operation == Node.num]
            symbolic = [n for n in evaluated if n.operation != Node.num]

            val = sum(numeric) if numeric else 0

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

            numeric = [n.operands for n in evaluated if n.operation == Node.num]
            symbolic = [n for n in evaluated if n.operation != Node.num]

            val = 1
            for v in numeric:
                val *= v

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
                res = Node.negative(evaluated)

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
        if reader.has_next() and (reader.look_next().typ == Token.equal):
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
        while reader.has_next() and (reader.look_next().typ in [Token.add, Token.sub]):
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
            tok = reader.look_next()
            if tok.typ == Token.id:
                tree = Node.variable(tok.lit)
                reader.move_next()
            elif tok.typ == Token.number:
                tree = Node.number(tok.number)
                reader.move_next()
            elif tok.typ == Token.l_paren:
                tree = self.parse_expr_in_parenthesis(reader, errors)
            else:
                errors.append(Error(tok.loc, 'expected P-token, received token \'{}\''.format(tok)))
                return None

            if operation == Node.mul:
                operands.append(tree)
            elif operation == Node.inv:
                operands.append(Node.inverse(tree))
            else:
                operands.append(tree)

            if reader.has_next() and (reader.look_next().typ == Token.mul):
                operation = Node.mul
                reader.move_next()
            elif reader.has_next() and (reader.look_next().typ == Token.div):
                operation = Node.inv
                reader.move_next()
            elif reader.has_next() and (tree.operation == Node.num) and (reader.look_next().typ in [Token.id, Token.l_paren]) and (operation in [None, Node.mul]):
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

reader = Reader(input)
toks = tokenizer.tokenize(reader)

tok_reader = Reader([t for t in toks])
parser = Parser()
errors = []
ast = parser.parse(tok_reader, errors)
# for t in toks:
#     print t,
if errors:
    for err in errors:
        print err
        print input
        err_loc = err.loc
        print ' ' * err_loc + '^'

if ast:
    print ast.to_str()

    print ast.simplify().to_str()
    print ast.simplify().simplify().to_str()

    print ast.vars()
    print ast.simplify().vars()
