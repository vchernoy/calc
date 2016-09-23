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

    def __init__(self, loc, typ, lit=None, num=None, err=None):
        self.loc = loc
        self.typ = typ
        self.lit = lit
        self.number = num
        self.err = err

    def __str__(self):
        return '({}:{})'.format(','.join([str(t) for t in [self.typ, self.lit, self.number, self.err] if t]), self.loc)

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

tokenizer = Tokenizer()
reader = Reader('2e-4=4x*  50log(100/6;)')

toks = tokenizer.tokenize(reader)
for t in toks:
    print t,

print