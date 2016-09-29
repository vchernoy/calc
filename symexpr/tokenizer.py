import string
import enum


class Type(enum.Enum):
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
    eol = 'EOL'


simple_tokens = {t.value:t for t in [
    Type.l_paren,
    Type.r_paren,
    Type.add,
    Type.sub,
    Type.mul,
    Type.div,
    Type.equal,
]}


class Error:
    def __init__(self, loc, expected_chars, parsed_chars, next_char):
        assert type(loc) == int

        self.location = loc
        self.expected_chars = expected_chars
        self.parsed_chars = parsed_chars
        self.next_char = next_char

    def __str__(self):
        return 'Error @ {}: expected={}, parsed={}, next={}'.format(
            self.location, self.expected_chars, self.parsed_chars, self.next_char
        )


class Token:
    def __init__(self, loc, typ, name=None, num=None, err=None):
        assert type(typ) == Type
        assert type(loc) == int

        self.location = loc
        self.typ = typ
        self.name = name
        self.number = num
        self.err = err

    def __repr__(self):
        return '({}:{})'.format(
            ';'.join([str(t) for t in [self.typ, self.name, self.number, str(self.err)] if 1]), self.location
        )

    def __str__(self):
        if self.typ == Type.number:
            return str(self.number)

        if self.typ == Type.id:
            return self.name

        if self.typ == Type.error:
            return str(self.typ)

        return str(self.typ)


def tokenize(scanner):
    while scanner.has_next():
        loc = scanner.location()
        if scanner.expected_next(simple_tokens):
            lexeme = scanner.move_next()
            yield Token(loc, simple_tokens[lexeme])
        elif scanner.expected_next(string.digits):
            is_int = True

            num = scanner.move_next()
            while scanner.expected_next(string.digits):
                num += scanner.move_next()

            if scanner.expected_next('.'):
                is_int = False
                num += scanner.move_next()

                while scanner.expected_next(string.digits):
                    num += scanner.move_next()

            if scanner.expected_next('Ee'):
                is_int = False
                num += scanner.move_next()
                if scanner.expected_next('+-'):
                    num += scanner.move_next()

                if scanner.expected_next(string.digits):
                    num += scanner.move_next()
                    while scanner.expected_next(string.digits):
                        num += scanner.move_next()

                else:
                    if scanner.has_next():
                        yield Token(
                            loc=scanner.location(),
                            typ=Type.error,
                            err=Error(loc=scanner.location(), expected_chars=['0-9'], parsed_chars=num,
                                      next_char=scanner.look_next())
                        )

                        scanner.move_next()
                    else:
                        yield Token(
                            loc=scanner.location(),
                            typ=Type.error,
                            err=Error(loc=scanner.location(), expected_chars=['0-9'], parsed_chars=num, next_char='')
                        )

                    continue

            try:
                if is_int:
                    yield Token(loc, Type.number, num=int(num))
                else:
                    yield Token(loc, Type.number, num=float(num))
            except ValueError:
                yield Token(
                    loc=loc,
                    typ=Type.error,
                    err=Error(loc=scanner.location(), expected_chars=['(0-9)+[.(0-9)*][[E|e][+|-](0-9)+]'],
                              parsed_chars=num, next_char=scanner.look_next())
                )

        elif scanner.expected_next(string.ascii_lowercase):
            lit = scanner.move_next()
            while scanner.expected_next(string.ascii_lowercase):
                lit += scanner.move_next()

            yield Token(loc, Type.id, lit)

        elif scanner.expected_next(' \t'):
            scanner.move_next()

        else:
            yield Token(
                loc=loc,
                typ=Type.error,
                err=Error(loc=scanner.location(), expected_chars='(0-9)|(a-z)|(|)|+|-|*|/|=', parsed_chars='',
                          next_char=scanner.look_next())
            )
            scanner.move_next()

    yield Token(scanner.location(), Type.eol)


class Scanner:
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

    def expected_next(self, expected_values):
        return self.has_next() and (self.look_next() in expected_values)
