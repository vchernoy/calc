import enum
import string
from collections.abc import Iterable
from typing import Generator

import symexpr.ast as ast


class Type(enum.Enum):
    """
    Defines the types of tokens that are created by tokenizer as result of processing the input string.
    When tokenizer (Lexer) reaches the end of the input, it generates eol-token.
    When it finds an invalid character, it creates an err-token.
    """
    l_paren = '('
    r_paren = ')'
    number = 'num'
    id = 'id'
    add = '+'
    sub = '-'
    mul = '*'
    div = '/'
    equal = '='
    comma = ','
    error = 'err'
    eol = 'EOL'


simple_tokens: dict[str, Type] = {t.value: t for t in [
    Type.l_paren,
    Type.r_paren,
    Type.add,
    Type.sub,
    Type.mul,
    Type.div,
    Type.equal,
    Type.comma,
]}


class Error:
    def __init__(self, loc: int, expected: list[str], parsed: str, ahead: str):
        assert type(loc) == int

        self.loc: int = loc
        self.expected: list[str] = expected
        self.parsed: str = parsed
        self.ahead: str = ahead

    def __str__(self) -> str:
        return f'Error @ {self.loc}: expected={self.expected}, parsed={self.parsed}, next={self.ahead}'


class Token:
    """
    Each token contains its type, see above, and location -- the starting position in the input.
    If typ is Type.id, the token contains name (of variable or 'log')
    If typ is Type.number, the token contains number (integer of float)
    If type is Type.error, the token contains err representing the Lexer error.
    """
    def __init__(self, loc: int, typ: Type, name: str = None, num: ast.Num = None, err: Error = None):
        assert type(typ) == Type
        assert type(loc) == int

        self.loc: int = loc
        self.typ: Type = typ
        self.name: str = name
        self.number: ast.Num = num
        self.err = err

    def __repr__(self) -> str:
        return f'({self.typ};{self.name};{self.number};{self.err}:{self.loc})'

    def __str__(self) -> str:
        if self.typ == Type.number:
            return str(self.number)

        if self.typ == Type.id:
            return self.name

        if self.typ == Type.error:
            return str(self.typ)

        return str(self.typ)


class Scanner:
    """
    Scanner provides the characters in the input stream. The scanner can look at one single character in ahead.
    """
    def __init__(self, source: str):
        self._src = source
        self._loc: int = -1

    def loc(self) -> int:
        return self._loc + 1

    def look_next(self) -> str:
        if not self.has_next():
            raise Exception("no input")

        return self._src[self.loc()]

    def move_next(self) -> str:
        tok = self.look_next()
        self._loc += 1
        return tok

    def has_next(self) -> bool:
        return self.loc() < len(self._src)

    def expected_next(self, expected_values: Iterable[str]) -> bool:
        return self.has_next() and self.look_next() in expected_values


def tokenize(scanner: Scanner) -> Generator[Token, None, None]:
    """
    It is the generator that creates tokens from the scanner's output
    :param scanner:
    """
    while scanner.has_next():
        loc = scanner.loc()
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
                            loc=scanner.loc(),
                            typ=Type.error,
                            err=Error(loc=scanner.loc(), expected=['0-9'], parsed=num, ahead=scanner.look_next())
                        )

                        scanner.move_next()
                    else:
                        yield Token(
                            loc=scanner.loc(),
                            typ=Type.error,
                            err=Error(loc=scanner.loc(), expected=['0-9'], parsed=num, ahead='')
                        )

                    continue

            try:
                yield Token(loc, Type.number, num=int(num) if is_int else float(num))
            except ValueError:
                yield Token(
                    loc=loc,
                    typ=Type.error,
                    err=Error(loc=scanner.loc(), expected=['(0-9)+[.(0-9)*][[E|e][+|-](0-9)+]'], parsed=num, ahead=scanner.look_next())
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
                err=Error(loc=scanner.loc(), expected=['(0-9)|(a-z)|(|)|+|-|*|/|='], parsed='', ahead=scanner.look_next())
            )
            scanner.move_next()

    yield Token(scanner.loc(), Type.eol)
