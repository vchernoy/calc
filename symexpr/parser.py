import typing

import symexpr.ast as ast
import symexpr.tokenizer as tokenizer


class Error:
    def __init__(self, expected: set[tokenizer.Type], received: tokenizer.Token):
        self.loc: int = received.loc
        self.expected: set[tokenizer.Type] = expected
        self.received: tokenizer.Token = received

        exp_simple_toks = "','".join(
            str(t.value) for t in expected if t.value in tokenizer.simple_tokens
        )
        exp_other_toks = ",".join(
            str(t.value) for t in expected if t.value not in tokenizer.simple_tokens
        )

        exp_toks = f"'{exp_simple_toks}'" if exp_simple_toks else ""
        if exp_other_toks:
            if exp_toks:
                exp_toks += ","

            exp_toks += exp_other_toks

        if received.typ == tokenizer.Type.error and received.err is not None:
            err = received.err
            self.msg = (
                f"error @ {self.loc}: invalid: '{err.ahead}', "
                f"expected: {exp_toks} (chars: {err.expected})"
            )
        else:
            self.msg = (
                f"error @ {self.loc}: unexpected: {received.typ.value}, "
                f"expected: {exp_toks}"
            )

    def __str__(self) -> str:
        return repr(self)

    def __repr__(self) -> str:
        return self.msg


class TokenReader:
    """
    Provides to the next token from the input.
    """

    def __init__(self, source: list[tokenizer.Token]):
        self.src: list[tokenizer.Token] = source
        self.loc: int = -1

    def look_next(self) -> tokenizer.Token:
        if self.loc + 1 >= len(self.src):
            raise EOFError("no input")

        return self.src[self.loc + 1]

    def move_next(self) -> tokenizer.Token:
        tok = self.look_next()
        self.loc += 1
        return tok


Errors: typing.TypeAlias = list[Error]

"""
The sets of tokens that each rule starts from.
Helps in parsing and error handling.
"""

expr_paren_starts: set[tokenizer.Type] = {tokenizer.Type.l_paren}
short_prod_starts: set[tokenizer.Type] = expr_paren_starts | {
    tokenizer.Type.number,
    tokenizer.Type.id,
}
prod_starts: set[tokenizer.Type] = short_prod_starts
sum_starts: set[tokenizer.Type] = prod_starts | {tokenizer.Type.add, tokenizer.Type.sub}
expr_starts: set[tokenizer.Type] = sum_starts

"""
All the valid tokens (no error and no eol tokens)
"""
all_tokens: set[tokenizer.Type] = {
    tokenizer.Type.l_paren,
    tokenizer.Type.r_paren,
    tokenizer.Type.number,
    tokenizer.Type.id,
    tokenizer.Type.add,
    tokenizer.Type.sub,
    tokenizer.Type.mul,
    tokenizer.Type.div,
    tokenizer.Type.equal,
    tokenizer.Type.comma,
}


def parse(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes:
    EE := E [= E]
    :param reader:
    :param errors:
    :return: AST
    """
    tree = parse_expr(reader, errors)
    if find_expected(reader, {tokenizer.Type.equal, tokenizer.Type.eol}, errors):
        if reader.look_next().typ == tokenizer.Type.equal:
            reader.move_next()
            tree2 = parse_expr(reader, errors)
            find_expected(reader, {tokenizer.Type.eol}, errors)

            if tree is not None and tree2 is not None:
                neg_tree2 = ast.neg(tree2)
                if neg_tree2 is not None:
                    tree = ast.add([tree, neg_tree2])

    return tree


def parse_expr(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rule:
    EE := E [= E]

    :param reader:
    :param errors:
    :return: AST
    """
    return parse_sum(reader, errors)


def parse_sum(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rule:
    S := ['+'|'-'] P ('+'|'-' P)*

    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, sum_starts, errors):
        return None

    neg = reader.look_next().typ == tokenizer.Type.sub
    if reader.look_next().typ in (tokenizer.Type.add, tokenizer.Type.sub):
        reader.move_next()

    tree = parse_product(reader, errors)
    if neg and tree is not None:
        tree = ast.neg(tree)
    operands: list[ast.Node] = [tree] if tree is not None else []

    if find_expected(reader, all_tokens | {tokenizer.Type.eol}, errors):
        while reader.look_next().typ in (tokenizer.Type.add, tokenizer.Type.sub):
            neg = reader.look_next().typ == tokenizer.Type.sub
            reader.move_next()
            tree = parse_product(reader, errors)
            if neg and tree is not None:
                tree = ast.neg(tree)
            if tree is not None:
                operands.append(tree)

    if not operands:
        return None
    return ast.add(operands)


def parse_product(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rules:
    P := num ('*'|'/' P)*
    P := num (ID | F | PE) ('*'|'/' P)*
    P := (ID | F | PE) ('*'|'/' P)*
    F := 'log' SP
    F := 'exp' SP
    F := 'expand' SP
    F := 'evalf' SP
    :param reader:
    :param errors:
    :return: AST
    """
    operation = None
    operands: list[ast.Node] = []
    while True:
        if not find_expected(reader, prod_starts, errors):
            break

        prev_tok = reader.look_next()
        if prev_tok.typ == tokenizer.Type.id:
            tree = parse_var_or_func(reader, errors)
        elif prev_tok.typ == tokenizer.Type.number:
            num = reader.move_next().number
            if num is not None:
                tree = ast.number(num)
            else:
                tree = None
        elif prev_tok.typ == tokenizer.Type.l_paren:
            tree = parse_expr_in_parenthesis(reader, errors)
        else:
            raise ValueError(f"unexpected token in parse_product: {prev_tok.typ}")

        if tree is not None:
            if operation == ast.OpKind.mul:
                operands.append(tree)
            elif operation == ast.OpKind.inv:
                operands.append(ast.inv(tree))
            else:
                operands.append(tree)

        if prev_tok.typ == tokenizer.Type.number and operation in (
            None,
            ast.OpKind.mul,
        ):
            if not find_expected(
                reader,
                all_tokens - {tokenizer.Type.number} | {tokenizer.Type.eol},
                errors,
            ):
                break
        else:
            if not find_expected(
                reader,
                all_tokens
                - {tokenizer.Type.number, tokenizer.Type.id, tokenizer.Type.l_paren}
                | {tokenizer.Type.eol},
                errors,
            ):
                break

        tok = reader.look_next()
        if tok.typ == tokenizer.Type.mul:
            operation = ast.OpKind.mul
            reader.move_next()
        elif tok.typ == tokenizer.Type.div:
            operation = ast.OpKind.inv
            reader.move_next()
        elif (
            prev_tok.typ == tokenizer.Type.number
            and tok.typ in (tokenizer.Type.id, tokenizer.Type.l_paren)
            and operation in (None, ast.OpKind.mul)
        ):
            operation = ast.OpKind.mul
        else:
            break

    if not operands:
        return None
    return ast.mul(operands)


def parse_short_product(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rules:
    SP := PE
    SP := num
    SP := num ID
    SP := num F
    SP := ID
    SP := F
    F := 'log' SP
    F := 'exp' SP
    F := 'expand' SP
    F := 'evalf' SP
    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, prod_starts, errors):
        return None

    if reader.look_next().typ == tokenizer.Type.l_paren:
        return parse_expr_in_parenthesis(reader, errors)

    operands: list[ast.Node] = []
    if reader.look_next().typ == tokenizer.Type.number:
        num_val = reader.move_next().number
        operands.append(ast.number(num_val if num_val is not None else 0))

    if reader.look_next().typ == tokenizer.Type.id:
        node = parse_var_or_func(reader, errors)
        if node is not None:
            operands.append(node)

    if not operands:
        return None
    return ast.mul(operands)


def parse_diff_args(reader: TokenReader, errors: Errors) -> list[ast.Node] | None:
    """
    Recognizes the rules:
    DA := `(` E `,` id `)`
    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, expr_paren_starts, errors):
        return None

    reader.move_next()
    expr = parse_expr(reader, errors)
    if not find_expected(reader, {tokenizer.Type.comma}, errors):
        return None

    reader.move_next()
    if not find_expected(reader, {tokenizer.Type.id}, errors):
        return None

    name = reader.move_next().name or ""
    var = ast.variable(name)
    if find_expected(reader, {tokenizer.Type.r_paren}, errors):
        reader.move_next()

    if expr is None:
        return None
    return [expr, var]


def parse_var_or_func(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rules:
    VF := ID | F
    F := 'log' SP
    F := 'exp' SP
    F := 'expand' SP
    F := 'evalf' SP
    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, {tokenizer.Type.id}, errors):
        return None

    name = reader.move_next().name or ""
    if name == "log":
        arg = parse_short_product(reader, errors)
        return ast.log(arg) if arg is not None else None
    if name == "exp":
        arg = parse_short_product(reader, errors)
        return ast.exp(arg) if arg is not None else None
    if name == "evalf":
        arg = parse_short_product(reader, errors)
        return ast.evalf(arg) if arg is not None else None
    if name == "expand":
        arg = parse_short_product(reader, errors)
        return ast.expand(arg) if arg is not None else None
    if name == "diff":
        diff_args = parse_diff_args(reader, errors)
        if diff_args is None:
            return None
        return ast.diff(diff_args)
    return ast.variable(name)


def parse_expr_in_parenthesis(reader: TokenReader, errors: Errors) -> ast.Node | None:
    """
    Recognizes the rules:
    E := '(' E ')'
    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, expr_paren_starts, errors):
        return None

    reader.move_next()
    tree = parse_expr(reader, errors)

    if find_expected(reader, {tokenizer.Type.r_paren}, errors):
        reader.move_next()

    return tree


def find_expected(
    reader: TokenReader, expected_tokens: set[tokenizer.Type], errors: Errors
) -> bool:
    if reader.look_next().typ not in expected_tokens:
        errors.append(Error(expected=expected_tokens, received=reader.look_next()))
        while (
            reader.look_next().typ != tokenizer.Type.eol
            and reader.look_next().typ not in expected_tokens
        ):
            reader.move_next()

    return reader.look_next().typ in expected_tokens
