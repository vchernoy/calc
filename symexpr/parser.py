import symexpr.tokenizer as tokenizer
import symexpr.ast as ast


class TokenReader:
    """
    Provides to the next token from the input.
    """
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


"""
The sets of tokens that each rule starts from.
Helps in parsing and error handling.
"""

expr_paren_starts = {tokenizer.Type.l_paren}
short_prod_starts = expr_paren_starts | {tokenizer.Type.number, tokenizer.Type.id}
prod_starts = short_prod_starts
sum_starts = prod_starts | {tokenizer.Type.add, tokenizer.Type.sub}
expr_starts = sum_starts

"""
All the valid tokens (no error and no eol tokens)
"""
all_tokens = {
    tokenizer.Type.l_paren, tokenizer.Type.r_paren,
    tokenizer.Type.number, tokenizer.Type.id,
    tokenizer.Type.add, tokenizer.Type.sub,
    tokenizer.Type.mul, tokenizer.Type.div,
    tokenizer.Type.equal
}


def parse(reader, errors):
    """
    Recognizes:
    EE := E [= E]
    :param reader:
    :param errors:
    :return: AST
    """
    tree = parse_expr(reader, errors)
    if find_expected(reader, [tokenizer.Type.equal, tokenizer.Type.eol], errors):
        if reader.look_next().typ == tokenizer.Type.equal:
            reader.move_next()
            tree2 = parse_expr(reader, errors)
            find_expected(reader, [tokenizer.Type.eol], errors)

            tree = ast.addition([tree, ast.negative(tree2)])

    return tree


def parse_expr(reader, errors):
    """
    Recognizes the rule:
    EE := E [= E]

    :param reader:
    :param errors:
    :return: AST
    """
    return parse_sum(reader, errors)


def parse_sum(reader, errors):
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
    if reader.look_next().typ in [tokenizer.Type.add, tokenizer.Type.sub]:
        reader.move_next()

    tree = parse_product(reader, errors)
    if neg:
        tree = ast.negative(tree)

    operands = [tree]

    if find_expected(reader, all_tokens | {tokenizer.Type.eol}, errors):
        while reader.look_next().typ in [tokenizer.Type.add, tokenizer.Type.sub]:
            neg = reader.look_next().typ == tokenizer.Type.sub
            reader.move_next()
            tree = parse_product(reader, errors)
            if neg:
                tree = ast.negative(tree)

            operands.append(tree)

    return ast.addition(operands)


def parse_product(reader, errors):
    """
    Recognizes the rules:
    P := num ('*'|'/' P)*
    P := num (ID | F | PE) ('*'|'/' P)*
    P := (ID | F | PE) ('*'|'/' P)*
    F := 'log' SP
    :param reader:
    :param errors:
    :return: AST
    """
    operation = None
    operands = []
    while True:
        if not find_expected(reader, prod_starts, errors):
            break

        prev_tok = reader.look_next()
        if prev_tok.typ == tokenizer.Type.id:
            name = reader.move_next().name
            if name == 'log':
                tree = ast.logarithm(parse_short_product(reader, errors))
            else:
                tree = ast.variable(name)

        elif prev_tok.typ == tokenizer.Type.number:
            number = reader.move_next().number
            tree = ast.number(number)
        elif prev_tok.typ == tokenizer.Type.l_paren:
            tree = parse_expr_in_parenthesis(reader, errors)
        else:
            assert False

        if operation == ast.OpKind.mul:
            operands.append(tree)
        elif operation == ast.OpKind.inv:
            operands.append(ast.inverse(tree))
        else:
            operands.append(tree)

        if (prev_tok.typ == tokenizer.Type.number) and (operation in (None, ast.OpKind.mul)):
            if not find_expected(reader, all_tokens - {tokenizer.Type.number} | {tokenizer.Type.eol}, errors):
                break
        else:
            if not find_expected(
                    reader,
                    all_tokens - {tokenizer.Type.number, tokenizer.Type.id, tokenizer.Type.l_paren} | {tokenizer.Type.eol},
                    errors
            ):
                break

        tok = reader.look_next()
        if tok.typ == tokenizer.Type.mul:
            operation = ast.OpKind.mul
            reader.move_next()
        elif tok.typ == tokenizer.Type.div:
            operation = ast.OpKind.inv
            reader.move_next()
        elif (prev_tok.typ == tokenizer.Type.number) and (
                    tok.typ in [tokenizer.Type.id, tokenizer.Type.l_paren]) and (operation in [None, ast.OpKind.mul]):
            operation = ast.OpKind.mul
        else:
            break

    return ast.multiplication(operands)


def parse_short_product(reader, errors):
    """
    Recognizes the rules:
    SP := PE
    SP := num
    SP := num ID
    SP := num F
    SP := ID
    SP := F
    F := 'log' SP
    :param reader:
    :param errors:
    :return: AST
    """
    if not find_expected(reader, prod_starts, errors):
        return None

    if reader.look_next().typ == tokenizer.Type.l_paren:
        return parse_expr_in_parenthesis(reader, errors)

    operands = []
    if reader.look_next().typ == tokenizer.Type.number:
        number = reader.move_next().number
        operands.append(ast.number(number))

    if reader.look_next().typ == tokenizer.Type.id:
        name = reader.move_next().name
        if name == 'log':
            operands.append(ast.logarithm(parse_short_product(reader, errors)))
        else:
            operands.append(ast.variable(name))

    return ast.multiplication(operands)


def parse_expr_in_parenthesis(reader, errors):
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

    if find_expected(reader, [tokenizer.Type.r_paren], errors):
        reader.move_next()

    return tree


def find_expected(reader, expected_tokens, errors) -> bool:
    if reader.look_next().typ not in expected_tokens:
        errors.append(
            Error(expected_tokens=expected_tokens, received_token=reader.look_next())
        )
        while (reader.look_next().typ != tokenizer.Type.eol) and reader.look_next().typ not in expected_tokens:
            reader.move_next()

    return reader.look_next().typ in expected_tokens


class Error:
    def __init__(self, expected_tokens, received_token):
        self.location = received_token.location
        self.expected_tokens = expected_tokens
        self.received_token = received_token

        exp_simple_toks = "','".join(str(t.value) for t in expected_tokens if t.value in tokenizer.simple_tokens)
        exp_other_toks = ",".join(str(t.value) for t in expected_tokens if t.value not in tokenizer.simple_tokens)

        exp_toks = f"'{exp_simple_toks}'" if exp_simple_toks else ""
        if exp_other_toks:
            if exp_toks:
                exp_toks += ','

            exp_toks += exp_other_toks

        if received_token.typ == tokenizer.Type.error:
            err = received_token.err
            self.msg = f"error @ {self.location}: invalid char: '{err.next_char}', expected tokens: {exp_toks} (chars: {err.expected_chars})"
        else:
            self.msg = f"error @ {self.location}: unexpected token: {received_token.typ.value}, expected tokens: {exp_toks}"

    def __str__(self) -> str:
        return repr(self)

    def __repr__(self) -> str:
        return self.msg
