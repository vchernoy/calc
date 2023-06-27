import itertools
import operator
import random
import symexpr.ast as ast
import symexpr.tokenizer as tokenizer
import symexpr.parser as parser
import symexpr.evaluators as evaluators

"""
The interactive symbolic calculator.
Run it as
python3 calc.py
It will prompt for input string (expression)
"""


def all_ways_to_compute(expr: ast.Node) -> list[ast.Node]:
    return [
        evaluators.simplify(expr),
        evaluators.expand(evaluators.simplify(expr)),
        evaluators.evalf(evaluators.expand(evaluators.simplify(expr))),
        evaluators.simplify(evaluators.evalf(evaluators.expand(evaluators.simplify(expr)))),
    ]


def s_len(n: ast.Node) -> int:
    return len(str(n))


def print_errors(inp: str, errors: parser.Errors):
    errors.sort(key=operator.attrgetter('loc'))
    buf = [' '] * (len(inp) + 1)
    for err in errors:
        buf[err.loc] = '^'

    print(inp)
    print(''.join(buf))

    for err in errors:
        print(err)


def check_solution(simplified: ast.Node, var: str, solution: ast.Node):
    print('checking the solution...')
    subs_ast = evaluators.subse(simplified, {var: solution})
    # print 'substituted the solution to the expression: ', subs_ast
    subs_attempts = all_ways_to_compute(subs_ast)
    subs_attempts.sort(key=s_len)
    simplified_subs = subs_attempts[0]
    print(f'substitutes the solution to the original expression: {simplified_subs}')

    if simplified_subs.numeric() and simplified_subs.coeff == 0:
        print('correct, 0 is expected')
    elif simplified_subs.numeric() and abs(simplified_subs.coeff) < 1e-10:
        print('correct, close to 0 is expected')
    elif ast.all_vars(simplified_subs):
        free_vars = ast.all_vars(simplified_subs)
        print(f'there are still free variables {free_vars}')
        print("let's generate random assignments for them...")
        assignment = {v: random.uniform(-1000, 1000) for v in free_vars}
        print(f'the generated assignment to be substituted is {assignment}')
        final_ast = evaluators.subs(simplified_subs, assignment)
        final_attempts = all_ways_to_compute(final_ast)
        final_attempts.sort(key=s_len)
        final_subs = final_attempts[0]
        print(f'random assignment gives {final_subs}')

        if final_subs.numeric() and final_subs.coeff == 0:
            print('correct, 0 is expected')
        elif final_subs.numeric() and abs(final_subs.coeff) < 1e-10:
            print('correct, close to 0 is expected')
        else:
            print('ups... something wrong happened, test it more!')

    else:
        print('ups... something wrong happened, test it more!')


def main():
    n_line = 1
    while True:
        inp = input(f'input {n_line} > ')
        token_list = list(tokenizer.tokenize(tokenizer.Scanner(inp)))
        if len(token_list) == 1 and token_list[0].typ == tokenizer.Type.eol:
            break

        tok_reader = parser.TokenReader(token_list)
        errors = []
        expr = parser.parse(tok_reader, errors)

        if errors:
            print_errors(inp, errors)

        if expr:
            print(f'parsed expression: {expr}')
            attempts = all_ways_to_compute(expr)
            attempts.sort(key=s_len)
            simplified = attempts[0]
            print(f'simplified expression: {simplified}')

            variables = ast.all_vars(simplified)
            if variables:
                print("equation on '" + "', '".join(variables) + "' is detected")
                var = next(itertools.chain((v for v in 'xyzabc' if v in variables), variables))

                print(f'trying to solve equation on variable {var}')
                solve_res = evaluators.solve(simplified, var)
                if solve_res:
                    root, root_expr = solve_res
                    solutions = all_ways_to_compute(root) + [root]
                    solutions.sort(key=s_len)
                    solution = solutions[0]
                    print(f'solution: {root_expr} = {solution}')

                    if not solution.numeric():
                        evaluated_sol = evaluators.evalf(solution)
                        if str(evaluated_sol) != str(solution):
                            print(f'approximate solution: root_expr = {evaluators.evalf(solution)}')

                    if root_expr.degree() == 1:
                        check_solution(simplified, var, solution)
                else:
                    print(f'cannot solve the equation over {var}')

            elif not simplified.numeric():
                evaluated = evaluators.evalf(simplified)
                if str(evaluated) != str(simplified):
                    print(f'approximate evaluation: {evaluators.evalf(simplified)}')

        n_line += 1
        print()

    print('thank you for being with us')


if __name__ == '__main__':
    main()
