import random
import symexpr.tokenizer as tokenizer
import symexpr.parser as parser
import symexpr.evaluators as evaluators


"""
The interactive symbolic calculator.
Run it as
python3 calc.py
It will prompt for input string (expression)
"""


def all_ways_to_compute(expr) -> list:
    return [
        evaluators.simplify(expr),
        evaluators.expand(evaluators.simplify(expr)),
        evaluators.evalf(evaluators.expand(evaluators.simplify(expr))),
        evaluators.simplify(evaluators.evalf(evaluators.expand(evaluators.simplify(expr)))),
    ]


def main():
    n_line = 1
    while True:
        inp = input(f'input {n_line} > ')
        toks = tokenizer.tokenize(tokenizer.Scanner(inp))
        tok_list = list(toks)
        if (len(tok_list) == 1) and (tok_list[0].typ == tokenizer.Type.eol):
            break

        tok_reader = parser.TokenReader(tok_list)
        errors = []
        expr = parser.parse(tok_reader, errors)

        if errors:
            errors.sort(key=lambda e: e.loc)
            buf = [' '] * (len(inp) + 1)
            for err in errors:
                buf[err.loc] = '^'
                err_loc = err.loc

            print(inp)
            print(''.join(buf))

            for err in errors:
                print(err)

        if expr:
            print(f'parsed expression: {expr}')
            attempts = all_ways_to_compute(expr)
            attempts.sort(key=lambda n: len(str(n)))
            simplified = attempts[0]
            print(f'simplified expression: {simplified}')

            variables = simplified.variables()

            if variables:
                print("equation on '" + "', '".join(variables) + "' is detected")
                var = variables.pop()
                variables.add(var)
                if len(variables) > 1:
                    var = [v for v in ('x', 'y', 'z', 'a', 'b', 'c', var) if v in variables][0]

                print(f'trying to solve equation on variable {var}')

                solve_res = evaluators.solve(simplified, var)
                if solve_res:
                    root, root_expr = solve_res
                    sols = all_ways_to_compute(root) + [root]
                    sols.sort(key=lambda n: len(str(n)))
                    sol = sols[0]
                    print(f'solution: {root_expr} = {sol}')

                    if not sol.numeric():
                        evaluated_sol = evaluators.evalf(sol)
                        if str(evaluated_sol) != str(sol):
                            print(f'approximate solution: root_expr = {evaluators.evalf(sol)}')

                    if root_expr.degree() == 1:
                        print('checking the solution...')
                        subs_ast = evaluators.subse(simplified, {var: sol})
                        # print 'substituted the solution to the expression: ', subs_ast
                        subs_attempts = all_ways_to_compute(subs_ast)
                        subs_attempts.sort(key=lambda n: len(str(n)))
                        simplified_subs = subs_attempts[0]
                        print(f'substitutes the solution to the original expression: {simplified_subs}')
                        if simplified_subs.numeric() and (simplified_subs.coefficient == 0):
                            print('correct, 0 is expected')
                        elif simplified_subs.numeric() and (abs(simplified_subs.coefficient) < 1e-10):
                            print('correct, close to 0 is expected')
                        elif simplified_subs.variables():
                            free_vars = simplified_subs.variables()
                            print(f'there are still free variables {free_vars}')
                            print("let's generate random assignments for them...")
                            assignment = {v: random.uniform(-1000, 1000) for v in free_vars}
                            print(f'the generated assignment to be substituted is {assignment}')
                            final_ast = evaluators.subs(simplified_subs, assignment)
                            final_attempts = all_ways_to_compute(final_ast)
                            final_attempts.sort(key=lambda n: len(str(n)))
                            final_subs = final_attempts[0]
                            print(f'random assignment gives {final_subs}')
                            if final_subs.numeric() and (final_subs.coefficient == 0):
                                print('correct, 0 is expected')
                            elif final_subs.numeric() and (abs(final_subs.coefficient) < 1e-10):
                                print('correct, close to 0 is expected')
                            else:
                                print('ups... something wrong happened, test it more!')

                        else:
                            print('ups... something wrong happened, test it more!')

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
