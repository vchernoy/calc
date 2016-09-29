import random
import symexpr.tokenizer as tokenizer
import symexpr.parser as parser
import symexpr.evaluator as evaluator

"""
The interactive symbolic calculator.
Run it as
python3 calc.py
It will prompt for input string (expression)
"""

def all_ways_to_compute(expr):
    return [
        evaluator.simplify(expr),
        evaluator.expand(evaluator.simplify(expr)),
        evaluator.evalf(evaluator.expand(evaluator.simplify(expr))),
        evaluator.simplify(evaluator.evalf(evaluator.expand(evaluator.simplify(expr))))
    ]


n_line = 1
while True:
    inp = input('input ' + str(n_line) + ' > ')
    toks = tokenizer.tokenize(tokenizer.Scanner(inp))
    tok_list = list(toks)
    if (len(tok_list) == 1) and (tok_list[0].typ == tokenizer.Type.eol):
        break

    tok_reader = parser.TokenReader(tok_list)
    errors = []
    expr = parser.parse(tok_reader, errors)

    if errors:
        errors.sort(key=lambda e: e.location)
        buf = [' '] * (len(inp) + 1)
        for err in errors:
            buf[err.location] = '^'
            err_loc = err.location

        print(inp)
        print(''.join(buf))

        for err in errors:
            print(err)

    if expr:
        print('parsed expression:', expr)
        attempts = all_ways_to_compute(expr)
        attempts.sort(key=lambda n: len(str(n)))
        simplified = attempts[0]
        print('simplified expression:', simplified)

        vars = simplified.variables()

        if vars:
            print('equation on', '\'' + '\', \''.join(vars) + '\'', 'is detected')
            var = vars.pop()
            vars.add(var)
            if len(vars) > 1:
                var = [v for v in ['x', 'y', 'z', 'a', 'b', 'c', var] if v in vars][0]

            print('trying to solve equation on variable', var)

            solve_res = evaluator.solve(simplified, var)
            if solve_res:
                root, root_expr = solve_res
                sols = all_ways_to_compute(root) + [root]
                sols.sort(key=lambda n: len(str(n)))
                sol = sols[0]
                print('solution:', root_expr, '=', sol)

                if not sol.is_number():
                    evaluated_sol = evaluator.evalf(sol)
                    if str(evaluated_sol) != str(sol):
                        print('approximate solution:', root_expr, '=', evaluator.evalf(sol))

                if root_expr.degree() == 1:
                    print('checking the solution...')
                    subs_ast = evaluator.subse(simplified, {var: sol})
                    # print 'substituted the solution to the expression: ', subs_ast
                    subs_attempts = all_ways_to_compute(subs_ast)
                    subs_attempts.sort(key=lambda n: len(str(n)))
                    simplified_subs = subs_attempts[0]
                    print('substitutes the solution to the original expression:', simplified_subs)
                    if simplified_subs.is_number() and (simplified_subs.coefficient == 0):
                        print('correct, 0 is expected')
                    elif simplified_subs.is_number() and (abs(simplified_subs.coefficient) < 1e-10):
                        print('correct, close to 0 is expected')
                    elif simplified_subs.variables():
                        free_vars = simplified_subs.variables()
                        print('there are still free variables', free_vars)
                        print('let\'s generate random assignments for them...')
                        assignment = {v: random.uniform(-1000, 1000) for v in free_vars}
                        print('the generated assignment to be substituted is', assignment)
                        final_ast = evaluator.subs(simplified_subs, assignment)
                        final_attempts = all_ways_to_compute(final_ast)
                        final_attempts.sort(key=lambda n: len(str(n)))
                        final_subs = final_attempts[0]
                        print('random assignment gives', final_subs)
                        if final_subs.is_number() and (final_subs.coefficient == 0):
                            print('correct, 0 is expected')
                        elif final_subs.is_number() and (abs(final_subs.coefficient) < 1e-10):
                            print('correct, close to 0 is expected')
                        else:
                            print('ups... something wrong happened, test it more!')

                    else:
                        print('ups... something wrong happened, test it more!')

            else:
                print('cannot solve the equation over', var)

        elif not simplified.is_number():
            evaluated = evaluator.evalf(simplified)
            if str(evaluated) != str(simplified):
                print('approximate evaluation:', evaluator.evalf(simplified))

    n_line += 1
    print()

print('thank you for being with us')
