import symexpr.tokenizer as tokenizer
import symexpr.parser as parser
import symexpr.evaluator as evaluator
import unittest


class TestCalc(unittest.TestCase):
    def test_table(self):
        table = (
            '0', '000', '+0', '1', '-1', '1234', '-903341234', '13156545', '00013145353',
        )
        for inp in table:
            expr, errors = parse(inp)
            expected = str(int(inp))
            self.assertEqual(errors, [])
            self.assertEqual(str(evaluator.simplify(expr)), expected)
            expr1 = evaluator.expand(expr)
            self.assertEqual(str(evaluator.simplify(expr1)), expected)
            expr2 = evaluator.evalf(expr)
            self.assertEqual(str(expr2), expected)

    def test_int_arith(self):
        table = (
            ('1+2+3+4+5+6+7+8+9', '45'),
            ('-1+1' * 20, '0'),
            ('-1+2' * 20, '20'),
            ('+(-1)*(-1)-2*2' * 20, '-60'),
            ('1' + '*(-1)' * 21, '-1'),
            ('-1' + '*(-2)' * 10, '-1024'),
            ('*'.join(['(3-1)'] * 10), '1024'),
            ('10-20+30-40+50-60+70-80', '-40'),
        )
        for inp, expected in table:
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            self.assertEqual(str(evaluator.simplify(expr)), expected)
            expr1 = evaluator.expand(expr)
            self.assertEqual(str(evaluator.simplify(expr1)), expected)
            expr2 = evaluator.evalf(expr)
            self.assertEqual(str(expr2), expected)

    def test_int_range(self):
        for n in range(2000):
            for inp in str(n), f'+{n}', f'-{n}':
                expected = str(int(inp))
                expr, errors = parse(inp)
                self.assertEqual(errors, [])
                self.assertEqual(str(evaluator.simplify(expr)), expected)
                expr1 = evaluator.expand(expr)
                self.assertEqual(str(evaluator.simplify(expr1)), expected)
                expr2 = evaluator.evalf(expr)
                self.assertEqual(str(expr2), expected)

    def test_float_arith(self):
        table = (
            ('1.+2.+3.+4.+5.+6.+7.+8.+9.', 45.0),
            ('-1.+1.' * 20, 0.),
            ('-1.+2.' * 20, 20.0),
            ('+(-1.)*(-1.)-2.*2.' * 20, -60.0),
            ('1.' + '*(-1.)' * 21, -1.0),
            ('-1.' + '*(-2.)' * 10, -1024.0),
            ('*'.join(['(3.-1.)'] * 10), 1024.0),
            ('10.-20.+30.-40.+50.-60.+70.-80.', -40.0),
        )
        for inp, expected in table:
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            self.assertEqual(float(str(evaluator.simplify(expr))), expected)
            expr1 = evaluator.expand(expr)
            self.assertEqual(float(str(evaluator.simplify(expr1))), expected)
            expr2 = evaluator.evalf(expr)
            self.assertEqual(float(str(expr2)), expected)

    def test_numeric_arith(self):
        table = (
            ('1.+2.+3.+4.+5.+6.+7.+8.+9.', 45.0),
            ('-1.+1.' * 20, 0.),
            ('-1.+2.' * 20, 20.0),
            ('+(-1.)*(-1.)-2.*2.' * 20, -60.0),
            ('1.' + '*(-1.)' * 21, -1.0),
            ('-1.' + '*(-2.)' * 10, -1024.0),
            ('*'.join(['(3.-1.)'] * 10), 1024.0),
            ('10.-20.+30.-40.+50.-60.+70.-80.', -40.0),
        )
        for inp, expected in table:
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            self.assertEqual(float(str(evaluator.evalf(expr))), expected)
            expr1 = evaluator.simplify(expr)
            self.assertEqual(float(str(evaluator.evalf(expr1))), expected)
            expr2 = evaluator.expand(expr)
            self.assertEqual(float(str(evaluator.evalf(expr2))), expected)

    def test_harmonic_num(self):
        gama = 0.577265669068499
        for n in [100, 1000, 10000]:
            inp = '+'.join(('1/' + str(j)) for j in range(1, n + 1)) + '-log(' + str(n) + ')'
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            val = float(str(evaluator.evalf(expr)))
            self.assertAlmostEqual(val, gama, delta=0.5 / n)
            expr1 = evaluator.simplify(expr)
            val1 = float(str(evaluator.evalf(expr1)))
            self.assertAlmostEqual(val1, gama, delta=0.5 / n)
            expr2 = evaluator.expand(expr)
            val2 = float(str(evaluator.evalf(expr2)))
            self.assertAlmostEqual(val2, gama, delta=0.5 / n)

    def test_float_range(self):
        for m in range(2000):
            n = m / 5
            for inp in str(n), f'+{n}', f'-{n}':
                expected = float(inp)
                expr, errors = parse(inp)
                self.assertEqual(errors, [])
                self.assertEqual(float(str(evaluator.simplify(expr))), expected)
                expr1 = evaluator.expand(expr)
                self.assertEqual(float(str(evaluator.simplify(expr1))), expected)
                expr2 = evaluator.evalf(expr)
                self.assertEqual(float(str(expr2)), expected)

    def test_float_e_range(self):
        for m in range(2000):
            n = m / 5
            for inp in str(n) + 'E-20', f'+{n}e+10', f'-{n}e15':
                expected = float(inp)
                expr, errors = parse(inp)
                self.assertEqual(errors, [])
                self.assertEqual(float(str(evaluator.simplify(expr))), expected)
                expr1 = evaluator.expand(expr)
                self.assertEqual(float(str(evaluator.simplify(expr1))), expected)
                expr2 = evaluator.evalf(expr)
                self.assertEqual(float(str(expr2)), expected)

    def test_binomial_num(self):
        for n in [10, 100, 1000]:
            inp = '*'.join(['(2.-1.)'] * n)
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            val = float(str(evaluator.evalf(expr)))
            self.assertAlmostEqual(val, 1, delta=0.)
            expr1 = evaluator.simplify(expr)
            val1 = float(str(evaluator.evalf(expr1)))
            self.assertAlmostEqual(val1, 1, delta=0.)
            expr2 = evaluator.expand(expr)
            val2 = float(str(evaluator.evalf(expr2)))
            self.assertAlmostEqual(val2, 1, delta=0.)

    def test_symbolic(self):
        for n in 1, 10, 20, 100, 101:
            inp = '*'.join(['(a+b)*(a-b)'] * n)
            expr, errors = parse(inp)
            exprs = [expr, evaluator.simplify(expr), evaluator.expand(expr), evaluator.expand(evaluator.simplify(expr))]
            for assignment in [{'a': 2, 'b': 1}]:
                expected = (assignment['a'] ** 2 - assignment['b'] ** 2) ** n
                for expr in exprs:
                    expr0 = evaluator.subs(expr, assignment)
                    val = str(evaluator.simplify(expr0))
                    self.assertEqual(val, str(expected))


def parse(inp):
    errors = []
    return parser.parse(parser.TokenReader(list(tokenizer.tokenize(tokenizer.Scanner(inp)))), errors), errors
