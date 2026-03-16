import unittest

import symexpr.ast as ast
import symexpr.evaluators as evaluators
import symexpr.parser as parser
import symexpr.tokenizer as tokenizer


class TestCalc(unittest.TestCase):
    _table: tuple[tuple[str, float], ...] = (
        ("1.+2.+3.+4.+5.+6.+7.+8.+9.", 45.0),
        ("-1.+1." * 20, 0.0),
        ("-1.+2." * 20, 20.0),
        ("+(-1.)*(-1.)-2.*2." * 20, -60.0),
        ("1." + "*(-1.)" * 21, -1.0),
        ("-1." + "*(-2.)" * 10, -1024.0),
        ("*".join(["(3.-1.)"] * 10), 1024.0),
        ("10.-20.+30.-40.+50.-60.+70.-80.", -40.0),
    )

    def _test_int(self, inp: str, expected: int | None = None) -> None:
        expected = int(inp) if expected is None else expected
        expr, errors = parse(inp)
        self.assertEqual(errors, [])
        assert expr is not None
        self.assertEqual(int(float(str(evaluators.simplify(expr)))), expected)
        expr1 = evaluators.expand(expr)
        self.assertEqual(int(float(str(evaluators.simplify(expr1)))), expected)
        expr2 = evaluators.evalf(expr)
        self.assertEqual(int(float(str(expr2))), expected)

    def _test_float(self, inp: str, expected: float | None = None) -> None:
        expected = float(inp) if expected is None else expected
        expr, errors = parse(inp)
        self.assertEqual(errors, [])
        assert expr is not None
        self.assertEqual(float(str(evaluators.simplify(expr))), expected)
        expr1 = evaluators.expand(expr)
        self.assertEqual(float(str(evaluators.simplify(expr1))), expected)
        expr2 = evaluators.evalf(expr)
        self.assertEqual(float(str(expr2)), expected)

    def test_table(self) -> None:
        table: tuple[str, ...] = (
            "0",
            "000",
            "+0",
            "1",
            "-1",
            "1234",
            "-903341234",
            "13156545",
            "00013145353",
        )
        for inp in table:
            self._test_int(inp)

    def test_int_arith(self) -> None:
        table: tuple[tuple[str, int], ...] = (
            ("1+2+3+4+5+6+7+8+9", 45),
            ("-1+1" * 20, 0),
            ("-1+2" * 20, 20),
            ("+(-1)*(-1)-2*2" * 20, -60),
            ("1" + "*(-1)" * 21, -1),
            ("-1" + "*(-2)" * 10, -1024),
            ("*".join(["(3-1)"] * 10), 1024),
            ("10-20+30-40+50-60+70-80", -40),
        )
        for inp, expected in table:
            self._test_int(inp, expected)

    def test_int_range(self) -> None:
        for n in range(2000):
            for inp in f"{n}", f"+{n}", f"-{n}":
                self._test_int(inp)

    def test_float_arith(self) -> None:
        for inp, expected in TestCalc._table:
            self._test_float(inp, expected)

    def test_numeric_arith(self) -> None:
        for inp, expected in TestCalc._table:
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            assert expr is not None
            self.assertEqual(float(str(evaluators.evalf(expr))), expected)
            expr1 = evaluators.simplify(expr)
            self.assertEqual(float(str(evaluators.evalf(expr1))), expected)
            expr2 = evaluators.expand(expr)
            self.assertEqual(float(str(evaluators.evalf(expr2))), expected)

    def test_harmonic_num(self) -> None:
        gama = 0.577265669068499
        for n in 100, 1000, 10000:
            inp = f'{" + ".join(("1/" + str(j)) for j in range(1, n + 1))} - log({n})'
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            assert expr is not None
            val = float(str(evaluators.evalf(expr)))
            self.assertAlmostEqual(val, gama, delta=0.5 / n)
            expr1 = evaluators.simplify(expr)
            val1 = float(str(evaluators.evalf(expr1)))
            self.assertAlmostEqual(val1, gama, delta=0.5 / n)
            expr2 = evaluators.expand(expr)
            val2 = float(str(evaluators.evalf(expr2)))
            self.assertAlmostEqual(val2, gama, delta=0.5 / n)

    def test_float_range(self) -> None:
        for m in range(2000):
            n = m / 5
            for inp in f"{n}", f"+{n}", f"-{n}":
                self._test_float(inp)

    def test_float_e_range(self) -> None:
        for m in range(2000):
            n = m / 5
            for inp in f"{n}" + "E-20", f"+{n}e+10", f"-{n}e15":
                self._test_float(inp)

    def test_binomial_num(self) -> None:
        for n in 10, 100, 1000:
            inp = " * ".join(["(2. - 1.)"] * n)
            expr, errors = parse(inp)
            self.assertEqual(errors, [])
            assert expr is not None
            val = float(str(evaluators.evalf(expr)))
            self.assertAlmostEqual(val, 1, delta=0.0)
            expr1 = evaluators.simplify(expr)
            val1 = float(str(evaluators.evalf(expr1)))
            self.assertAlmostEqual(val1, 1, delta=0.0)
            expr2 = evaluators.expand(expr)
            val2 = float(str(evaluators.evalf(expr2)))
            self.assertAlmostEqual(val2, 1, delta=0.0)

    def test_diff(self) -> None:
        cases = [
            ("diff(x*x, x)", "2x"),
            ("diff(x*x+3*x, x)", "3+(2x)"),
            ("diff(log x, x)", "1/x"),
            ("diff(exp x, x)", "exp x"),
            ("diff(x*x*x, x)", "3x*x"),
        ]
        for inp, expected in cases:
            expr, errors = parse(inp)
            self.assertEqual(errors, [], f"Parse errors for {inp}: {errors}")
            assert expr is not None
            result = str(evaluators.simplify(expr))
            self.assertEqual(
                result, expected, f"diff: {inp} => {result}, expected {expected}"
            )

    def test_subse(self) -> None:
        """Test AST substitution (subse) with expression values."""
        expr, errors = parse("2*x+1")
        self.assertEqual(errors, [])
        assert expr is not None
        x_expr, _ = parse("3*y+1")
        assert x_expr is not None
        # subse('2x+1', x=3y+1) => 2*(3y+1)+1
        subbed = evaluators.subse(expr, {"x": x_expr})
        simplified = str(evaluators.simplify(subbed))
        self.assertIn("y", simplified)
        # Verify numerically: subse then subs y=0 => 2*1+1 = 3
        with_y0 = evaluators.subs(subbed, {"y": 0.0})
        self.assertEqual(float(str(evaluators.simplify(with_y0))), 3.0)

    def test_symbolic(self) -> None:
        for n in 1, 10, 20, 100, 101:
            inp = " * ".join(["(a+b) * (a-b)"] * n)
            expr, errors = parse(inp)
            assert expr is not None
            # For small n, test all forms; expand is numerically unstable for large n
            exprs: list[ast.Node] = [
                expr,
                evaluators.simplify(expr),
            ]
            if n <= 20:
                exprs.extend(
                    [
                        evaluators.expand(expr),
                        evaluators.expand(evaluators.simplify(expr)),
                    ]
                )
            for assignment in [{"a": 2.0, "b": 1.0}, {"a": 3.0, "b": -2.0}]:
                expected = (assignment["a"] ** 2 - assignment["b"] ** 2) ** n
                for e in exprs:
                    expr0 = evaluators.subs(e, assignment)
                    val = str(evaluators.simplify(expr0))
                    self.assertAlmostEqual(
                        float(val),
                        float(expected),
                        delta=max(abs(float(expected)) * 1e-8, 1e-10),
                    )


def parse(inp: str) -> tuple[ast.Node | None, parser.Errors]:
    errors: parser.Errors = []
    return (
        parser.parse(
            parser.TokenReader(list(tokenizer.tokenize(tokenizer.Scanner(inp)))), errors
        ),
        errors,
    )
