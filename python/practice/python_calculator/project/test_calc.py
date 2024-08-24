import unittest

from calc import calc


class TestCalculator(unittest.TestCase):
    def run_test_cases(self, cases):
        for raw_expression, res_plan, case_name in cases:
            with self.subTest(case_name):
                res_fact = calc(raw_expression)
                if res_plan is None:
                    self.assertIsNone(res_fact)
                else:
                    self.assertAlmostEqual(res_plan, res_fact)

    def test_integer_numbers(self):
        cases = (
            ["0", 0, "zero"],
            ["8", 8, "non-zero single-digit integer number"],
            ["12", 12, "many-digit integer number"],
            ["0123", 123, "many-digit int number starting with zero"],
            ["008", 8, "several-digit number at the beginning"],
            ["9876543210", 9876543210, "long integer number"],
            ["a5", None, "non-digit at the beginning"],
            ["5$", None, "non-digit at the end"],
            ["1_2", None, "non-digit in the middle"],
            ["a", None, "not a digit"],
            ["abc", None, "several non-digits"],
            ["-5", None, "negative number"],
            ["+5", None, "unary plus"],
        )

        self.run_test_cases(cases)

    def test_floating_point_numbers(self):
        cases = (
            ["1.", 1, "floating-point without the fractional part"],
            [".03", 0.03, "floating-point without integer part"],
            ["..03", None, "no integer part and 2 dots"],
            ["1.2", 1.2, "floating-point with integer and fractional parts"],
            [
                "0.1",
                0.1,
                "floating-point with 0 integer and non-zero fractional part",
            ],
            ["1.2345", 1.2345, "fractional part with 4 digits"],
            ["987654.321", 987654.321, "integer part with 6 digits"],
            ["00.1", 0.1, "floating-point with leading zeroes"],
            ["1.1000", 1.1, "floating-point with trailing zeroes"],
            ["1.a", None, "floating-point with letter in fractional part"],
            ["1.2e+3", None, "exponential representation"],
            ["1.2.3", None, "floating-point with 2 dots"],
        )

        self.run_test_cases(cases)

    def test_operators(self):
        cases = (
            ["1+2", 3, "expression with single +"],
            ["1+22+333", 356, "expression with chain of + operators"],
            ["9-11", -2, "expression with single - and negative result"],
            ["11-9", 2, "expression with single - and positive result"],
            [
                "58-304-0",
                -246,
                "expression with chain of - operators (with 0)",
            ],
            ["9-1-1-0.5-1.5", 5, "expression with chain of - operators"],
            ["3*11", 33, "expression with single *"],
            [
                "2*9*100*7",
                12600,
                "expression with chain of * operators (integers only)",
            ],
            [
                "2.0*9.0*100.0*7.0",
                12600,
                "expression with chain of * operators (floating-points only)",
            ],
            ["49/7", 7, "expression with single /"],
            ["28/2/2/2", 3.5, "expression with chain of / operators"],
            [
                "8/4/2",
                1,
                "expression with chain of / operators and integer result",
            ],
            ["5/0.5", 10, "expression with / operator and floating-point"],
        )

        self.run_test_cases(cases)

    def test_operators_priority(self):
        cases = (
            ["2+5-1", 6, "operators with equal priority: +, -"],
            ["2-5+1", -2, "operators with equal priority: -, +"],
            ["4*2*3/11", 2.18181818, "operators with equal priority: *, /"],
            ["4/2/3*11", 7.33333333, "operators with equal priority: /, *"],
            ["3-9+8*2", 10, "operators with mixed priority: -, +, *"],
            ["44*150-45/5", 6591, "operators with mixed priority: *, -, /"],
            [
                "6+400/500-1*1",
                5.8,
                "operators with mixed priority: +, /, -, *",
            ],
            ["1/2-1/2", 0, "operations with mixed priority: /, -, /"],
            ["15+2*6-8", 19, "operations with mixed priority: +, *, -"],
            [
                "2-503*8+1/7",
                -4021.85714286,
                "operations with mixed priority: -, *, +, /",
            ],
            [
                "5*2-6/3+5-7-9*0",
                6,
                "operators with mixed priority and 0 multipliplication",
            ],
        )

        self.run_test_cases(cases)

    def test_parenthesis(self):
        cases = (
            ["()", None, "empty parenthesis"],
            [")()", None, "empty invalid parenthesis"],
            ["(45.7)", 45.7, "parenthesis with single number"],
            ["(1)+(2)", 3, "numbers with parenthesis"],
            ["((62))", 62, "nested parenthesis with single number"],
            ["(1+5.6)", 6.6, "expression in parenthesis"],
            ["(8+2*5)/(1+3*2-4)", 6, "2 expressions in parenthesis"],
            ["3+4*2/(1-5)*2", -1, "expression in parenthesis (in dividor)"],
            ["(1-2)*3", -3, "priority of - operator in parenthesis"],
            [
                "(1+(2/2))-(3-5)",
                4,
                "priority of / and - operators in parenthesis",
            ],
            [
                "(2-503)*8.0+1/7",
                -4007.85714286,
                "parenthesis around 1st operator",
            ],
            [
                "2-(503*8)+1.0/7",
                -4021.85714286,
                "parenthesis around 2nd operator",
            ],
            [
                "2.0-503*(8+1)/7",
                -644.714285714,
                "parenthesis around 3rd operator",
            ],
            [
                "2-503.0*8+(1/7)",
                -4021.85714286,
                "parenthesis around 4th operator",
            ],
            ["((2-503)*8+1)/7", -572.428571429, "nested parenthesis"],
            [
                "(((2)-((503)))*8+1)/(7)",
                -572.428571429,
                "redundant parenthesis",
            ],
        )

        self.run_test_cases(cases)

    def test_simple_invalid_expressions(self):
        cases = (
            ["", None, "empty string"],
            ["a+b", None, "invalid characters"],
            ["*", None, "operator-only"],
            ["/*", None, "two operators without numbers"],
            ["67--", None, "incorrect placement of - operators"],
            ["5+-2", None, "incorrect placement of +, - operators"],
        )

        self.run_test_cases(cases)

    def test_long_correct_expressions(self):
        cases = (
            [
                "15/(7-(1+1))*3-(2+(1+1))*15/(7-(200+1))*3-(2+(0.5+1.5))*(15/(7-(1+1))*3-(2+(1+1))+15/(7-(1+1))*3-(2+(1+1)))",
                -30.0721649485,
                "long expression with parenthesis",
            ],
        )

        self.run_test_cases(cases)


if __name__ == "__main__":
    unittest.main()
