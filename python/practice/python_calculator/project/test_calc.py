import unittest
from calc import calc


class TestCalculator(unittest.TestCase):
    def test_valid_cases(self):
        # Каждый тестовый кейс состоит из двух элементов:
        # строки с алгебраическим выражением; его значения
        cases = (
            ["12345", 12345],
            ["12.345", 12.345],
            ["2.0+3", 5],
            ["( 1+5.6 )", 6.6],
            ["(8+2* 5) / (1+3*2-4)", 6],
            [" 3 + 4 * 2 / (1 - 5)^2 ", 3.5],
            ["2^ 3", 8],
            ["1-2*3", -5],
            ["(1-2)*3", -3],
            ["(1+(2/2))-(3-5)", 4],
            ["1/2 - 1/2", 0],
            ["2 ^ 2 ^ 3", 64],
            ["8 / 4 / 2", 1],
            [
                "15/(7-(1+1))*3-(2+(1+1))*15/(7-(200+1))*3-(2+(1+1))*(15/(7-(1+1))*3-(2+(1+1))+15/(7-(1+1))*3-(2+(1+1)))",
                -30.0721649485,
            ],
        )

        for raw_expression, res_plan in cases:
            with self.subTest(raw_expression):
                res_fact = calc(raw_expression)
                self.assertAlmostEqual(res_plan, res_fact)

    def test_invalid_cases(self):
        # Каждый тестовый кейс содержит некорректное алгебраическое выражение,
        # для которого функция calc() должна вернуть None
        cases = (
            "",
            " ",
            "()",
            "76-",
            "7^^8",
            "9++2",
            "1.4.4",
            "(0+4",
            "3-5)",
            ")1+1(",
            "6-.",
        )

        for raw_expression in cases:
            res_fact = calc(raw_expression)
            self.assertIs(res_fact, None)
