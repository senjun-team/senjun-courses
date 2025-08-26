package main

import (
	"errors"
	"math"
	"testing"
)

type testCaseT struct {
	in      string
	res     float64
	err     error
	comment string
}

func TestCalc(t *testing.T) {
	cases := []func() []testCaseT{testIntegerNumbers,
		testFloatingPointNumbers,
		testOperators,
		testOperatorsPriority,
		testParanthesis,
		testSimpleInvalidExpressions,
		testLongCorrectExpressions}

	for _, c := range cases {
		for _, test := range c() {
			res, err := calc(test.in)
			if parseError(err) != parseError(test.err) {
				t.Fatalf("calc(%s) got error: %s, want error: %s", test.in,
					parseError(err), parseError(test.err))
			}
			if test.err == nil && !isAlmostEqual(res, test.res, 1e-6) {
				t.Fatalf("calc(\"%s\") got result: %.6f, want result: %.6f", test.in,
					res, test.res)
			}
		}
	}
}

func calcErr() error {
	return errors.New("calculation error")
}

func parseError(err error) string {
	if err == nil {
		return "nil"
	}
	return "not nil"
}

func isAlmostEqual(a, b, delta float64) bool {
	return math.Abs(a-b) < delta
}

func testIntegerNumbers() []testCaseT {
	return []testCaseT{
		{
			"0",
			0,
			nil,
			"zero",
		},
		{
			"8",
			8,
			nil,
			"non-zero single-digit integer number",
		},
		{
			"12",
			12,
			nil,
			"many-digit integer number",
		},
		{
			"0123",
			123,
			nil,
			"many-digit int number with leading zero",
		},
		{
			"008",
			8,
			nil,
			"many-digit int number with several leading zeros",
		},
		{
			"9876543210",
			9876543210,
			nil,
			"long integer number",
		},
		{
			"a5",
			0,
			calcErr(),
			"non-digit at the beginning",
		},
		{
			"5$",
			0,
			calcErr(),
			"non-digit at the end",
		},
		{
			"1_2",
			0,
			calcErr(),
			"non-digit in the middle",
		},
		{
			"a",
			0,
			calcErr(),
			"not a digit",
		},
		{
			"abc",
			0,
			calcErr(),
			"several non-digits",
		},
		{
			"-5",
			0,
			calcErr(),
			"negative number",
		},
		{
			"+5",
			0,
			calcErr(),
			"unary plus",
		},
	}
}

func testFloatingPointNumbers() []testCaseT {
	return []testCaseT{
		{
			"1.",
			1,
			nil,
			"floating-point without the fractional part",
		},
		{
			".03",
			0.03,
			nil,
			"floating-point without integer part",
		},
		{
			"..03",
			0,
			calcErr(),
			"no integer part and 2 dots",
		},
		{
			"1.2",
			1.2,
			nil,
			"floating-point with integer and fractional parts",
		},
		{
			"0.1",
			0.1,
			nil,
			"floating-point with 0 integer and non-zero fractional part",
		},
		{
			"1.2345",
			1.2345,
			nil,
			"fractional part with 4 digits",
		},
		{
			"987654.321",
			987654.321,
			nil,
			"integer part with 6 digits",
		},
		{
			"00.1",
			0.1,
			nil,
			"floating-point with leading zeros",
		},
		{
			"1.1000",
			1.1,
			nil,
			"floating-point with trailing zeros",
		},
		{
			"1.a",
			0,
			calcErr(),
			"floating-point with letter in fractional part",
		},
		{
			"1.2e+3",
			0,
			calcErr(),
			"exponential representation",
		},
		{
			"1.2.3",
			0,
			calcErr(),
			"floating-point with 2 dots",
		},
	}
}

func testOperators() []testCaseT {
	return []testCaseT{
		{
			"1+2",
			3,
			nil,
			"expression with single +",
		},
		{
			"1+22+333",
			356,
			nil,
			"expression with chain of + operators",
		},
		{
			"9-11",
			-2,
			nil,
			"expression with single - and negative result",
		},
		{
			"11-9",
			2,
			nil,
			"expression with single - and positive result",
		},
		{
			"58-304-0",
			-246,
			nil,
			"expression with chain of - operators (with 0)",
		},
		{
			"9-1-1-0.5-1.5",
			5,
			nil,
			"expression with chain of - operators",
		},
		{
			"3*11",
			33,
			nil,
			"expression with single *",
		},
		{
			"2*9*100*7",
			12600,
			nil,
			"expression with chain of * operators (integers only)",
		},
		{
			"2.0*9.0*100.0*7.0",
			12600,
			nil,
			"expression with chain of * operators (floating-points only)",
		},
		{
			"49/7",
			7,
			nil,
			"expression with single /",
		},
		{
			"28/2/2/2",
			3.5,
			nil,
			"expression with chain of / operators",
		},
		{
			"5/0.5",
			10,
			nil,
			"expression with / operator and floating-point",
		},
	}
}

func testOperatorsPriority() []testCaseT {
	return []testCaseT{
		{
			"2+5-1",
			6,
			nil,
			"operators with equal priority: +, -",
		},
		{
			"2-5+1",
			-2,
			nil,
			"operators with equal priority: -, +",
		},
		{
			"4*2*3/11",
			2.18181818,
			nil,
			"operators with equal priority: *, /",
		},
		{
			"4/2/3*11",
			7.33333333,
			nil,
			"operators with equal priority: /, *",
		},
		{
			"3-9+8*2",
			10,
			nil,
			"operators with mixed priority: -, +, *",
		},
		{
			"44*150-45/5",
			6591,
			nil,
			"operators with mixed priority: *, -, /",
		},
		{
			"6+400/500-1*1",
			5.8,
			nil,
			"operators with mixed priority: +, /, -, *",
		},
		{
			"1/2-1/2",
			0,
			nil,
			"operations with mixed priority: /, -, /",
		},
		{
			"15+2*6-8",
			19,
			nil,
			"operations with mixed priority: +, *, -",
		},
		{
			"2-503*8+1/7",
			-4021.85714286,
			nil,
			"operations with mixed priority: -, *, +, /",
		},
		{
			"5*2-6/3+5-7-9*0",
			6,
			nil,
			"operators with mixed priority and multiplication by 0",
		},
	}
}

func testParanthesis() []testCaseT {
	return []testCaseT{
		{
			"()",
			0,
			calcErr(),
			"empty parentheses",
		},
		{
			")()",
			0,
			calcErr(),
			"empty invalid parentheses",
		},
		{
			"(45.7)",
			45.7,
			nil,
			"parentheses with single number",
		},
		{
			"[45.7]",
			0,
			calcErr(),
			"square brackets",
		},
		{
			"{45.7}",
			0,
			calcErr(),
			"curly braces",
		},
		{
			"(1)+(2)",
			3,
			nil,
			"numbers with parentheses",
		},
		{
			"((62))",
			62,
			nil,
			"nested parentheses with single number",
		},
		{
			"(1+5.6)",
			6.6,
			nil,
			"expression in parentheses",
		},
		{
			"(8+2*5)/(1+3*2-4)",
			6,
			nil,
			"2 expressions in parentheses",
		},
		{
			"3+4*2/(1-5)*2",
			-1,
			nil,
			"expression in parentheses (in divider)",
		},
		{
			"3*(4+5)",
			27,
			nil,
			"multiplying the sum in parentheses by a number",
		},
		{
			"(1-2)*3",
			-3,
			nil,
			"priority of - operator in parentheses",
		},
		{
			"(1+(2/2))-(3-5)",
			4,
			nil,
			"priority of / and - operators in parentheses",
		},
		{
			"(2-503)*8.0+1/7",
			-4007.85714286,
			nil,
			"parentheses around 1st operator",
		},
		{
			"2-(503*8)+1.0/7",
			-4021.85714286,
			nil,
			"parentheses around 2nd operator",
		},
		{
			"2.0-503*(8+1)/7",
			-644.714285714,
			nil,
			"parentheses around 3rd operator",
		},
		{
			"2-503.0*8+(1/7)",
			-4021.85714286,
			nil,
			"parentheses around 4th operator",
		},
		{
			"((2-503)*8+1)/7",
			-572.428571429,
			nil,
			"nested parentheses",
		},
		{
			"(((2)-((503)))*8+1)/(7)",
			-572.428571429,
			nil,
			"redundant parentheses",
		},
	}
}

func testSimpleInvalidExpressions() []testCaseT {
	return []testCaseT{
		{
			"",
			0,
			calcErr(),
			"empty string",
		},
		{
			"a+b",
			0,
			calcErr(),
			"invalid characters",
		},
		{
			"*",
			0,
			calcErr(),
			"operator-only",
		},
		{
			"/*",
			0,
			calcErr(),
			"two operators without numbers",
		},
		{
			"67-",
			0,
			calcErr(),
			"incorrect placement of - operator",
		},
		{
			"67--",
			0,
			calcErr(),
			"incorrect placement of two - operators",
		},
		{
			"5+-2",
			0,
			calcErr(),
			"incorrect placement of +, - operators",
		},
		{
			"3(4+5)",
			0,
			calcErr(),
			"absent multiply operator before parentheses",
		},
		{
			"(4+5)3",
			0,
			calcErr(),
			"absent multiply operator after parentheses",
		},
	}

}

func testLongCorrectExpressions() []testCaseT {
	return []testCaseT{
		{
			"15/(7-(1+1))*3-(2+(1+1))*15/(7-(200+1))*3-(2+(0.5+1.5))*(15/(7-(1+1))*3-(2+(1+1))+15/(7-(1+1))*3-(2+(1+1)))",
			-30.0721649485,
			nil,
			"long expression with parentheses",
		},
	}

}
