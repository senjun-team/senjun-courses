package main

import (
	"errors"
	"testing"
)

type testCaseT struct {
	in      string
	res     float64
	err     error
	comment string
}

func TestCalc(t *testing.T) {
	var cases []func() []testCaseT
	cases = append(cases, testIntegerNumbers)

	for _, c := range cases {
		for _, test := range c() {
			res, err := calc(test.in)
			if parseError(err) != parseError(test.err) {
				t.Fatalf("calc(%s) got error: %s, want error: %s", test.in,
					parseError(err), parseError(test.err))
			}
			if test.err == nil && res != test.res {
				t.Fatalf("calc(%s) got result: %f, want result: %f", test.in,
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
