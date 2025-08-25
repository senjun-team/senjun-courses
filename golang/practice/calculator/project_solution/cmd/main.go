package main

import (
	"calculator/internal/cerrors"
	"calculator/internal/command"
	"fmt"
)

func calc(rawExpression string) (float64, error) {
	var c command.Command
	c.ReadCommand(rawExpression)
	err := c.Tokenize()
	if err != nil {
		return 0, fmt.Errorf("%s : %s", cerrors.ErrCalculate, err)
	}
	err = c.Parse()
	if err != nil {
		return 0, fmt.Errorf("%s : %s", cerrors.ErrCalculate, err)
	}

	res, err := c.Solve()

	if err != nil {
		return 0, fmt.Errorf("%s : %s", cerrors.ErrCalculate, err)
	}

	return res, nil
}

func main() {
	expressions := []string{
		"2+3",             // 5
		"1-2*3",           // -5
		"(1-2)*3",         // -3
		"(1+(2/2))-(3-5)", // 4
		"1/2-1/2",         // 0
	}

	for _, expr := range expressions {
		res, err := calc(expr)
		if err != nil {
			fmt.Println(err)
		} else {
			fmt.Printf("%s = %f\n", expr, res)
		}
	}
}
