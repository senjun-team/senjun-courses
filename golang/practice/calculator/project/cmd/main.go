package main

import (
	"fmt"
)

func calc(rawExpression string) (float64, error) {
	return 0, nil
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
