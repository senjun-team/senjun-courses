package main

import "fmt"

func swap(p *string, p2 *string) {
	var s = *p
	*p = *p2
	*p2 = s
}

func main() {
	var s string
	var s2 string

	s = ", gophers!"
	s2 = "Hello"

	swap(&s, &s2)
	fmt.Println(s + s2)
}
