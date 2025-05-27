# Глава 12. Методы
В Go особым образом реализовано объектно-ориентированное программирование — ООП. Под объектом понимается переменная, которая имеет *методы*. Методы — это функции, увязанные с некоторым типом. Вот пример метода `show`, который печатает «змейку» на экран:


```go {.example_for_playground}
package main

import "fmt"

func main() {
	var smallSnake snake = snake{head: "═", body: "═", length: 3}
	var bigSnake snake = snake{head: "(:", body: "0", length: 6}
	smallSnake.show()
	bigSnake.show()
}

func (s snake) show() {
	fmt.Print(s.head)
	for i := 0; i < s.length; i++ {
		fmt.Print(s.body)
	}
	fmt.Println()
}

type snake struct {
	head   string
	body   string
	length int
}
```
```
════
(:000000
```

Объекты способны скрывать часть своей реализации. Мы уже видели, как это достигается в Go: строчными буквами в начале имен. Такое же правило действует и для имен методов.
