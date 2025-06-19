# Глава 12. Методы
## Пример метода 
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

Параметр `s` метода `show` называется *получателем*. Получатель часто называют первой буквой имени типа, как в нашем случае.

Объекты способны скрывать часть своей реализации. Мы уже видели, как это достигается в Go: строчными буквами в начале имен. Такое же правило действует и для имен методов. Так метод `show` недоступен из других пакетов. 

```go
package main

import (
	"fmt"
)

func main() {
}

func (s *snake) show() {
	fmt.Print(s.head)
	for i := 0; i < s.length; i++ {
		fmt.Print(s.body)
	}
}

type snake struct {
	head   string
	body   string
	length int
}
```


```go 
package main

import (
	"errors"
	"fmt"
)

func main() {
	var g gameMap
	g.rowsNumber = 4
	g.colsNumber = 5
	g.wall = 'o'
	g.field = '*'
	err := g.newGame(2, 2)
	if err != nil {
		fmt.Println(err)
	}
}

func (s *snake) show() {
	fmt.Print(s.head)
	for i := 0; i < s.length; i++ {
		fmt.Print(s.body)
	}
}

func (g gameMap) newGame(xPos int, yPos int) error {
	var s snake = snake{head: "(:"}
	if yPos < 1 || yPos >= g.rowsNumber-1 ||
		xPos < 1 || xPos > g.colsNumber-len(s.head)-1 {
		return errors.New("size error")
	}

	for i := 0; i < g.rowsNumber; i++ {
		for j := 0; j < g.colsNumber; j++ {
			if i == yPos && j > xPos &&
				j+len(s.head) >= g.colsNumber {
				fmt.Print(string(g.wall))
				break
			} else if j == 0 || j == g.colsNumber-1 ||
				i == 0 || i == g.rowsNumber-1 {
				fmt.Print(string(g.wall))
			} else if i == yPos && j == xPos {
				s.show()
			} else {
				fmt.Print(string(g.field))
			}

		}
		fmt.Println()
	}

	return nil
}

type snake struct {
	head   string
	body   string
	length int
}

type gameMap struct {
	rowsNumber int
	colsNumber int
	wall       rune
	field      rune
}
```

## Модификация получателя внутри метода 
Иногда методу необходимо изменить получатель. Поскольку каждый вызов метода создает копию получателя, то для этого нужно воспользоваться указателем: 

```go 
func (s *snake) respawn() {
	s.length = 0
}
```

В Go принято следующее соглашение. Если какой-либо метод `snake` использует указатель в качетсве получателя, то все методы `snake` должны использовать указатель. Даже если это не требуется компилятором, нам следует преобразовать наш код:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	var s snake = snake{head: "(:", body: "0", length: 6}
	s.respawn()
	s.show()
}

func (s *snake) show() {
	fmt.Print(s.head)
	for i := 0; i < s.length; i++ {
		fmt.Print(s.body)
	}
	fmt.Println()
}

func (s *snake) respawn() {
	s.length = 0
}

type snake struct {
	head   string
	body   string
	length int
}
```

Теперь метод `show` также использует указатель на `snake` в качестве получателя.

Отметим, что указатели используются не только для модификации получателя. Разумным решением по-прежнему является использование указателя и для доступа к большим данным. 

