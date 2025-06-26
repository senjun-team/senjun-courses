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

Реализуйте метод `newGame` структуры `gameMap`. Структура `gameMap` состоит из четырех полей: `rowsNumber` — числа строк, `colsNumber` — числа столбцов, `wall` — руны символа препятствия, `field` — руны символа свободной клетки.  Метод `newGame` должен напечатать на экран карту со змейкой. По краям этой карты — препятствия. Остальное — свободные клетки. Он должен принимать в качестве параметров позиции змейки `0 <= xPos` — позицию по оси `x` типа `int`, `0 <= yPos` — позицию по оси `y` типа `int`. Змейка должна иметь нулевую длину, нулевое тело и голову в виде символов `(:`. Например, для карты размером `rowsNumber=10` и `colsNumber=15`, когда `wall='o'`, `field='*'` и `xPos=4`, `yPos=7` метод `newGame` должен напечатать:  {.task_text}

```
ooooooooooooooo
o*************o
o*************o
o*************o
o*************o
o*************o
o*************o
o***(:********o
o*************o
ooooooooooooooo
```

Метод `newGame` возвращает ошибку. В случае, когда змейка оказывается за пределами карты или упирается в стенку, верните ошибку с сообщением `size error`. Во всех остальных случаях в качестве ошибки верните `nil`. {.task_text}
 
```go {.task_source #golang_chapter_0120_task_0010}
package main

import (
	"fmt"
)

func main() {
	var g gameMap
	g.rowsNumber = 10
	g.colsNumber = 15
	g.wall = 'o'
	g.field = '*'
	err := g.newGame(4, 7)
	if err != nil {
		fmt.Println(err)
	}
}

func (s snake) show() {
	fmt.Print(s.head)
	for i := 0; i < s.length; i++ {
		fmt.Print(s.body)
	}
}

// ваш код здесь 

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

Учитывайте длину головы через `len(s.head)`. {.task_hint}

```go {.task_answer}
package main

import (
	"errors"
	"fmt"
)

func main() {
	var g gameMap
	g.rowsNumber = 10
	g.colsNumber = 15
	g.wall = 'o'
	g.field = '*'
	err := g.newGame(4, 7)
	if err != nil {
		fmt.Println(err)
	}
}

func (s snake) show() {
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

В Go принято следующее соглашение. Если какой-либо метод `snake` использует указатель в качестве получателя, то все методы `snake` должны использовать указатель. Даже если это не требуется компилятором, нам следует преобразовать наш код:

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

`Nikolaev A.E.` ожидает получить 200 долларов от компании `OOO "Horns and Hooves"`. Однако, судя по выводу, не получает ничего. Найдите ошибку в коде. {.task_text}

```go {.task_source #golang_chapter_0120_task_0020}
package main

import "fmt"

func main() {
	var t transaction
	t.fromWho = `OOO "Horns and Hooves"`
	t.toWhere = `Nikolaev A.E.`
	t.setAmount(200)
	fmt.Println(t.getAmount())
}

func (t transaction) setAmount(amount dollars) {
	t.amount = amount
}

func (t transaction) getAmount() dollars {
	return t.amount
}

type transaction struct {
	fromWho string
	toWhere string
	amount  dollars
}

type dollars float64
```

Вспомните, что модифицировать получатель внутри метода возможно только через указатель. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func main() {
	var t transaction
	t.fromWho = `OOO "Horns and Hooves"`
	t.toWhere = `Nikolaev A.E.`
	t.setAmount(200)
	fmt.Println(t.getAmount())
}

// ошибка была здесь!
// метод setAmount должен модифицировать 
// transaction через указатель
func (t *transaction) setAmount(amount dollars) {
	t.amount = amount
}

// по нашему соглашению getAmount
// теперь тоже должен работать 
// через указатель 
func (t *transaction) getAmount() dollars {
	return t.amount
}

type transaction struct {
	fromWho string
	toWhere string
	amount  dollars
}

type dollars float64
```

## Пустой получатель 
Если вы используете указатель в качестве получателя, то этот указтель может быть равен `nil`. Такую ситуацию имеет смысл корректно обрабатывать, о чем дополнительно пишут в комментарии к функции.

```go {.example_for_playground}
package main

import "fmt"

type rating struct {
	student string
	mark    int
}

type allRating struct {
	raitings []rating
}

/*
The average gets the average value from all the ratings.
If r is nil then the average returns 0.
*/
func (r *allRating) average() (res float64) {
	if r == nil {
		return
	}
	for _, raiting := range r.raitings {
		res += float64(raiting.mark) / float64(len(r.raitings))
	}
	return
}

func main() {
	var a *allRating
	fmt.Println(a.average())
}
```
```
0
```

В коде представлены структуры книги `book` и библиотеки `library`, состоящей из книг. Реализуйте метод `getAuthorBooks`  с указателем на `library` в качестве получателя. Метод принимает аргумент `author` типа `string` и должен вернуть срез из структур `book`. Срез должен содержать все книги автора `author`. {.task_text}

Учтите, что получатель может иметь значение `nil`. В этом случае верните пустой срез. {.task_text}

``` go {.task_source #golang_chapter_0120_task_0030}
package main

import "fmt"

type book struct {
	author, name string
	pages        int
	year         int
}

type library struct {
	books []book
}

func main() {
	var lib library
	lib.books = append(lib.books, book{"J. R. R. Tolkien", "The Hobbit, or There and Back Again", 310, 1937})
	lib.books = append(lib.books, book{"J. R. R. Tolkien", "The Lord of the Rings", 1077, 1954})
	lib.books = append(lib.books, book{"George R. R. Martin", "A Game of Thrones", 694, 1996})
	for _, book := range lib.getAuthorBooks("J. R. R. Tolkien") {
		fmt.Println(book.name)
	}
	var nilLib *library
	fmt.Println(nilLib.getAuthorBooks("George R. R. Martin"))
}
```

Если получатель равен `nil`, сразу же выполните `return`.  В противном случае пройдите в цикле по всем книгам библиотеки. Если автор совпадает, добавьте книгу в результирующий срез. {.task_hint}

```go {.task_answer}
package main

import "fmt"

type book struct {
	author, name string
	pages        int
	year         int
}

type library struct {
	books []book
}

func (lib *library) getAuthorBooks(
	author string) (res []book) {
	if lib == nil {
		return
	}
	for _, book := range lib.books {
		if book.author == author {
			res = append(res, book)
		}
	}
	return res
}

func main() {
	var lib library
	lib.books = append(lib.books, book{"J. R. R. Tolkien", "The Hobbit, or There and Back Again", 310, 1937})
	lib.books = append(lib.books, book{"J. R. R. Tolkien", "The Lord of the Rings", 1077, 1954})
	lib.books = append(lib.books, book{"George R. R. Martin", "A Game of Thrones", 694, 1996})
	for _, book := range lib.getAuthorBooks("J. R. R. Tolkien") {
		fmt.Println(book.name)
	}
	var nilLib *library
	fmt.Println(nilLib.getAuthorBooks("George R. R. Martin"))
}
```

## Композиция 

С помощью встраивания структур реализуют *композицию*. Композиция — это прием, который позволяет объединить несколько полей, каждое из которых содержит множество методов. Следующий пример иллюстрирует этот прием. Для этого используется анонимное встраивание структуры `book` в `textbook`. Заметим, что метод `print` определен для структуры `book`, однако может вызываться и для переменных структуры `textbook`:

``` go {.example_for_playground}
package main

import "fmt"

type book struct {
	author, name string
	pages        int
	year         int
}

func (b book) print() {
	fmt.Printf("%s. %s. %d p., %d year\n",
		b.author, b.name, b.pages, b.year)
}

type textbook struct {
	subject string
	book
}

func main() {
	var t textbook = textbook{"Go",
	 	book{"Alan A. A. Donovan", "The Go Programming Language", 380, 2015}}
	t.print()
}
```
```
Alan A. A. Donovan. The Go Programming Language. 380 p., 2015 year
```

В `textbook` может быть содеражть любое количество анонимных полей. Создайте еще одну структуру — `library`, которая содержит поля `name` и `address` типа `string`. Реализуйте метод этой структуры `printAddr`, который напечатает `name` и `address` через пробел. Встройте `library` в `textbook` как еще одно анонимное поле. Теперь переменные структуры `textbook` также содержат метод `printAddr`!

```go {.task_answer}
package main

import "fmt"

type book struct {
	author, name string
	pages        int
	year         int
}

type library struct {
	name    string
	address string
}

func (lib library) printAdr() {
	fmt.Printf("%s %s\n", lib.name, lib.address)
}

func (b book) print() {
	fmt.Printf("%s. %s. %d p., %d year\n",
		b.author, b.name, b.pages, b.year)
}

type textbook struct {
	subject string
	book
	library
}

func main() {
	var t textbook = textbook{"Go",
		book{"Alan A. A. Donovan", "The Go Programming Language", 380, 2015},
		library{"Moscow State Library", "Moscow city"}}
	t.print()
	t.printAdr()
}
```