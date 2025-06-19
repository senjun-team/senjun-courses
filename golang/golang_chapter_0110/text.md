# Глава 11. Структуры

## Понятие структуры

Структура — это тип данных, который объединяет собой другие типы данных:

```go
type hero struct{
	name string 
	age uint
	level uint 
	timeOfSpawn time.Time
	location [2]int 
}
```
 
Чтобы создать переменную типа `hero` и присвоить ей значение, поступают следующим образом:

```go 
var mage hero = hero{"Kate", 150, 32, time.Now(), [2]int{15, 76}}
```

При таком синтаксисе важно соблюдать порядок инициализируемых значений.  

Иногда используют и пустые скобки:

```go
var a []hero
a = append(a, hero{})
```

В результате в срез из переменных типа `hero` будет добавлена структура с нулевыми значениями полей.

## Приемы по работе со структурами

Когда  нужно задать лишь некоторые поля, то перечисляют их имена:

```go
var mage = hero{level: 32, name: "Kate"}
```

Такой подход позволяет также не заботиться о порядке инициализируемых значений, однако он более многословен.

Доступ к каждому полю структуры выполняется через точку:

```go
mage.location[1] = 32
```

Чтобы сократить запись, имена полей одного типа объединяют:

```go
type book struct {
	id           uint
	name, author string
	pages        uint
}
```

*Совет*: старайтесь объединять такие имена полей, которые логически связаны между собой.

Структуры могут быть вложенными:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	var config configuration
	config.name = "test"
	config.settings.address = "192.168.23.48"
	config.settings.port = "4040"
	fmt.Println(config)
}

type configuration struct {
	name     string
	settings proxy
}

type proxy struct {
	address string
	port    string
}
```
```
{test {192.168.23.48 4040}}
```

Такой прием называется *встраиванием структур*.

Чтобы не писать лишнюю сущность `settings` при обращении к `config`, используют анонимные поля:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	var config configuration
	config.name = "test"
	config.address = "192.168.23.48"
	config.port = "4040"
	fmt.Println(config)
}

type configuration struct {
	name string
	proxy
}

type proxy struct {
	address string
	port    string
}
```
```
{test {192.168.23.48 4040}}
```

## Области видимости

Как вы [уже знаете,](/courses/golang/chapters/golang_chapter_0010/#block-visibility) имена, которые начинаются с прописных букв, доступны из пакетов, которые их импортируют. В противном случае они доступны только внутри своего пакета. Это актуально как для самой структуры, так и для полей внутри нее. Например, поле `level` такой структуры доступно только внутри пакета `heroes`:

```go
package heroes

import "time"

type Hero struct {
	Name        string
	Age         uint
	level       uint
	TimeOfSpawn time.Time
	Location    [2]int
}
```

Сама же структура `Hero`, как и все ее поля кроме `level`, доступна из других пакетов, которые ее импортируют. Так, получить доступ к `level` из пакета `main` невозможно.

*Совет*: начинайте со строчных букв все поля, которые возможно скрыть. Используйте прописные буквы только там, где это необходимо. Чем меньше знает о внутренней реализации внешний пакет, тем лучше. Такой прием в объектно-ориентированных языках называют инкапсуляция. Инкапсуляция помогает делать отдельные части программы менее связанными. Программа получается более масштабируемой и менее подверженной ошибкам со стороны программиста.

## Передача структуры в качестве параметра функции

Структуры можно передавать как параметры в функцию. Когда структура большая, то для повышения эффективности используют указатель на структуру. Кроме того, если функция модифицирует параметр-структуру, то он обязательно передается через указатель:

```go 
func main() {
	var mage hero = hero{"Kate", 150, 32, time.Now(), [2]int{15, 76}}
	kill(&mage)
}

func kill(h *hero) {
	h.level = 0
}

type hero struct {
	name        string
	age         uint
	level       uint
	timeOfSpawn time.Time
	location    [2]int
}
```

Следующий код реализует структуру данных — [односвязный список.](https://ru.wikipedia.org/wiki/%D0%A1%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA#%D0%9E%D0%B4%D0%BD%D0%BE%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_(%D0%BE%D0%B4%D0%BD%D0%BE%D0%BD%D0%B0%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9_%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA)) Он представляет собой список из элементов, каждый из которых содержит два поля: значение и указатель на следующий элемент. Такой элемент представлен в коде структурой `linkedNode`. {.task_text}

Функция `newNode` добавляет новый элемент к последнему, адрес которого передается ей в параметре `node`, и возвращает адрес вновь созданного элемента. Функция `newList` создает новый односвязный список размера `nodeNumber` и возвращает адрес начального элемента. {.task_text}

Реализуйте тело функции `printList`, которая принимает на вход адрес первого элемента и выводит односвязный список на экран.  {.task_text}

Например, для односвязного списка размером `5` код должен вывести: `0->5->10->15->20->nil`. В случае `nodeNumber <= 0` функция `printList` должна вывести `nil`. {.task_text}

```go {.task_source #golang_chapter_0110_task_0010}
package main

import "fmt"

type linkedNode struct {
	next  *linkedNode
	value int
}

func printList(beginNode *linkedNode) {
	// ваш код здесь 
}
```

Создайте новую переменную и присвойте ей значение `beginNode`. Изменяя эту переменную в цикле на адрес следующего элемента, напечатайте все значения списка через стрелочку `->`, пока значение этой переменной не станет равным `nil`. Не забудьте в конце также напечатать `nil` как строку. {.task_hint}

```go {.task_answer}
package main

import "fmt"

type linkedNode struct {
	next  *linkedNode
	value int
}

func printList(beginNode *linkedNode) {
	node := beginNode
	for node != nil {
		fmt.Print(node.value)
		fmt.Print("->")
		node = node.next
	}
	fmt.Print("nil")
}
```

## Сравнение структур

Структуры одного типа можно сравнивать, если все их поля сравнимы друг с другом. Две такие структуры равны, если их поля совпадают:

```go {.example_for_playground}
package main

import ("fmt"
        "time"
)

func main() {
	timeOfSpawn := time.Now()
	var mage hero = hero{"Kate", 150, 32, timeOfSpawn, [2]int{15, 76}}
	var necromancer hero = hero{"Kate", 150, 32, timeOfSpawn, [2]int{15, 76}}
	var barbarian hero = hero{"Kate", 40, 32, timeOfSpawn, [2]int{15, 76}}
	fmt.Println(mage == necromancer)
	fmt.Println(mage == barbarian)
}

type hero struct {
	name        string
	age         uint
	level       uint
	timeOfSpawn time.Time
	location    [2]int
}
```
```
true
false
```

Попытка сравнить структуры с полями, которые не сравнимы друг с другом, приведет к ошибке компиляции:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	var gameOfThrones book = book{0, "A Game of Thrones",
		694, make(map[int]string)}
	var gameOfThronesDeluxEdition book = book{0, "A Game of Thrones",
		694, make(map[int]string)}
	fmt.Println(gameOfThrones == gameOfThronesDeluxEdition)
}

type book struct {
	id           uint
	name         string
	pages        uint
	wordsPerPage map[int]string
}
```
```
./main.go:10:14: invalid operation: gameOfThrones == gameOfThronesDeluxEdition (struct containing map[int]string cannot be compared) (exit status 1)
```

## Псевдонимы типов данных

Для выразительности и дополнительного контроля можно объявить псевдоним (alias) для типов данных:
```go
type userId uint
type ip string
``` 

Вновь созданные переменные таких типов также проверяются на соответствие типов. Следующий код вызовет ошибку компиляции:

```go
type userId uint
var mainId userId = 20
var computerId uint = 19

fmt.Println(mainId == computerId)
```

```
./main.go:10:24: invalid operation: mainId == computerId (mismatched types userId and uint) (exit status 1)
```

Язык Go — язык со строгой типизацией. Необходимо явное приведение:

```go
type userId uint
var mainId userId = 20
var computerId uint = 19

fmt.Println(mainId == userId(computerId))
```

```
false
```

Абстрактное синтаксическое дерево — это дерево, в котором внутренние узлы сопоставлены операторам языка, а листья — его операндам. Сокращенно — AST, от английского: abstract syntax tree. Каждый узел такого дерева содержит так называемую лексему. {.task_text}

Следующий код реализует AST. Структура `ast` состоит из единственного поля — указателя на корневой узел `node`. Структура `node` состоит из трех полей: указателя на родительский узел, среза указателей из дочерних узлов и самого значения `value`. Функция `printAst` печатает AST на экран. Функция `randromTree` генерирует AST для случайного арифметического выражения. Она принимает в качестве параметров `seed`, на основе которого считаются случайные числа, и размер дерева. Зафиксировав `seed`, можно получать одинаковые деревья для одних и тех же параметров. {.task_text}

Арифметическое выражение может состоять из целых чисел, скобок и трех операций: `+`, `-`, `*`.Реализуйте функцию `solve`, которая считает результат арифметического выражения по его AST. На вход функция `solve` принимает указатель на корень дерева. Вы можете также писать код за пределами функции `solve`. {.task_text}

Например, AST для арифметического выражения `(5+4)*(10-8)` выглядит так: {.task_text}

```
*
├─ +
│  ├─ 5
│  └─ 4
└─ -
    ├─ 10
    └─ 8
```

```go {.task_source #golang_chapter_0110_task_0020}
package main

import (
	"fmt"
	"math/rand"
	"strconv"
)

type ast struct {
	root *node
}

type node struct {
	parent   *node
	children []*node
	value    lexeme
}

type lexeme string

func solve(n *node) lexeme {
	 // ваш код здесь
}

// ваш код также здесь
```

Создайте вспомогательную функцию `compute`. Она получает на вход три лексемы: операцию, левый операнд и правый операнд. Функция считает результат и возвращает его в качестве лексемы. Воспользуйтесь этой функцией для рекурсивной реализации `solve`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math/rand"
	"strconv"
)

type ast struct {
	root *node
}

type node struct {
	parent   *node
	children []*node
	value    lexeme
}

type lexeme string

func solve(n *node) lexeme {
	if len(n.children) == 0 {
		return n.value
	}

	return compute(n.value, solve(n.children[0]), solve(n.children[1]))
}

func compute(op lexeme,
	leftLexeme lexeme, rightLexeme lexeme) lexeme {
	left, _ := strconv.Atoi(string(leftLexeme))
	right, _ := strconv.Atoi(string(rightLexeme))

	switch op {
	case "+":
		return lexeme(strconv.Itoa(left + right))
	case "-":
		return lexeme(strconv.Itoa(left - right))
	case "*":
		return lexeme(strconv.Itoa(left * right))
	}

	panic("no such operation")
}
```

## Резюме

1. Структура представляет собой пользовательский тип данных. Структура удобна, когда необходимо представить одну сущность из нескольких полей разных типов как единой целое.
2. Одна структура может быть частью другой структуры. Это встраивание структур.
3. Как структура, так и поля внутри нее, ограничены областью видимости. Имена, которые начинаются с прописных букв, видны из пакетов, которые их импортируют. Имена, которые начинаются со строчных букв, доступны только внутри пакета.  
4. Полем структуры может быть даже указатель на саму эту структуру! Это позволяет легко реализовать рекурсивные структуры данных. Например, списки и деревья. 
5. Полезным свойством структур является то, что их можно сравнивать, если сравнимы соответствующие поля. Вместо того, чтобы писать цикл по обходу полей структуры, достаточно проверить их на равенство.
6. Для типов данных допустимо использовать псевдоним. Это делает код более выразительным и менее подверженным ошибкам. Разные пседовнимы, образованные от одного типа, являются разными типами. 
