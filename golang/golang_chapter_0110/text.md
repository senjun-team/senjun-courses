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

Такой прием называется *встраиванием структур*. Чтобы не писать лишнюю сущность `settings` при обращении к `config`, используют анонимные поля:

```go {.example_for_playground}
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

Напомним, имена, которые начинаются с прописных букв, доступны из пакетов, которые их импортируют. В противном случае они доступны только внутри своего пакета. Это актуально как для самой структуры, так и для полей внутри нее. Например, поле `level` такой структуры доступно только внутри пакета `heroes`:

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

Сама же структура `Hero`, как и все ее поля, кроме `level`, доступны из других пакетов, которые ее импортируют. Получить доступ к `level`, например, из пакета `main` невозможно.

*Совет*: начинайте со строчных букв все поля, которые возможно скрыть. Используйте прописные буквы только там, где это необходимо. Чем меньше знает о внутренней реализации внешний пакет, тем лучше. Такой прием в объектно-ориентированных языках называют инкапсуляция. Инкапсуляция помогает делать отдельные части программы менее связанными. Программа получается более масштабируемой и менее подверженной ошибкам со стороны программиста.

## Передача структуры функции в качестве параметра

Структуры можно передавать как параметры в функцию. Когда структура большая, то для повышения эффективности используют указатель на структуру. Кроме того, если функция редактирует параметр-структуру, то она обязательно передается через указатель:

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

Следующий код реализует структуру данных — односвязный список. Односвязный список представляет собой список из элементов, каждый из которых содержит два поля: значение и указатель на следующий элемент. Такой элемент представлен в коде структурой `linkedNode`. Функция `newNode` добавляет новый элемент к последнему, адрес которого передается ей в параметре `node`, и возвращает адрес вновь созданного элемента. Функция `newList` создает новый односвязный список размера `nodeNumber` и возвращает адрес начального элемента. Реализуйте тело функции `printList`, которая принимает на вход адрес первого элемента и выводит односвязный список на экран. Например, для односвязного размером `5` код должен вывести: `0->5->10->15->20->nil`. В случае `nodeNumber <= 0` функция `printList` должна вывести `nil`. {.task_text}

```go {.task_source #golang_chapter_0110_task_0010}
package main

import "fmt"

type linkedNode struct {
	next  *linkedNode
	value int
}

func newNode(node *linkedNode, value int) *linkedNode {
	node.next = &linkedNode{nil, value}
	return node.next
}

func newList(nodeNumber int) *linkedNode {
	if nodeNumber <= 0 {
		return nil
	}
	var beginNode = &linkedNode{nil, 0}
	node := beginNode

	for i := 1; i < nodeNumber; i++ {
		node = newNode(node, i*5)
	}
	return beginNode
}

func printList(beginNode *linkedNode) {
	// ваш код здесь 
}

func main() {
	beginNode := newList(5)
	printList(beginNode)
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

func newNode(node *linkedNode, value int) *linkedNode {
	node.next = &linkedNode{nil, value}
	return node.next
}

func newList(nodeNumber int) *linkedNode {
	var beginNode = &linkedNode{nil, 0}
	node := beginNode

	for i := 1; i < nodeNumber; i++ {
		node = newNode(node, i*5)
	}
	return beginNode
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

func main() {
	beginNode := newList(5)
	printList(beginNode)
}
```

## Сравнение структур
Структуры одного типа можно сравнивать, если все поля структуры сравнимы друг с другом. Две такие структуры равны, если их поля совпадают:

```go {.example_for_playground}
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

## Псевдонимы существующих типов данных
Для выразительности и дополнительного контроля можно объявить псевдоним *alias* для существующих типов данных:
```go
type userId uint
type ip string
``` 

Вновь созданные переменные таких типов также проверяются на соответствие типов. Следующий код вызовет ошибку компиляции:

```go {.example_for_playground}
type userId uint
var bubaId userId = 20
var valueId uint = 19

fmt.Println(bubaId == valueId)
```

```
./main.go:10:24: invalid operation: bubaId == valueId (mismatched types userId and uint) (exit status 1)
```

Язык Go — язык со строгой типизацией. Необходимо явное приведение:

```go {.example_for_playground}
type userId uint
var bubaId userId = 20
var valueId uint = 19

fmt.Println(bubaId == userId(valueId))
```

```
false
```

Абстрактное синтаксическое дерево — это дерево, в котором внутренние узлы сопоставлены операторам языка, а листья — его операндам. Сокращенно — AST, от английского: abstract syntax tree. Например, для арифметического выражения `(5+4)*(10-8)` можно построить следующее AST: {.task_text}

```
*
├─ +
│  ├─ 5
│  └─ 4
└─ -
    ├─ 10
    └─ 8
```
Каждый узел такого дерева содержит так называемую лексему. {.task_text}

Следующий код реализует AST. Структура `ast` состоит из единственного поля — указателя на корневой узел `node`. Структура `node` состоит из трех полей: указателя на родительский узел, среза указателей из дочерних узлов и самого значения `value`. Функция `printAst` печатает AST на экран. Функция `randromTree` генерирует AST для случайного арифметического выражения. Она принимает в качестве параметров `seed`, на основе которого считаются случайные числа, и размер дерева. Зафиксировав `seed`, можно получать одинаковые деревья для одних и тех же параметров. Арифметическое выражение может состоять из целых чисел, скобок и трех операций: `+`, `-`, `*`.Реализуйте функцию `solve`, которая считает результат арифметического выражения по его AST. На вход функция `solve` принимает указатель на корень дерева. Вы можете также писать код за пределами функции `solve`.  {.task_text}

```go {.task_source #golang_chapter_0110_task_0020}
package main

import (
	"fmt"
	"math/rand"
	"strconv"
)

func main() {
	tree := randomTree(42, 5)
	printAst(tree)
}

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

func printAst(a *ast) {
	printInLevel(a.root, 0, map[int]struct{}{})
}

func printInLevel(n *node, level int, prevBranchLevels map[int]struct{}) (branchLevels map[int]struct{}) {
	branchLevels = make(map[int]struct{})

	for i := 0; i < level; i++ {
		_, ok := prevBranchLevels[i]
		if ok {
			fmt.Print("│  ")
			branchLevels[i] = struct{}{}
		} else if i != 0 {
			fmt.Print("   ")
		}
	}

	if level != 0 {
		if n.parent == nil || n.parent.children[len(n.parent.children)-1] == n {
			fmt.Print("└─ ")
		} else {
			fmt.Print("├─ ")
			branchLevels[level] = struct{}{}
		}
	}

	fmt.Println(n.value)

	for _, child := range n.children {
		branchLevels = printInLevel(child, level+1, branchLevels)
	}

	return
}
func randomTree(seed int64, size int) *ast {
	rnd := rand.New(rand.NewSource(seed))
	var leaf node
	var a ast
	a.root = &leaf
	a.root.value = randomOp(rnd)
	var nonTerminalNodes []*node
	nonTerminalNodes = append(nonTerminalNodes, a.root)

	for i := 0; i < size; i++ {
		n := nonTerminalNodes[i]
		left, right := extendTree(rnd, n)
		if left != nil {
			nonTerminalNodes = append(nonTerminalNodes, left)
		}
		if right != nil {
			nonTerminalNodes = append(nonTerminalNodes, right)
		}
	}

	for i := size; i < len(nonTerminalNodes); i++ {
		nonTerminalNodes[i].children = append(nonTerminalNodes[i].children, &node{})
		nonTerminalNodes[i].children = append(nonTerminalNodes[i].children, &node{})
		nonTerminalNodes[i].children[0] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		nonTerminalNodes[i].children[1] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		nonTerminalNodes[i].children[0].parent = nonTerminalNodes[i]
		nonTerminalNodes[i].children[1].parent = nonTerminalNodes[i]
	}

	return &a
}

func extendTree(rnd *rand.Rand, n *node) (left *node, right *node) {
	n.children = append(n.children, &node{})
	n.children = append(n.children, &node{})
	switch rnd.Intn(3) {
	case 0:
		n.children[0] = &node{value: randomOp(rnd)}
		n.children[1] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		n.children[0].parent = n
		n.children[1].parent = n
		return n.children[0], nil
	case 1:
		n.children[0] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		n.children[1] = &node{value: randomOp(rnd)}
		n.children[0].parent = n
		n.children[1].parent = n
		return nil, n.children[1]
	case 2:
		n.children[0] = &node{value: randomOp(rnd)}
		n.children[1] = &node{value: randomOp(rnd)}
		n.children[0].parent = n
		n.children[1].parent = n
		return n.children[0], n.children[1]
	}
	panic("no such case for int value")
}

func randomOp(rnd *rand.Rand) lexeme {

	switch rnd.Intn(3) {
	case 0:
		return "+"
	case 1:
		return "-"
	case 2:
		return "*"
	}
	panic("no such case for int value")
}

```

Создайте еще одну функцию: `compute`. Функция `compute` должна получать на вход три лексемы: операцию, левый операнд и правый операнд. Она считает результат и возвращает его также в качестве лексемы. Воспользуйтесь этой функцией для реализации рекурсивной функции `solve`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math/rand"
	"strconv"
)

func main() {
	tree := randomTree(42, 5)
	printAst(tree)
}

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

func printAst(a *ast) {
	printInLevel(a.root, 0, map[int]struct{}{})
}

func printInLevel(n *node, level int, prevBranchLevels map[int]struct{}) (branchLevels map[int]struct{}) {
	branchLevels = make(map[int]struct{})

	for i := 0; i < level; i++ {
		_, ok := prevBranchLevels[i]
		if ok {
			fmt.Print("│  ")
			branchLevels[i] = struct{}{}
		} else if i != 0 {
			fmt.Print("   ")
		}
	}

	if level != 0 {
		if n.parent == nil || n.parent.children[len(n.parent.children)-1] == n {
			fmt.Print("└─ ")
		} else {
			fmt.Print("├─ ")
			branchLevels[level] = struct{}{}
		}
	}

	fmt.Println(n.value)

	for _, child := range n.children {
		branchLevels = printInLevel(child, level+1, branchLevels)
	}

	return
}
func randomTree(seed int64, size int) *ast {
	rnd := rand.New(rand.NewSource(seed))
	var leaf node
	var a ast
	a.root = &leaf
	a.root.value = randomOp(rnd)
	var nonTerminalNodes []*node
	nonTerminalNodes = append(nonTerminalNodes, a.root)

	for i := 0; i < size; i++ {
		n := nonTerminalNodes[i]
		left, right := extendTree(rnd, n)
		if left != nil {
			nonTerminalNodes = append(nonTerminalNodes, left)
		}
		if right != nil {
			nonTerminalNodes = append(nonTerminalNodes, right)
		}
	}

	for i := size; i < len(nonTerminalNodes); i++ {
		nonTerminalNodes[i].children = append(nonTerminalNodes[i].children, &node{})
		nonTerminalNodes[i].children = append(nonTerminalNodes[i].children, &node{})
		nonTerminalNodes[i].children[0] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		nonTerminalNodes[i].children[1] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		nonTerminalNodes[i].children[0].parent = nonTerminalNodes[i]
		nonTerminalNodes[i].children[1].parent = nonTerminalNodes[i]
	}

	return &a
}

func extendTree(rnd *rand.Rand, n *node) (left *node, right *node) {
	n.children = append(n.children, &node{})
	n.children = append(n.children, &node{})
	switch rnd.Intn(3) {
	case 0:
		n.children[0] = &node{value: randomOp(rnd)}
		n.children[1] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		n.children[0].parent = n
		n.children[1].parent = n
		return n.children[0], nil
	case 1:
		n.children[0] = &node{value: lexeme(strconv.Itoa(rnd.Intn(20)))}
		n.children[1] = &node{value: randomOp(rnd)}
		n.children[0].parent = n
		n.children[1].parent = n
		return nil, n.children[1]
	case 2:
		n.children[0] = &node{value: randomOp(rnd)}
		n.children[1] = &node{value: randomOp(rnd)}
		n.children[0].parent = n
		n.children[1].parent = n
		return n.children[0], n.children[1]
	}
	panic("no such case for int value")
}

func randomOp(rnd *rand.Rand) lexeme {

	switch rnd.Intn(3) {
	case 0:
		return "+"
	case 1:
		return "-"
	case 2:
		return "*"
	}
	panic("no such case for int value")
}
```

## Резюме
1. Структура представляет собой пользовательский тип данных. Структура удобна, когда необходимо представить одну сущность из нескольких полей разных типов как единой целое.
2. Одна структура может быть частью другой структуры.
3. Как структура, так и поля внутри нее, ограничены областью видимости. Имена, которые начинаются с прописных букв, видны из пакетов, которые их импортируют. Имена, которые начинаются со строчных букв, доступны только внутри пакета.  
4. Полем структуры может быть даже указатель на саму эту структуру! Это позволяет легко реализовать многие структуры данных. Например, списки и деревья. 
5. Полезным свойством структур является то, что их можно сравнивать, если сравнимы соответствующие поля. Вместо того, чтобы писать цикл по обходу полей структуры, достаточно проверить их на равенство.
6. Для существующих типов данных допустимо использовать псевдоним. Это делает код более выразительным и менее подверженным ошибкам. Разные пседовнимы, образованные от одного типа, являются разными типами. 
