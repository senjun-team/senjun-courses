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
 
Создать переменную типа `hero` и присвоить ей значение:

```go 
var mage hero = hero{"Kate", 150, 32, time.Now(), [2]int{15, 76}}
```

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
type hero struct {
	name        string
	age, level  uint
	timeOfSpawn time.Time
	location    [2]int
}
```

*Совет*: старайтесь объединять такие имена полей, которые логически связаны между собой.

Полем для одной структуры может быть другая структура:

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

Такой прием называется встраиванием структур. Чтобы не писать лишнюю сущность `settings` при обращении к `config`, используют анонимные поля:

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

Сама же структура `Hero`, как и все ее поля, кроме `level`, доступна из других пакетов, которые ее импортируют. Получить доступ к `level`, например, из пакета `main` невозможно.

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

Следующий код реализует структуру данных — односвязный список. Односвязный список представляет собой список из элементов, каждый из которых содержит два поля: значение и указатель на следующий элемент. Такой элемент представлен в коде структурой `linkedNode`. Функция `newNode` добавляет новый элемент к последнему, адрес которого передается ей в параметре `node`, и возвращает адрес вновь созданного элемента. Функция `newList` создает новый односвязный список размера `nodeNumber` и возвращает адрес начального элемента. Реализуйте тело функции `printList`, которая принимает на вход адрес первого элемента и выводит односвязный список на экран. Например, для односвязного размером `5` код должен вывести: `0->5->10->15->20->nil` {.task_text}

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

Создайте новую переменную и присвойте ей значение `beginNode`. Изменяя эту переменную в цикле на адрес следующего элемента, напечатайте все значения списка через стрелочку `->`, пока значение этой переменной не станет равным `nil`. Не забудьте в конце также напечатать `nil`. {.task_hint}

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
