# Глава 11. Структуры
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

Когда  нужно задать лишь некоторые поля, то перечисляют их имена:

```go
var mage = hero{level: 32, name: "Kate"}
```

Такой подход позволяет также не заботиться о порядке инициализируемых значений, даже если вы задаете их все, однако он более многословен.

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

Создайте новую переменную и присвойте ей значение `beginNode`. Изменяя эту переменную в цикле на адрес следующего элемента, напечатайте все значения списка через `->`, пока значение этой переменной не станет равным `nil`. Не забудьте в конце также напечатать `nil`. {.task_hint}

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
