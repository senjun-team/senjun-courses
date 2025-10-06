# Глава 14. Некоторые приемы по работе с интерфейсами
## Сравнение интерфейсов
Интерфейсы сравнимы, если сравнимы их конкретные типы: 
```go {.example_for_playground}
package main

import "fmt"

func main() {
	var a interface{} = 10
	var b interface{} = 20
	fmt.Println(a == b)
}
``` 
```
false
```

Что выведет следующий код? В случае ошибки напишите `error`. {.task_text}

```go {.example_for_playground}
package main

import (
	"fmt"
)

type musicTrack struct {
	id     int
	name   string
	author string
	albums []int
}

func (m musicTrack) String() string {
	return fmt.Sprintf("%s: %s", m.author, m.name)
}

func main() {
	var m fmt.Stringer = musicTrack{10,
		"Come Together",
		"The Beatles",
		[]int{1, 2, 3},
	}
	var m2 fmt.Stringer = musicTrack{10,
		"Come Together",
		"The Beatles",
		[]int{2, 3},
	}
	fmt.Println(m == m2)
}
```

```consoleoutput {.task_source #golang_chapter_0140_task_0010}
```
Вспомните, как сравниваются срезы. {.task_hint}
```go {.task_answer}
error
```

## Сортировка
На практике нам часто приходится сталкиваться с тем, чтобы отсортитровать какие-либо данные. Например, при выдаче списка файлов в папке мы можем отсортировать его по имени, дате или размеру файла. В Go для того чтобы организовать сортировку данных, удобно использовать `sort.Interface`.

Функция `Sort` пакета `sort` умеет сортировать переменные типа `sort.Interface`. Чтобы тип удовлетворял `sort.Interface`, необходимо реализовать три метода:

```go
type Interface interface {
	// Len возвращает количество элементов коллекции.
	Len() int

	// Less сообщает, должен ли элемент с индексом i сортироваться
    // перед элементом с индесом j
	Less(i, j int) bool

	// Swap меняет местами элементы с индексами i и j
	Swap(i, j int)
}
```
В качестве простого примера можно привести сортировку среза целых чисел: 

```go {.example_for_playground}
package main

import (
	"fmt"
	"sort"
)

type intSlice []int

func (p intSlice) Len() int           { return len(p) }
func (p intSlice) Less(i, j int) bool { return p[i] < p[j] }
func (p intSlice) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

func main() {
    s := intSlice{100, 80, 200, 3756, 0}
	fmt.Println(s)
	sort.Sort(s)
	fmt.Println(s)
}
```
```
[100 80 200 3756 0]
[0 80 100 200 3756]
```

Чтобы отсортировать срез в обратном порядке, необходимо поступить следующим образом: 
```go {.example_for_playground}
package main

import (
	"fmt"
	"sort"
)

type intSlice []int

func (p intSlice) Len() int           { return len(p) }
func (p intSlice) Less(i, j int) bool { return p[i] < p[j] }
func (p intSlice) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

func main() {
    s := intSlice{100, 80, 200, 3756, 0}
	fmt.Println(s)
	sort.Sort(sort.Reverse(s))
	fmt.Println(s)
}
```
```
[3756 200 100 80 0]
```

Функция `sort.Reverse` возвращает тип `sort.Interface`, в котором функция `Less` вызывается с индексами в обратном порядке. Вот как это достигается в исходном коде: 

```go
type reverse struct {
	// This embedded Interface permits Reverse to use the methods of
	// another Interface implementation.
	Interface
}

// Less returns the opposite of the embedded implementation's Less method.
func (r reverse) Less(i, j int) bool {
	return r.Interface.Less(j, i)
}

// Reverse returns the reverse order for data.
func Reverse(data Interface) Interface {
	return &reverse{data}
}
```

Память `memory` выделяется частями `chunk` по 16 значений типа `int`. Вся выделенная память представляется в виде односвязного списка `linkedList`, в котором каждое значение представлено в виде `memory`. Каждый чанк имеет `id`. Отсортируйте выделенную память по возрастанию `id` чанков.  {.task_text}


```go {.task_source #golang_chapter_0140_task_0020}
package main

import (
	"fmt"
	"sort"
)

type memory struct {
	id    int
	chunk [16]int
}

type linkedNode struct {
	next  *linkedNode
	value memory
}

type linkedList struct {
	begin *linkedNode
}

func (list *linkedList) print() {
	node := list.begin
	for node != nil {
		fmt.Print(node.value)
		fmt.Print("->")
		node = node.next
	}
	fmt.Print("nil")
}

func exampleList() *linkedList {
	mem0 := memory{2, [16]int{1, 2, 3}}
	mem1 := memory{3, [16]int{0, 0, 4, 5, 6}}
	mem2 := memory{1, [16]int{7, 0, 8}}
	mem3 := memory{0, [16]int{9, 10}}
	return &linkedList{&linkedNode{
		&linkedNode{
			&linkedNode{
				&linkedNode{nil,
					mem3}, mem2}, mem1}, mem0}}
}

func main() {
	lst := exampleList()
	lst.print()
	fmt.Println()
	sort.Sort(lst)
	lst.print()
	fmt.Println()
}

```

Чтобы отсортировать односвязный список, достаточно изменить адреса соответствующих указателей. {.task_hint}

```go  {.task_answer}
package main

import (
	"fmt"
	"sort"
)

const errNilNode = "one of the nodes is nil"

type memory struct {
	id    int
	chunk [16]int
}

type linkedNode struct {
	next  *linkedNode
	value memory
}

type linkedList struct {
	begin *linkedNode
}

func (list *linkedList) Len() int {
	n := list.begin
	counter := 0

	for n != nil {
		counter++
		n = n.next
	}
	return counter
}

func (list *linkedList) Less(i, j int) bool {
	iNode := list.numberNode(i)

	if iNode == nil {
		panic(errNilNode)
	}
	jNode := list.numberNode(j)
	if jNode == nil {
		panic(errNilNode)
	}
	return iNode.value.id < jNode.value.id
}

func (list *linkedList) Swap(i, j int) {
	iNode := list.numberNode(i)
	
	if iNode == nil {
		panic(errNilNode)
	}

	jNode := list.numberNode(j)
	if jNode == nil {
		panic(errNilNode)
	}

	var iPrev *linkedNode = nil
	var jPrev *linkedNode = nil

	if i-1 >= 0 {
		iPrev = list.numberNode(i - 1)
	}
	if j-1 >= 0 {
		jPrev = list.numberNode(j - 1)
	}

	if iPrev != nil {
		iPrev.next = jNode
	}
	if jPrev != nil {
		jPrev.next = iNode
	}
	iNode.next, jNode.next = jNode.next, iNode.next

	if i == 0 {
		list.begin = jNode
	}
	if j == 0 {
		list.begin = iNode
	}
}

func (list *linkedList) numberNode(number int) *linkedNode {
	if list.begin == nil || number < 0 {
		return nil
	}

	inode := list.begin

	for k := 0; k < number; k++ {
		inode = inode.next
		if inode == nil {
			return nil
		}
	}
	return inode
}

func (list *linkedList) print() {
	node := list.begin
	for node != nil {
		fmt.Print(node.value)
		fmt.Print("->")
		node = node.next
	}
	fmt.Print("nil")
}

func exampleList() *linkedList {
	mem0 := memory{2, [16]int{1, 2, 3}}
	mem1 := memory{3, [16]int{0, 0, 4, 5, 6}}
	mem2 := memory{1, [16]int{7, 0, 8}}
	mem3 := memory{0, [16]int{9, 10}}
	return &linkedList{&linkedNode{
		&linkedNode{
			&linkedNode{
				&linkedNode{nil,
					mem3}, mem2}, mem1}, mem0}}
}

func main() {
	lst := exampleList()
	lst.print()
	fmt.Println()
	sort.Sort(lst)
	lst.print()
	fmt.Println()
}
```
