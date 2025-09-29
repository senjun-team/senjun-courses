# Глава 14. Некоторые приемы по работе с интерфейсами

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
