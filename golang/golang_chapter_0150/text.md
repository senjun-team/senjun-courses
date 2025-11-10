# Глава 15. Дженерики
Начиная с Go 1.18, в языке появились дженерики. Дженерики позволяют работать с различными типами данных, на меняя структуру исходного кода. 

Дженерики легче понять на примере. Допустим, мы реализовали некоторый алгоритм сортировки: 

``` go {.example_for_playground}
package main

import "fmt"

func bubbleSort(s []int) {
	for i := 0; i < len(s)-1; i++ {
		for j := 0; j < len(s)-i-1; j++ {
			if s[j] > s[j+1] {
				s[j], s[j+1] = s[j+1], s[j]
			}
		}
	}
}

func main() {
	input := []int{40, 10, 30, 100, 80}
	bubbleSort(input)
	fmt.Println(input)
}
```
```
[10 30 40 80 100]
```

Что делать, если мы хотим сортировать не только `int`, но и другие сравнимые типы? Например, `float` или даже `string`. Мы могли бы воспользоваться интерфейсом `any`, чтобы передать любой тип в функцию. Но как понять, что элементы этого типа сравнимы?

Интерфейсы в Go являются либо **базовыми** (**basic**), либо нет (**non-basic**). До сих пор мы имели дело с базовыми интерфейсами. Это такие интерфейсы, которые определены через список методов. Non-basic интерфейсы могут включать себя не только набор методов, но и конкретные типы. Например, интерфейс `cmp.Ordered` определяется через все типы, которые могут быть упорядочены. Он является non-basic. Non-basic интерфейсы не могут быть использованы как обычные типы. Их применяют совместно в дженериках. Для нашего примера как раз подойдет такой интерфейс: 

``` go {.example_for_playground}
package main

import (
	"cmp"
	"fmt"
)

func bubbleSort[T cmp.Ordered](s []T) {
	for i := 0; i < len(s)-1; i++ {
		for j := 0; j < len(s)-i-1; j++ {
			if s[j] > s[j+1] {
				s[j], s[j+1] = s[j+1], s[j]
			}
		}
	}
}

func main() {
	input := []int{40, 10, 30, 100, 80}
	bubbleSort(input)
	fmt.Println(input)
	anotherInput := []float64{50.4, 10, 30.99, 30.8}
	bubbleSort(anotherInput)
	fmt.Println(anotherInput)
	amazingInput := []string{"bubble", "orange", "apple"}
	bubbleSort(amazingInput)
	fmt.Println(amazingInput)
}
```
```
[10 30 40 80 100]
[10 30.8 30.99 50.4]
[apple bubble orange]
```
Это и есть дженерик. В качестве типа `T` мы использовали некоторый обощенный тип `cmp.Ordered`. Теперь мы можем сортировать данные всех типов, которые могут быть упорядочены.

Что выведет следующий код? В случае ошибки напишите error. {.task_text}

```go {.example_for_playground}
package main

import (
	"cmp"
	"fmt"
)

func bubbleSort[T cmp.Ordered](s []T) {
	for i := 0; i < len(s)-1; i++ {
		for j := 0; j < len(s)-i-1; j++ {
			if s[j] > s[j+1] {
				s[j], s[j+1] = s[j+1], s[j]
			}
		}
	}
}

func main() {
	input := []any{1, "bla", 2}
	bubbleSort(input)
	fmt.Println(input)
}
```

```consoleoutput {.task_source #golang_chapter_0150_task_0010}
```
Тип соответствует `cmp.Ordered`, если он может быть упорядочен. {.task_hint}
```{.task_answer}
error
```
