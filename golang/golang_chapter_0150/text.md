# Глава 15. Дженерики
## Дженерики-функции

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

Что делать, если мы хотим сортировать не только `int`, но и, например, `float64`? На помощь приходят дженерики: 

``` go {.example_for_playground}
package main

import "fmt"

func bubbleSort[T int | float64](s []T) {
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
}
```
```
[10 30 40 80 100]
[10 30.8 30.99 50.4]
```
В квадртных скобках после имени функции описывается новый тип, с которым мы будем работать. В нашем случае `T` — имя этого типа. Его вы даете сами, но на практике чаще всего используют букву `T`. Конкретные типы, которые может принимать `T`, перечисляются через вертикальную черту `|`. После того, как мы описали новый тип, он становится доступен всюду внутри функции.

Что делать, если мы хотим сортировать не только `int` и `float64`, но и другие типы, которые могут быть упорядочены? Например, `float32` или даже `string`. Мы могли бы воспользоваться интерфейсом `any`, чтобы передать любой тип в функцию. Но как понять, что элементы этого типа можно упорядочить? Мы могли бы перечислить все типы, с которыми мы можем работать явно, но это утомительно и громоздко. За нас это уже сделали в интерфейсе `cmp.Ordered`.

Интерфейсы в Go являются либо **базовыми** (**basic**), либо нет (**non-basic**). До сих пор мы имели дело с базовыми интерфейсами. Это такие интерфейсы, которые определены через список методов. Non-basic интерфейсы могут включать себя не только набор методов, но и конкретные типы. Иинтерфейс `cmp.Ordered` определяется через все типы, которые могут быть упорядочены. Он является non-basic. Non-basic интерфейсы не могут быть использованы как обычные типы. Их применяют совместно с дженериками. Для нашего примера как раз подойдет такой интерфейс: 

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
В качестве типа `T` мы использовали некоторый обощенный тип `cmp.Ordered`. Теперь мы можем сортировать данные всех типов, которые могут быть упорядочены.

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
## Дженерики-типы 

Когда в структуре есть поля, в которых необходимо писать данные различных типов, бывает удобно использовать дженерики-типы:
 
``` go {.example_for_playground}
package main

import "fmt"

type recordT[T int | float64] struct {
	data []T
	tag  string
}

func main() {
	sampleRecord := recordT[int]{[]int{1, 2, 3}, "sample data"}
	anotherRecord := recordT[float64]{[]float64{3.141592653589,
		2.718281828}, "consts"}

	fmt.Println(sampleRecord)
	fmt.Println(anotherRecord)
}
```
```
{[1 2 3] sample data}
{[3.141592653589 2.718281828] consts}
```

В этом случае при создании переменной типа `recordT` необходимо указывать тот конкретный тип, с которым мы работаем.

## Прием с тильдой

Рассмотрим следующий пример:

``` go {.example_for_playground}
package main

import (
	"fmt"
	"strings"
)

func wordCounter() func(word string) map[string]int {
	res := make(map[string]int)

	return func(word string) map[string]int {
		res[word]++
		return res
	}
}

func main() {
	wc := wordCounter()

	wordToFind := "яблоко"
	anotherWordToFind := "апельсин"
	s := "в корзине лежали яблоко, апельсин и еще одно яблоко"

	var res map[string]int
	for _, word := range strings.Split(s, " ") {
		// проверка вхождения wordToFind в word
		if strings.Contains(word, wordToFind) {
			res = wc(wordToFind)
		}
		if strings.Contains(word, anotherWordToFind) {
			res = wc(anotherWordToFind)
		}
	}

	fmt.Println(res)

}
```

``` go {.example_for_playground}
package main

import (
	"fmt"
	"strings"
)

type lexeme string

func wordCounter[T ~string]() func(word T) map[T]int {
	res := make(map[T]int)

	return func(word T) map[T]int {
		res[word]++
		return res
	}
}

func main() {
	wc := wordCounter[lexeme]()

	var wordToFind lexeme = "яблоко"
	var anotherWordToFind lexeme = "апельсин"
	s := "в корзине лежали яблоко, апельсин и еще одно яблоко"

	var res map[lexeme]int
	for _, word := range strings.Split(s, " ") {
		if strings.Contains(word, string(wordToFind)) {
			res = wc(wordToFind)
		}
		if strings.Contains(word, string(anotherWordToFind)) {
			res = wc(anotherWordToFind)
		}
	}

	fmt.Println(res)

}
```
```
map[апельсин:1 яблоко:2]
```