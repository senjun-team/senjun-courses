# Глава 15. Дженерики
## Дженерики-функции

Начиная с Go 1.18, в языке появились дженерики. Дженерики позволяют работать с различными типами данных, не меняя структуру исходного кода. 

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
В квадратных скобках после имени функции описывается новый тип, с которым мы будем работать. В нашем случае `T` — имя этого типа. Его вы даете сами, но на практике чаще всего используют букву `T`. Типы, которые может принимать `T`, перечисляются через вертикальную черту `|`. После того, как мы описали новый тип, он становится доступен всюду внутри функции.

Что делать, если мы хотим сортировать не только `int` и `float64`, но и другие типы, которые могут быть упорядочены? Например, `float32` или даже `string`. Мы могли бы воспользоваться интерфейсом `any`, чтобы передать любой тип в функцию. Но как понять, что элементы этого типа можно упорядочить? Мы могли бы перечислить все типы, с которыми мы можем работать явно, но это утомительно и громоздко. За нас это уже сделали в интерфейсе `cmp.Ordered`.

Интерфейсы в Go являются либо **базовыми** (**basic**), либо нет (**non-basic**). До сих пор мы имели дело с базовыми интерфейсами. Это такие интерфейсы, которые определены через список методов. Non-basic интерфейсы могут включать себя не только набор методов, но и типы. Интерфейс `cmp.Ordered` определяется через все типы, которые могут быть упорядочены. Он является non-basic. Non-basic интерфейсы не могут быть использованы как обычные типы. Их применяют совместно с дженериками. Для нашего примера как раз подойдет такой интерфейс: 

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
В качестве типа `T` мы использовали некоторый обобщенный тип `cmp.Ordered`. Теперь мы можем сортировать данные всех типов, которые могут быть упорядочены.

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

	lexemeToFind := "i"
	anotherLexemeToFind := "print"

	s := "for i := 0; i < 5; i++" +
		"{print(\"hello world!\n\"))}"

	var res map[string]int
	for _, word := range strings.Split(s, " ") {
		// проверка вхождения lexemeToFind в word
		if strings.Contains(word, lexemeToFind) {
			res = wc(lexemeToFind)
		}
		if strings.Contains(word, anotherLexemeToFind) {
			res = wc(anotherLexemeToFind)
		}
	}

	fmt.Println(res)

}
```
```
map[i:3 print:1]
```
В данном случае все работает верно. Но что, если мы используем псевдоним `lexeme` для типа `string`?
``` go {.example_for_playground}
package main

type lexeme string

func wordCounter() func(word string) map[string]int {
	res := make(map[string]int)

	return func(word string) map[string]int {
		res[word]++
		return res
	}
}

func main() {
	wc := wordCounter()

	var lexemeToFind lexeme = "i"
	wc(lexemeToFind)
}
```
```
./main.go:18:5: cannot use lexemeToFind (variable of string type lexeme) as string value in argument to wc (exit status 1)
```

Получаем ошибку компиляции. Но ведь по смыслу ничего не изменилось! Псевдоним `lexeme` — это все тот же `string`, только с другим названием. Чтобы функция принимала все типы, под которыми находится `string`, удобно применить дженерик с символом тильды `~`. Достаточно просто поставить символ тильды перед конкретным типом, в момент создания нового типа: 

```go
func wordCounter[T ~string]() func(word T) map[T]int {
	...
}
```

Выражение «тип под типом» означает, что данный тип `T` имеет в качестве базового типа другой тип. Для встроенных типов базовым типом является сам тип. Например, для `float64` — это `float64`. Для составных типом базовый тип определяется по их структуре. Например, для `type Point struct { x int }` базовый тип — `struct{ x int }`. Для псевдонимов базовый тип — это тот тип, из которого объявлен псевдоним. В нашем случае базовый тип `lexeme` — `string`. Выражаясь иначе, под типом `lexeme` лежит тип `string`.

Вот как будет выглядеть полный код:

``` go {.example_for_playground}
package main

import "fmt"

type lexeme string

func wordCounter[T ~string]() func(word T) map[T]int {
	res := make(map[T]int)

	return func(word T) map[T]int {
		res[word]++
		return res
	}
}

func main() {
	var lexemeToFind lexeme = "i"
	wc := wordCounter[lexeme]()
	fmt.Println(wc(lexemeToFind))
}
```
```
map[i:1]
```

Когда вызываем функцию `wordCounter`, также необходимо указывать тип, с которым будем работать. В данном случае этот тип — `lexeme`. В противном случае возникнет ошибка компиляции: 
```go
func main() {
	var lexemeToFind lexeme = "i"
	wc := wordCounter()
	fmt.Println(wc(lexemeToFind))
}
```
```
./main.go:18:21: in call to wordCounter, cannot infer T (declared at ./main.go:7:20) (exit status 1)
```

Таким образом, правильный вариант: 
``` go
func main() {
	var lexemeToFind lexeme = "i"
	wc := wordCounter[lexeme]()
	fmt.Println(wc(lexemeToFind))
}
```
```
map[i:1]
```

Функция `Filter` позволяет фильтровать срез по некоторому предикату. В данном случае предикат представляет собой функцию. При передаче некоторого аргумента в нее функция возвращает значение типа `bool`. Если это значение истинно, то мы добавляем очередной элемент в новый срез. В противном случае — нет. {.task_text}

Сейчас функция `Filter` принимает значения типа `any`. Но как убедиться в том, что тип элементов среза совпадает с типом аргумента, который принимает предикат? Это должен быть один и тот же тип. {.task_text}

С помощью дженерика отредактируйте функцию `Filter` так, чтобы выполнился код из `main`. {.task_text}

```go {.task_source #golang_chapter_0150_task_0020}
package main

import (
	"fmt"
	"net/mail"
)

func valid(email string) bool {
	_, err := mail.ParseAddress(email)
	return err == nil
}

// отредактируйте эту функцию
func Filter(slice []any, predicate func(any) bool) []any {
	var result []any
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func main() {
	slice := []string{"anton@yandex.ru", "https://go.dev/", "go@best@hackers"}
	newSlice := Filter(slice, func(p string) bool {
		return valid(p)
	})
	fmt.Println(newSlice)
	consts := []float64{3.141592653589, 2, 718281828}
	newConsts := Filter(consts, func(p float64) bool {
		return p < 3
	})
	fmt.Println(newConsts)
}
```

Внутри дженерика объявите тип `T`. Тип, который может принимать `T`, — `any`. {.task_hint}

``` go  {.task_answer}
package main

import (
	"fmt"
	"net/mail"
)

func valid(email string) bool {
	_, err := mail.ParseAddress(email)
	return err == nil
}

func Filter[T any](slice []T, predicate func(T) bool) []T {
	var result []T
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}
func main() {
	slice := []string{"anton@yandex.ru", "https://go.dev/", "go@best@hackers"}
	newSlice := Filter(slice, func(p string) bool {
		return valid(p)
	})
	fmt.Println(newSlice)
	consts := []float64{3.141592653589, 2, 718281828}
	newConsts := Filter(consts, func(p float64) bool {
		return p < 3
	})
	fmt.Println(newConsts)
}
```

## Резюме 
1. Дженерики бывают удобны, когда необходимо выполнять одинаковые действия над различными типами.
2. В Go существуют **basic** и **non-basic** интерфейсы. **Non-basic** интерфейсы могут определяться не только через набор методов, но и через конкретные типы. Они используются только в дженериках. 
3. Дженерики используют не только с функциями, но и со структурами. 
4. Тильда рядом с типом означает, что дженерик работает не только с этим типом. Он работает со всеми типами, под которыми лежит данный тип.
