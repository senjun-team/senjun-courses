# Глава 7. Функции

## Объявление и вызов функции

Мы уже встречались с функциями ранее. В данной главе функции будут рассмотрены более подробно.  

Объявление функции соответствует следующему шаблону:

```go
func имяФункции(параметры)(возвращаемыеЗначения) {
    тело
}
``` 

Функция подсчета площади прямоугольника: 

```go {.example_for_playground .example_for_playground_001}
func rectangleSquare(width float64, height float64) float64 {
	return width * height
}
```

Напишите функцию с именем `rectangleSquareByPoints`, которая считает площадь прямоугольника по координатам его левого нижнего и правого верхнего углов.  {.task_text}

В качестве типа данных для координат точек и результата используйте `float64`. {.task_text}

```go {.task_source #golang_chapter_0070_task_0010}
package main
import "fmt"

// ваш код здесь 

func main() {
    // (3; -4) - левый нижний угол, (15; 10) - правый верхний
	fmt.Println(rectangleSquareByPoints(3, -4, 15, 10))
}

// либо здесь

``` 
Реализуйте функцию, которая возвращает произведение разности соответствующих координат. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
)

func main() {
	fmt.Println(rectangleSquareByPoints(3, -4, 15, 10))
}

func rectangleSquareByPoints(x0 float64, y0 float64, x1 float64, y1 float64) float64 {
	return (x1 - x0) * (y1 - y0)
}
```

## Сокращенная запись типов аргументов

Если у нескольких подряд идущих параметров функции совпадает тип, то его можно указать только для последнего из них. А для остальных пропустить.

Вот так могла бы выглядеть функция подсчета площади прямоугольника: 

```go {.example_for_playground .example_for_playground_002}
func rectangleSquare(width, height float64) float64 {
	return width * height
}
```

## Механизм передачи аргументов

Аргументы передаются в функцию **по значению**. Это означает, что изменения копии не влияют на исходный объект. Если аргумент представляет собой ссылку, то функция может влиять на объект через эту ссылку. Например, срез является ссылочным типом.

Рассмотрим функцию `BubbleSort()`, которая реализует метод сортировки среза пузырьком. Она принимает на вход срез и ничего не возвращает. Срез после вызова функции окажется отсортированным, потому что функция косвенно повлияла на срез через ссылку. 

```go {.example_for_playground}
package main

import (
	"fmt"
)

func main() {
	s := []int{10, 8, 3, 7, 6, 1, 0, 4}
	BubbleSort(s)
	fmt.Println(s)
}

func BubbleSort(s []int) {
	for i := 0; i < len(s)-1; i++ {
		for j := 0; j < len(s)-i-1; j++ {
			if s[j] > s[j+1] {
				s[j], s[j+1] = s[j+1], s[j]
			}
		}
	}
}
```

## Вариативные функции

Если необходимо передать переменное число аргументов в функцию, то используются вариативные (variadic) функции. Функция `fmt.Printf` является вариативной. Ей можно передать один или большее число аргументов:

```go {.example_for_playground .example_for_playground_003}
server := "127.0.0.1"
port := "8080"
fmt.Printf("Hello from %s:%s!", server, port) 
```

В данном примере использован прием форматирования строк: на место `%s` вставляются значения переменных `server` и `port`. 

Чтобы объявить вариативную функцию, используют многоточие `...`, которое ставят перед типом последнего аргумента.

Функция `registerUsers` вариативная:

```go {.example_for_playground .example_for_playground_004}
func registerUsers(newUsers ...string) map[int]string {
	var users map[int]string = make(map[int]string)
	var id int
	for _, user := range newUsers {
		users[id] = user
		id++
	}
	return users
}

func main() {
	users := registerUsers("ivanov", "petrov", "sidorov")
	fmt.Println(users)
}
```

Функция `valid` проверяет, является ли строка допустимым email адресом. Напишите функцию с именем `findMails`, которая принимает произвольное количество строк и возвращает слайс, в котором находятся только строки с почтой. {.task_text}

```go {.task_source #golang_chapter_0070_task_0020}
package main

import (
	"fmt"
	"net/mail"
)

func main() {
	fmt.Println(findMails(
		"ivanov@yandex.ru",
		"magic-string",
		"petrov@gmail.com",
		"sidorov@mail.ru",
		"go-programmer.com"))
}

// ваш код здесь 

func valid(email string) bool {
	_, err := mail.ParseAddress(email)
	return err == nil
}

``` 

Функция `findMail` должна быть вариативной. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"net/mail"
)

func main() {
		fmt.Println(findMails(
		"ivanov@yandex.ru",
		"magic-string",
		"petrov@gmail.com",
		"sidorov@mail.ru",
		"go-programmer.com"))
}

func findMails(mails ...string) []string {
	var res []string

	for _, m := range mails {
		if valid(m) {
			res = append(res, m)
		}
	}

	return res
}

func valid(email string) bool {
	_, err := mail.ParseAddress(email)
	return err == nil
}
```

## Именованные возвращаемые значения

Чтобы сделать код функции более лаконичным, иногда используют именованные возвращаемые значения. Объявлять их вручную внутри функции не надо: именованные возвращаемые значения неявно создаются компилятором в начале тела функции.

```go {.example_for_playground .example_for_playground_005}
func average(s []float64) (res float64) {
	for _, el := range s {
		res += el / float64(len(s))
	}
	return
}
```

Функция `average` возвращает `res`. Необходимости в том, чтобы указывать `res` после ключевого слова `return`, нет.

Теперь рядом с возвращаемым значением необходимо ставить скобки:
```go
func average(s []float64) (res float64) 
```

Если возвращаемое значение не именованное, то скобки не нужны:
```go
func average(s []float64) float64
```

## Функции без возвращаемых значений

Функция не обязана возвращать какие-либо значения вообще. В случае, когда в этом нет необходимости, возвращаемые значения не указываются:

```go
func hello(name string) {
	fmt.Printf("Hello, %s!\n", name)
}
``` 

Кстати, функция `main` пакета `main` как раз является функцией без принимаемых и возвращаемых значений.

## Функции, возвращающие несколько значений

Функция может возвращать более одного значения. Часто в качестве последнего значения указывают значение специального типа `error` — ошибки. В случае, когда ошибка произошла, функция вернет значение этого типа с сообщением о том, что пошло не так. Если ошибки не было, то вернется `nil`.

Ошибку можно обработать на месте или вернуть выше:

```go
func hello(name string) error {
	_, err := fmt.Printf("Hello, %s!\n", name)
	return err 
}
```

Напишите функцию с именем `printTriangleType`, которая определяет тип треугольника по координатам его вершин: остроугольный, тупоугольный, прямоугольный или вырожденный. Функция `printTriangleType` принимает шесть аргументов типа `float64` и печатает одно из четырех сообщений на экран: `acute triangle`, `obtuse triangle`, `right triangle` или `degenerate triangle`. {.task_text}

В качестве первого аргумента передается `x0`, в качестве второго — `y0`, третий — `x1`, четвертый — `y1`, пятый — `x2`, шестой — `y2`. {.task_text}

Для расчета расстояния между точками вы можете использовать функции `Math.Pow()` и `Math.Sqrt()` из [пакета math.](https://pkg.go.dev/math) {.task_text}

```go {.task_source #golang_chapter_0070_task_0030}
package main

import (
    "fmt"
    "math"
)

// ваш код здесь 

func main() {
	printTriangleType(5, 7, 5, 17, 10, 7)
}

// либо здесь

``` 

Если сумма меньших сторон треугольника равна большей стороне, то треугольник вырожденный. В остальных случаях рассмотрим квадрат большей стороны и сумму квадратов меньших. Если квадрат большей стороны равен сумме квадратов меньших, то треугольник прямоугольный. Если больше, то тупоугольный. Если меньше, то остроугольный. Учтите также, что архитектура компьютера не позволяет точно представить результат вычислений с вещественными переменными. Проверка на строгое равенство не сработает, нужно учитывать небольшую погрешность. Вы можете взять погрешность в пределах от `-1e-9` до `+1e-9`. {.task_hint}

```go {.task_answer}
package main

import (
    "fmt"
    "math"
)

func printTriangleType(x0 float64, y0 float64, x1 float64, y1 float64, x2 float64, y2 float64) {

	a := dist(x0, y0, x1, y1)
	b := dist(x0, y0, x2, y2)
	c := dist(x1, y1, x2, y2)

	if a > b && a > c {
		a, c = c, a
	}

	if b > a && b > c {
		b, c = c, b
	}

	const errorValue = 1e-9

	switch {
	case c-errorValue < a+b && c+errorValue > a+b:
		fmt.Println("degenerate triangle")
	case c*c-errorValue < a*a+b*b && c*c+errorValue > a*a+b*b:
		fmt.Println("right triangle")
	case c*c > a*a+b*b:
		fmt.Println("obtuse triangle")
	case c*c < a*a+b*b:
		fmt.Println("acute triangle")
	default:
		panic("failed to determine triangle type")
	}
}

func dist(x0 float64, y0 float64, x1 float64, y1 float64) float64 {
	return math.Sqrt(math.Pow(x0-x1, 2) + math.Pow(y0-y1, 2))
}

func main() {
	printTriangleType(5, 7, 5, 17, 10, 7)
}

```

## Рекурсивные функции

Функции называются рекурсивными, если прямо или косвенно вызывают сами себя.

Функция `factorial` в следующем примере рекурсивная:

```go {.example_for_playground}
package main

import (
	"errors"
	"fmt"
	"math"
)

func main() {
	f, err := factorial(5)
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(f)
	}
}

func factorial(n uint64) (uint64, error) {
	if n == 0 {
		return 1, nil
	}
	prev, err := factorial(n - 1)

	if err != nil {
		return 0, err
	}

	return mul64(n, prev)
}

func mul64(left, right uint64) (uint64, error) {

	if left > math.MaxUint64/right {
		return 0, errors.New("uint64 overflow")
	}
	return left * right, nil
}
```

В этом примере присутствует функция `mul64`. Она перемножает два числа типа `uint64` и возвращает ошибку в случае переполнения. Чтобы создать новую ошибку, вызывается функция `New` пакета `errors`, в которую передается сообщение об ошибке.

Через рекурсию удобно реализуются многие интересные алгоритмы. Например, сортировка. Существуют различные алгоритмы сортировки массива, однако часто говорят лишь об учебных примерах. Учебные алгоритмы, например сортировка пузырьком, работают медленно. Вычислительная сложность сортировки пузырьком — *O(n^2)*.   

На практике используют более совершенные алгоритмы. Например, сортировку слиянием. Его вычислительная сложность — *O(n\*log(n))*. 

Смысл этого алгоритма состоит в том, что на каждом шаге мы разбиваем массив на две примерно равные части. Если число элементов нечетное, то слева окажется на один элемент больше. Повторяем процесс для каждой из половинок, пока не получим массивы длиной в один элемент. Массив из одного элемента всегда отсортирован. 

Затем начинаем объединять половинки. Идем параллельно по каждой из половинок. Если элемент в первой половинке меньше элемента второй половинки, то добавляем его в результирующий массив. После чего продвигаемся на один элемент вперед в первой половинке. Аналогично для второй половинки. Когда окажется, что одна из половинок закончилась, то необходимо дописать все оставшиеся элементы в конец результирующего массива.

![Пример сортировки слиянием](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/golang/merge_sort.jpg)

Реализуйте функцию `mergeSort` для сортировки слиянием среза из целых чисел. Функция должна принимать срез из элементов типа `[]int` и возвращать отсортированный срез элементов типа `[]int`. {.task_text}

```go {.task_source #golang_chapter_0070_task_0040}
package main
import "fmt"

// ваш код здесь 

func main() {
    a := []int{4, 7, 10, 1}
	a = mergeSort(a)
	fmt.Println(a)
}

// либо здесь

``` 
Реализуйте две функции: `merge` и `mergeSort`. Функция `mergeSort` рекурсивно разбивает срез на половинки и возвращает результат работы функции `merge`. Последняя объединяет половинки. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func main() {
	a := []int{4, 7, 10, 1}
	a = mergeSort(a)
	fmt.Println(a)
}

func mergeSort(items []int) []int {
	if len(items) < 2 {
		return items
	}
	first := mergeSort(items[:len(items)/2])
	second := mergeSort(items[len(items)/2:])
	return merge(first, second)
}

func merge(a []int, b []int) []int {
	final := []int{}
	i := 0
	j := 0
	for i < len(a) && j < len(b) {
		if a[i] < b[j] {
			final = append(final, a[i])
			i++
		} else {
			final = append(final, b[j])
			j++
		}
	}
	for ; i < len(a); i++ {
		final = append(final, a[i])
	}
	for ; j < len(b); j++ {
		final = append(final, b[j])
	}
	return final
}
```

## Отложенный вызов функций

Вызов функции бывает удобно отложить. Например, открыть файл и обозначить намерение его закрыть после завершения записи данных. Подобные сценарии реализуются с помощью ключевого слова `defer`. Оно позволяет отсрочить вызов функции до завершения выполнения текущей функции.

При выводе приветствия пользователю мы точно знаем, что в конце программы попрощаемся с ним. Чтобы не забыть это сделать впоследствии, воспользуемся `defer`:

```go {.example_for_playground}
package main

import "fmt"

func hello(name string) {
	fmt.Printf("Hello, %s!\n", name)
}

func goodbuy(name string) {
	fmt.Printf("Goodbuy, %s!\n", name)
}

func main() {
	sep := "===================="
	hello("gopher")
	defer goodbuy("gopher")
	fmt.Println(sep)
	fmt.Println("Here we study defer")
	fmt.Println(sep)
}
```

Вызов функции `goodbuy` откладывается до завершения функции `main` независимо от того, как эта функция завершится: с ошибкой или без нее. Допустимо откладывать любое количество вызов. Выполняются они в порядке, обратном тому, в котором были отложены:

```go {.example_for_playground}
package main

import "fmt"

func closeConn(id int) {
	fmt.Printf("%d connection closed\n", id)
}

func main() {
	for i := 0; i < 5; i++ {
		defer closeConn(i)
	}
}
```

## Резюме

1. Объявление функции соответствует шаблону, в котором параметры и возвращаемые значения необязательны.
2. Если у подряд идущих параметров одинаковый тип, то его можно указать только для последнего параметра, а для предыдущих — пропустить.
3. При передаче аргументов в функцию мы работаем с копиями этих объектов внутри функции. В Go существует только передача аргументов по значению. Передать аргумент по ссылке, как, например, в языке C, невозможно. Однако допустимо передать в функцию указатель по значению.
4. Функция может возвращать как одно, так и несколько значений, либо вовсе не возвращать никаких значений. 
5. Функции, принимающие переменное число параметров, называются вариативными. 
6. В Go реализованы рекурсивные функции. Когда мы работаем с рекурсивными функциями в других языках, то часто приходится думать о том, чтобы не произошло переполнения стека вызовов. Типичные реализации Go используют стек вызовов переменного размера, который вырастает по мере необходимости.
7. Вызов функции можно отложить с помощью ключевого слова `defer`.
