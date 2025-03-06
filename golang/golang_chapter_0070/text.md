# Глава 7. Функции
## Объявление и вызов функции 
Мы уже встречались с функциями ранее. В данной главе функции будут рассмотрены более подробно.  

Объявление функции соответствует следующему шаблону:

```
func имя функции(параметры)(возвращаемые значения){
    тело
}
``` 

Функция подсчета площади прямоугольника: 

```go {.example_for_playground .example_for_playground_001}
func rectangleSquare(width float64, height float64) float64 {
	return width * height
}
```

Напишите функцию с именем `rectangleSquareByPoints`, которая считает площадь прямоугольника по координатам его левого нижнего и правого верхнего углов. В качестве типа данных для координат точек и результата используйте `float64`. {.task_text}

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
	fmt.Println(rectangleSquareByPoints(15, 10, 3, -4))
}

func rectangleSquareByPoints(x0 float64, y0 float64, x1 float64, y1 float64) float64 {
	return (x1 - x0) * (y1 - y0)
}
```

## Сокращенная запись типов принимаемых аргументов
Если у нескольких подряд идущих параметров функции совпадает тип, то его можно указать только для последнего из них. А для остальных пропустить. Вот так могла бы выглядеть функция подсчета площади прямоугольника: 

```go {.example_for_playground .example_for_playground_002}
func rectangleSquare(width, height float64) float64 {
	return width * height
}
```

## Механизм передачи аргументов в функцию
Аргументы передаются в функцию **по значению**. Это означает, что изменения копии не влияют на исходный объект. Если аргумент представляет собой ссылку, то функция может влиять на объект через эту ссылку. Например, срез является ссылочным типом. Следующая функция реализует метод сортировки среза пузырьком. Она принимает на вход срез и ничего не возвращает. Срез после вызова такой функции окажется отсортированным, потому что функция косвенно повлияла на срез через эту ссылку. 

```go {.example_for_playground .example_for_playground_011}
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
Если необходимо передать переменное число аргументов в функцию, то используются вариативные (*variadic*) функции. Функция `fmt.Printf` является вариативной. Ей можно передать один или большее число аргументов:

```go {.example_for_playground .example_for_playground_009}
server := "127.0.0.1"
port := "8080"
fmt.Printf("Hello from %s:%s!", server, port) 
```

В данном примере использован прием форматирования строк: на место `%s` вставляются значения переменных `server` и `port`. 

Чтобы объявить вариативную функцию, используют многоточие `...`, которое ставят перед типом последнего аргумента. Функция `addUsers` вариативная:

```go {.example_for_playground .example_for_playground_010}
var users map[int]string = make(map[int]string)
var id int

func addUsers(newUsers ...string) {
	for _, user := range newUsers {
		users[id] = user
		id++
	}
}

func main() {
	addUsers("ivanov", "petrov", "sidorov")
}
```

Функция `valid` проверяет, является ли строка допустимым email адресом. Напишите функцию с именем `findMails`, которая принимает на вход произвольное количество строк и возвращает слайс, в котором находятся только строки с почтой. {.task_text}

```go {.task_source #golang_chapter_0070_task_0030}
package main

import (
	"fmt"
	"net/mail"
)

func main() {
	fmt.Println(findMails("ivanov@yandex.ru", "magic-string", "petrov@gmail.com", "sidorov@mail.ru", "go-programmer.com"))
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
	fmt.Println(findMails("ivanov@yandex.ru", "magic-string", "petrov@gmail.com", "sidorov@mail.ru", "go-programmer.com"))
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
Чтобы сделать код функции более лаконичным, иногда используют именованные возвращаемые значения: 

```go {.example_for_playground .example_for_playground_003}
func average(s []float64) (res float64) {
	for _, el := range s {
		res += el / float64(len(s))
	}
	return
}
```

Функция `average` возвращает `res`. Необходимости в том, чтобы указывать `res` после ключевого слова `return`, нет.

## Функции без возвращаемых значений
Функция не обязана возвращать какие-либо значения вообще. В случае, когда в этом нет необходимости, возвращаемые значения не указываются:

```go {.example_for_playground .example_for_playground_004}
func hello(name string) {
	fmt.Printf("Hello, %s!\n", name)
}
``` 

Кстати, функция `main` пакета `main` как раз является функцией без принимаемых и возвращаемых значений.

## Функции, возвращающие несколько значений
Функция может возвращать более одного значения. Часто в качестве последнего значения указывают значение специального типа `error` — ошибки. В случае, когда ошибка произошла, функция вернет значение этого типа с сообщением о том, что пошло не так. Если ошибки не было, то вернется `nil`. Получив ошибку, ее необходимо обработать на месте или вернуть выше:

```go {.example_for_playground .example_for_playground_008}
func hello(name string) {
	_, err := fmt.Printf("Hello, %s!\n", name)
	if err != nil {
		panic(err)
	}
}
```

Напишите функцию с именем `printTriangleType`, которая определяет тип треугольника по координатам его вершин: остроугольный, тупоугольный, прямоугольный или вырожденный. Функция `printTriangleType` принимает шесть аргументов типа `float64` и печатает одно из четырех сообщений на экран: `acute triangle`, `obtuse triangle`, `right triangle` или `degenerate triangle`. В качестве первого аргумента передается `x0`, в качестве второго — `y0`, третий — `x1`, четвертый — `y1`, пятый — `x2`, шестой — `y2`. {.task_text}

```go {.task_source #golang_chapter_0070_task_0020}
package main
import "fmt"

// ваш код здесь 

func main() {
	printTriangleType(5, 7, 5, 17, 10, 7)	
}

// либо здесь

``` 

Если сумма меньших сторон треугольника равна большей стороне, то треугольник вырожденный. В остальных случаях рассмотрим квадрат большей стороны и сумму квадратов меньших. Если квадрат большей стороны равен сумме квадратов меньших, то треугольник прямоугольный. Если больше, то тупоугольный. Если меньше, то остроугольный. Учтите также, что архитектура компьютера не позволяет точно представить результат вычислений с вещественными переменными. Проверка на строгое равенство не сработает, нужно учитывать небольшую погрешность. {.task_hint}

```go {.task_answer}
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

	const errorVelue = 1e-9

	switch {
	case c-errorVelue < a+b && c+errorVelue > a+b:
		fmt.Println("degenerate triangle")
	case c*c-errorVelue < a*a+b*b && c*c+errorVelue > a*a+b*b:
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

```

## Рекурсивные функции
Функции называются рекурсивными, если вызывают сами себя. Функция `factorial` в следующем примере рекурсивная:

```go {.example_for_playground .example_for_playground_020}
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

Функция `mul64` перемножает два числа типа `uint64` и возвращает ошибку в случае переполнения. Чтобы создать новую ошибку, пользуются функцией `New` пакета `errors`, куда передают сообщение об ошибке. 

Существуют различные алгоритмы сортировки массива, однако часто говорят лишь об учебных примерах. Учебные алгоритмы, например, алгоритм сортировки пузырьком, работают медленно. Вычислительная сложность алгоритма сортировки пузырьком — *O(n^2)*. На практике используют более совершенные алгоритмы, например, алгоритм сортировки слиянием. Его вычислительная сложность — *O(n\*log(n))*. Смысл этого алгоритма состоит в том, что на каждом шаге мы разбиваем массив на две примерно равные части. Если число элементов нечетное, то слева окажется на один элемент больше. Повторяем процесс для каждой из половинок, пока не получим массивы длиной один. Массив из одного элемента всегда отсортирован. Затем начинаем объединять половинки. Идем параллельно по каждой из половинок. Если элемент в первой половинке меньше элемента второй половинки, то добавляем его в результирующий массив, после чего продвигаемся на один элемент вперед в первой половинке. Аналогично для второй половинки. Когда окажется, что одна из половинок закончилась, то необходимо дописать все оставшиеся элементы в конец результирующего массива.

![merge_sort.jpg](merge_sort.jpg)

 Реализуйте функцию `mergeSort` сортировки слиянием для среза целых чисел. Функция должна принимать срез из элементов типа `[]int` и возвращать отсортированный срез элементов типа `[]int`. {.task_text}

```go {.task_source #golang_chapter_0070_task_0050}
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

import (
	"fmt"
)

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

## Отложенные вызовы функций
Вызов функции бывает удобно отложить. Например, при выводе приветствия пользователю мы точно знаем, что в конце программы попрощаемся с ним. Чтобы не забыть это сделать впоследствии, используют ключевое слово `defer`:

```go {.example_for_playground .example_for_playground_005}
package main

import (
	"fmt"
)

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

Вызов функции `goodbuy` откладывается до завершения функции `main`, независимо от того, как эта функция завершится: с ошибкой или без нее. Допустимо откладывать любое количество вызов. Выполняются они в порядке, обратном тому, в котором были отложены:

```go {.example_for_playground .example_for_playground_005}
package main

import (
	"fmt"
)

func closeConn(id int) {
	fmt.Printf("%d connection closed\n", id)
}

func main() {
	for i := 0; i < 5; i++ {
		defer closeConn(i)
	}
}
```

## Резюме.
1. Объявление функции соответствует шаблону, однако некоторые из элементов этого шаблона необязательные.
2. Чтобы сократить объем программного кода, иногда бывает полезным сделать возвращаемые значения функции именованными, а некоторые типы принимаемых аргументов пропустить.
3. При передаче аргументов в функцию, мы работаем с копиями этих объектов внутри функции. В Go существует только передача аргументов по значению. Передать аргумент по ссылке, как, например, в языке `C`, невозможно. Однако допустимо передать в функцию указатель по значению.
4. Функция может возвращать как одно, так и несколько значений, либо вовсе не возвращать никаких значений. 
5. Функции, принимающие переменное число аргументов, называются вариативными. 
6. В Go реализованы рекурсивные функции. Когда мы работаем с рекурсивными функциями в других языках, то часто приходится думать о том, чтобы не произошло переполнения стека вызовов. Типичные реализации Go используют стек вызовов переменного размера, который вырастает по мере необходимости.
7. Вызов функции бывает удобно отложить, например, закрытие файла при его создании и записи. 



