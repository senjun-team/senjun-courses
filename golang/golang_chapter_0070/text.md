# Глава 7. Функции
## Понятие функции  
Как и в других языках программирования, функции в Go позволяют многократно использовать один и тот же код. Мы уже встречались с функциями ранее. В данной главе функции будут рассмотрены более подробно. Объявление функции соответствует следующему шаблону:

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

## Именованные возвращаемые значения
Чтобы сделать код функции более лаконичным, иногда используют именованные возвращаемые значения: 

```go {.example_for_playground .example_for_playground_002}
func average(myslice []float64) (res float64) {
	for _, el := range myslice {
		res += el / float64(len(myslice))
	}
	return
}
```

## Функции без возвращаемых значений
Функция не обязана возвращать какие-либо значения вообще. В случае, когда в этом нет необходимости, возвращаемые значения не указываются:

```go {.example_for_playground .example_for_playground_003}
func hello(name string) {
	fmt.Printf("Hello, %s!\n", name)
}
```

В данном примере использован прием форматирования строк: на место `%s` вставляется значение переменной `name`. 

## Функции, возвращающие несколько значений
Функция может возвращать более одного значения. Часто в качестве последнего значения указывают значение специального типа `error` — ошибки. В случае, когда ошибка произошла, функция вернет значение этого типа с сообщением о том, что пошло не так. Если ошибки не было, то вернется `nil`. Получив ошибку, ее необходимо обработать на месте или вернуть выше. В случае, когда ошибка возникла в следствие неправильного программного кода, вызывают функцию `panic`. Функция `panic` немедленно завершает работу программы с сообщением о том, где произошла паника:

```go {.example_for_playground .example_for_playground_004}
func hello(name string) {
	_, err := fmt.Printf("Hello, %s!\n", name)
	if err != nil {
		panic(err)
	}
}
```

Функцию `panic` необходимо использовать осторожно и только в том случае, когда это действительно необходимо.

Напишите функцию с именем `printTriangleType`, которая определяет тип треугольника по координатам его вершин: остроугольный, тупоугольный, прямоугольный или вырожденный. Функция `printTriangleType` принимает шесть аргументов типа `float64` и печатает одно из четырех сообщений на экран: `acute triangle`, `obtuse triangle`, `right triangle` или `degenerate triangle`. {.task_text}

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

## Механизм передачи аргументов в функцию
Аргументы передаются в функцию **по значению**, за исключением аргументов-ссылок. Это означает, что изменения копии не влияют на исходный объект, если этот объект не ссылка. Если аргумент представляет собой ссылку, то функция может влиять на объект через эту ссылку. Например, срез является ссылочным типом. Следующая функция реализует метод сортировки среза пузырьком. Она принимает на вход срез и ничего не возвращает. Срез после вызова такой функции окажется отсортированным, потому что функция косвенно повлияла на срез через эту ссылку. 

```go {.example_for_playground .example_for_playground_005}
package main

import (
	"fmt"
)

func main() {
	myslice := []int{10, 8, 3, 7, 6, 1, 0, 4}
	BubbleSort(myslice)
	fmt.Println(myslice)
}

func BubbleSort(myslice []int) {
	for i := 0; i < len(myslice)-1; i++ {
		for j := 0; j < len(myslice)-i-1; j++ {
			if myslice[j] > myslice[j+1] {
				myslice[j], myslice[j+1] = myslice[j+1], myslice[j]
			}
		}
	}
}
```

## Рекурсивные функции
Функции бывают рекурсивными.
