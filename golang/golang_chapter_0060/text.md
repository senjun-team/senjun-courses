# Глава 6. Отображения
## Понятие отображения

Отображения в Go реализуют идею хеш-таблиц или карт (map). Хеш-таблица — это структура, которая представляет собой неупорядоченный набор пар «ключ — значение». Ключи в хеш-таблице уникальны. К каждому ключу привязано значение. Отображения в Go представляют собой ссылку на такую хеш-таблицу. 

Создать отображение:

```go
var catWeights map[string]int
``` 

В данным примере ключи имеют тип `string`, а значения — тип `int`. Использовать несколько типов для одного ключа или значения нельзя. Типы ключей должны быть проверяемы на равенство, чтобы можно было различать два ключа между собой.  

Мы объявили отображение, но не инициализировали его, оно равно `nil`. Делать так можно, но это не имеет особого смысла. При попытке заполнения отображения данными произойдет ошибка. Чтобы избежать таких проблем, при создании отображения используют встроенную функцию `make`:

```go
catsWeights := make(map[string]int)
 ```

## Заполнение отображения данными
`catsWeights` — пустое отображение. Выведем его через функцию `fmt.Println`:

```go
catsWeights := make(map[string]int)
fmt.Println(catsWeights)
```

Вот, что мы увидим:

```
map[]
```

Заполним это отображение данными:

 ```go
catsWeights["Barsik"] = 10
catsWeights["Busa"] = 2
 ```

Выведем отображение на экран:

```go
fmt.Println(catsWeights)
```

Так выглядит заполнение данными при создании отображения:

```go
catsWeights := map[string]int{
    "Barsik": 10,
    "Busa":   2,
}
```

## Работа с элементами отображения

Так выглядит обращение к элементу отображения по его ключу:
```go
fmt.Println(catsWeights["Busa"])
```

Если вы попытаетесь узнать значение несуществующего элемента, то получите значение типа по умолчанию. Например, следующий код напечатает `0`:

```go
fmt.Println(catsWeights["Sharik"])
```

Функция `piDigit()` по индексу возвращает очередной знак числа Пи (3.1415926535...) после десятичной точки, начиная с нулевого знака и заканчивая девятым включительно. {.task_text}

С использованием `piDigit()` реализуйте тело функции `countDigits()`. Она принимает количество цифр числа Пи после десятичной точки (`0 <= n <= 10`) и возвращает отображение с количестом раз, которое встретилась каждая цифра. Цифры, не встретившиеся ни разу, в отображение включать не нужно. {.task_text}

Например, для `countDigits(5)` результат следующий: `map[1:2 4:1 5:1 9:1]`. Таким образом, для последовательности цифр `14159` цифра `1` встречается `2` раза; `4`, `5`, `9` - по одному разу. Другие цифры не встречаются. {.task_text}

```go {.task_source #golang_chapter_0060_task_0010}
package main

import (
	"fmt"
	"math"
	"strconv"
)

func piDigit(index int) int {
	const maxLen = 10
	if index < 0 || index >= maxLen {
		return -1
	}

	const integerPartLen = 2
	piStr := strconv.FormatFloat(math.Pi, 'f', maxLen+integerPartLen, 64)
	res, _ := strconv.Atoi(string(piStr[index+integerPartLen]))
	return res
}

func countDigits(n int) map[int]int {
	// ваш код здесь 
}

func main() {
	fmt.Println(countDigits(10))
}
```

Добавляйте в цикле единицу значению отображения, ключом которого является очередная цифра числа Пи. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math"
	"strconv"
)

func piDigit(index int) int {
	const maxLen = 10
	if index < 0 || index >= maxLen {
		return -1
	}

	const integerPartLen = 2
	piStr := strconv.FormatFloat(math.Pi, 'f', maxLen+integerPartLen, 64)
	res, _ := strconv.Atoi(string(piStr[index+integerPartLen]))
	return res
}

func countDigits(n int) map[int]int {
	res := make(map[int]int)

	for i := 0; i < n; i++ {
		res[piDigit(i)]++
	}

	return res
}

func main() {
	fmt.Println(countDigits(10))
}
```

Так выглядит удаление элемента из отображения:

```go
delete(catsWeights, "Busa")
```

Узнать длину отображения позволяет встроенная функция `len()`:

```go
fmt.Println(len(catsWeights)) 
```

## Перебор элементов отображения
Чтобы перебрать все пары «ключ-значение», воспользуйтесь циклом и ключевым словом `range`:
```go
for key, val := range catsWeights {
	fmt.Println(key, ":", val)
}
```
Порядок, в котором располагаются элементы в отображении, **не определен** и изменяется от одного запуска программы к другому. Это сделано намеренно, поскольку из-за особенностей реализации нельзя гарантировать, что в разных версиях языка порядок окажется одним и тем же. Решение, при котором порядок элементов отображения не определен даже от запуска к запуску, позволяет писать более надежные программы. Если нужен конкретный порядок элементов, то их придется отсортировать самостоятельно. Например, строковые ключи можно отсортировать функцией `Strings` пакета `sort`.

Функция `calculateConsts` возвращает отображение из нескольких констант, каждая из которых содержит некоторое количество цифр после десятичной точки. Количество цифр после десятичной точки каждой из констант генерируется псевдослучайным образом в зависимости от входного аргумента `seed`. Реализуйте тело функции `chooseConsts`, которая принимает на вход отображение и удаляет из него те константы, которые содержат после десятичной точки менее 5 цифр. Константы в отображении представлены строками. Длину строки в байтах позволяет узнать встроенная функция `len()`. {.task_text}

```go {.task_source #golang_chapter_0060_task_0020}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"strings"
)

func calculateConsts(seed int64) map[string]string {
	const constsNum = 7 // постоянное значение
	const maxLen = 10

	randomGenerator := rand.New(rand.NewSource(seed))

	var digitsNumberSlice []int
	for i := 0; i < constsNum; i++ {
		digitsNumber := randomGenerator.Intn(maxLen) + 1
		digitsNumberSlice = append(digitsNumberSlice, digitsNumber)
	}

	return map[string]string{
		"E":      strconv.FormatFloat(math.E, 'f', maxLen+2, 64)[:digitsNumberSlice[0]+2],
		"Pi":     strconv.FormatFloat(math.Pi, 'f', maxLen+2, 64)[:digitsNumberSlice[1]+2],
		"Sqrt2":  strconv.FormatFloat(math.Sqrt2, 'f', maxLen+2, 64)[:digitsNumberSlice[2]+2],
		"SqrtE":  strconv.FormatFloat(math.SqrtE, 'f', maxLen+2, 64)[:digitsNumberSlice[3]+2],
		"SqrtPi": strconv.FormatFloat(math.SqrtPi, 'f', maxLen+2, 64)[:digitsNumberSlice[4]+2],
		"Ln2":    strconv.FormatFloat(math.Ln2, 'f', maxLen+2, 64)[:digitsNumberSlice[5]+2],
		"Ln10":   strconv.FormatFloat(math.Ln10, 'f', maxLen+2, 64)[:digitsNumberSlice[6]+2],
	}
}

func chooseConsts(consts map[string]string) {
	// ваш код здесь 
}

func main() {
	// аргумент можно менять для отладки 
	consts := calculateConsts(42) 
	fmt.Println(consts)
	chooseConsts(consts)
	fmt.Println(consts)
}

```

Чтобы узнать число цифр после десятичной точки, воспользуйтесь функцией `Split` пакета `strings`, разбив строку на две: до точки и после точки, а затем вычислите длину строки после точки через встроенную функцию `len()`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"strings"
)

func calculateConsts(seed int64) map[string]string {
	const constsNum = 7 
	const maxLen = 10

	randomGenerator := rand.New(rand.NewSource(seed))

	var digitsNumberSlice []int
	for i := 0; i < constsNum; i++ {
		digitsNumber := randomGenerator.Intn(maxLen) + 1
		digitsNumberSlice = append(digitsNumberSlice, digitsNumber)
	}

	return map[string]string{
		"E":      strconv.FormatFloat(math.E, 'f', maxLen+2, 64)[:digitsNumberSlice[0]+2],
		"Pi":     strconv.FormatFloat(math.Pi, 'f', maxLen+2, 64)[:digitsNumberSlice[1]+2],
		"Sqrt2":  strconv.FormatFloat(math.Sqrt2, 'f', maxLen+2, 64)[:digitsNumberSlice[2]+2],
		"SqrtE":  strconv.FormatFloat(math.SqrtE, 'f', maxLen+2, 64)[:digitsNumberSlice[3]+2],
		"SqrtPi": strconv.FormatFloat(math.SqrtPi, 'f', maxLen+2, 64)[:digitsNumberSlice[4]+2],
		"Ln2":    strconv.FormatFloat(math.Ln2, 'f', maxLen+2, 64)[:digitsNumberSlice[5]+2],
		"Ln10":   strconv.FormatFloat(math.Ln10, 'f', maxLen+2, 64)[:digitsNumberSlice[6]+2],
	}
}

func chooseConsts(consts map[string]string) {
	digitsNumber := 5

	var keysToDelete []string

	for key, val := range consts {

		if len(strings.Split(val, ".")[1]) < digitsNumber {
			keysToDelete = append(keysToDelete, key)
		}
	}

	for _, key := range keysToDelete {
		delete(consts, key)
	}
}

func main() {
	consts := calculateConsts(42)
	fmt.Println(consts)
	chooseConsts(consts)
	fmt.Println(consts)

}

```

## Проверка наличия элемента в отображении
Когда вы обращаетесь к элементу отображения, то всегда получаете значение. Не важно, содержится элемент в отображении или просто имеет значение по умолчанию. В большинстве случаев это нормально, однако иногда нужно точно знать, содержит ли отображение данный элемент. На самом деле, когда вы обращаетесь к элементу отображения, то возвращаются само значение отображения и значение типа `bool`. Последнее равно `true`, если элемент содержится в отображении и `false` — в противном случае. Часто его присваивают переменной с именем `ok`:

```go
catsWeights := make(map[string]int)
catsWeights["Barsik"] = 10
catsWeights["Busa"] = 2

if catsWeight, ok := catsWeights["Sharik"]; !ok {
	fmt.Println("no key Sharik in the catsWeights")
	fmt.Println("catsWeight =", catsWeight)
} 
```

Если переменная `ok` не нужна, то объявлять ее не следует:

```go 
catsWeight := catsWeights["Sharik"]
fmt.Println("catsWeight =", catsWeight)
```

Как и срезы, отображения нельзя сравнивать друг с другом. Отображение можно сравнить только со значением `nil`. Напишите тело функции `equalWeights`, которая проверяет, содержат ли два среза типа `map[string]int` одинаковые ключи и связанные с ними значения. Функция `equalWeights` возвращает `true`, если это так, и `false` — в противном случае. {.task_text}

```go {.task_source #golang_chapter_0060_task_0030}
package main

import "fmt"

func equalWeights(x, y map[string]int) bool {
	// ваш код здесь 
}

func main() {
	catsWeights := map[string]int{
		"Barsik": 10,
		"Busa":   2,
		"Murzik": 0,
	}

	dogsWeights := map[string]int{
		"Barsik": 10,
		"Busa":   2,
		"Sharik": 0,
	}

	fmt.Println(equalWeights(catsWeights, dogsWeights))
}
```

Проверьте длины передаваемых отображений. Если длины не равны, то возвращаем `false`. В противном случае организуйте цикл по элементам отображения и сравните их. Учтите: когда вы обращаетесь к элементу отображения, то элемент может как содержать значение по умолчанию, так и вовсе отсутствовать в отображении. В обоих случаях вы получите значение по умолчанию. Две эти ситуации необходимо различать. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func equalWeights(firstWeights, secondWeights map[string]int) bool {
	if len(firstWeights) != len(secondWeights) {
		return false
	}

	for key, firstValue := range firstWeights {
		if secondValue, ok := secondWeights[key]; !ok || secondValue != firstValue {
			return false
		}
	}

	return true
}

func main() {
	catsWeights := map[string]int{
		"Barsik": 10,
		"Busa":   2,
		"Murzik": 0,
	}

	dogsWeights := map[string]int{
		"Barsik": 10,
		"Busa":   2,
		"Sharik": 0,
	}

	fmt.Println(equalWeights(catsWeights, dogsWeights))
}

```

## Реализация множеств
Во многих современных языках, например в C++ и Python, существуют множества `set`. Когда мы добавляем элементы в множество, то каждый из добавленных элементов оказывается там только один раз. Каждое значение внутри множества уникально. В Go множества отсутствуют, однако для этих целей используют отображения. Отображение хранит пары: уникальные ключи и привязанные к ним значения. Для решения реальных задач значения нужны не всегда. Зачастую достаточно хранить только ключи. Без значений. Если использовать отображение с ключом требуемого типа и значением типа пустой структуры `struct{}`, то мы получим отображение, в котором добавляемые ключи встречаются лишь однажды. В качестве значения всегда указывается `struct{}{}`. Пустая структура, во-первых, не занимает места. Во-вторых, это подчеркивает намерение: нам нужны ключи без значений. Попробуйте реализовать эту идею в задаче подсчета уникальных слов в тексте.

В переменной `s` типа `string` содержится некоторый текст. Допишите функцию `countUniq`, которая подсчитает число уникальных слов в этом тексте. Словом считать любую последовательность символов, разделяемую пробелами. {.task_text}

```go {.task_source #golang_chapter_0060_task_0040}
package main

import (
	"fmt"
	"strings"
)

func countUniq(s string) int {
	// ваш код здесь
}
func main() {
	s := "программисты компилируемого языка Go пишут программы на Go с использованием многих удобных возможностей языка"

	fmt.Println(countUniq(s))
}

``` 

Создайте отображение типа `map[string]struct{}`. Добавляйте в цикле каждое слово в качестве ключа отображения, а в качестве его значения — `struct{}{}`. Таким образом, вы получите отображение с уникальными словами. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"strings"
)

func countUniq(s string) int {

	if s == "" {
		return 0
	}

	words := strings.Split(s, " ")
	seen := make(map[string]struct{})

	for _, word := range words {
		seen[word] = struct{}{}
	}

	return len(seen)
}
func main() {
	s := "программисты компилируемого языка Go пишут программы на Go с использованием многих удобных возможностей языка"

	fmt.Println(countUniq(s))
}
```

## Резюме
1. В Go хеш-таблицы реализованы через тип `map`.
2. Отображение — это ссылка на хеш-таблицу.
3. Отображения заполняются парами «ключ — значение» с уникальными ключами.
4. При попытке узнать значение несуществующего элемента отображения вы получите значение типа по умолчанию. Чтобы узнать, содержится ли элемент в отображении, стоит вспомнить: при обращении к элементу отображения возвращается не только значение отображения, но и флаг. Этот флаг содержит `true`, если элемент содержится в отображении. В противном случае — `false`.
5. Встроенная функция `delete` позволяет удалить элемент из отображения, `len` — узнать длину отображения, ключевое слово `range` — перебрать элементы отображения.
6. Порядок элементов отображения не определен.
7. Существует специальный прием, который позволяет реализовать множества стандартными средствами языка Go.