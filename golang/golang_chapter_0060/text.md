# Глава 6. Отображения
## Понятие отображения

Отображения в Go реализуют идею хеш-таблиц или карт (map). Хеш-таблица — это структура, которая представляет собой неупорядоченный набор пар «ключ — значение». Ключи в хеш-таблице уникальны. К каждому ключу привязано значение. Отображения в Go представляют собой ссылку на такую хеш-таблицу.

Создание отображения с ключом типа `string` и значением типа `int`:

```go {.example_for_playground .example_for_playground_001}
var catsWeights map[string]int
``` 

Использовать несколько типов для одного ключа или значения нельзя. Типы ключей должны быть проверяемыми на равенство, чтобы можно было различать два ключа между собой.

Мы объявили отображение, но не инициализировали его: оно равно `nil`. Делать так можно, но это не имеет особого смысла. При попытке заполнения отображения данными произойдет ошибка. Чтобы избежать таких проблем, при создании отображения используют встроенную функцию `make`:

```go {.example_for_playground .example_for_playground_002}
catsWeights := make(map[string]int)
```

## Заполнение отображения данными
`catsWeights` — пустое отображение. Выведем его через функцию `fmt.Println`:

```go  {.example_for_playground .example_for_playground_003}
catsWeights := make(map[string]int)
fmt.Println(catsWeights)
```
```
map[]
```

Заполним это отображение данными:

```go {.example_for_playground .example_for_playground_004}
catsWeights["Barsik"] = 10
catsWeights["Busa"] = 2

fmt.Println(catsWeights)
```

Так выглядит заполнение отображения данными в момент создания:

```go {.example_for_playground .example_for_playground_005}
catsWeights := map[string]int{
    "Barsik": 10,
    "Busa":   2,
}
```

## Работа с элементами отображения

Так выглядит обращение к элементу отображения по его ключу:

```go  {.example_for_playground .example_for_playground_006}
fmt.Println(catsWeights["Busa"])
```

Если вы попытаетесь узнать значение несуществующего элемента, то получите значение типа по умолчанию:

```go {.example_for_playground .example_for_playground_007}
fmt.Println(catsWeights["Sharik"])
```
```
0
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

```go {.example_for_playground .example_for_playground_008}
delete(catsWeights, "Busa")
```

Встроенная функция `delete` удаляет элемент по ключу. Если ключ не найден, функция ничего не делает.

Узнать длину отображения позволяет встроенная функция `len()`:

```go
fmt.Println(len(catsWeights)) 
```

## Перебор элементов отображения
Чтобы перебрать все пары «ключ-значение», воспользуйтесь циклом и ключевым словом `range`:

```go {.example_for_playground .example_for_playground_009}
for key, val := range catsWeights {
	fmt.Println(key, ":", val)
}
```

Порядок, в котором располагаются элементы в отображении, **не определен** и изменяется от одного запуска программы к другому. Это сделано намеренно, поскольку из-за особенностей реализации нельзя гарантировать, что в разных версиях языка порядок окажется одним и тем же. Решение, при котором порядок элементов отображения не определен даже от запуска к запуску, позволяет писать более надежные программы.

Если нужен конкретный порядок элементов, то их придется отсортировать самостоятельно. Например, строковые ключи можно отсортировать функцией `Strings` пакета `sort`.

Функция `calculateConsts()` возвращает отображение, в котором наименованиям констант соотнесены их значения. Значение каждой константы содержит некоторое количество цифр после десятичной точки. Оно генерируется псевдослучайным образом в зависимости от входного аргумента `seed`.  {.task_text}

Реализуйте тело функции `chooseConsts()`, которая принимает на вход это отображение и удаляет из него те константы, которые содержат после десятичной точки менее 5 цифр. Константы в отображении представлены строками. Длину строки в байтах позволяет узнать встроенная функция `len()`. {.task_text}

```go {.task_source #golang_chapter_0060_task_0020}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"strings"
)

func randLenStr(val float64, pseudoRandGen *rand.Rand) string {
	// дробная часть чисел не длиннее 10 знаков
	const prec = 10
	// значение в интервале [1, 11)
	fractionalPartLen := pseudoRandGen.Intn(prec) + 1
	strVal := strconv.FormatFloat(val, 'f', prec, 64)
	integerPartLen := strings.IndexRune(strVal, '.') + 1
	return strVal[:integerPartLen + fractionalPartLen]
}

func calculateConsts(seed int64) map[string]string {
	// генератор псевдослучайных чисел
	gen := rand.New(rand.NewSource(seed))

	return map[string]string{
		"E":      randLenStr(math.E, gen),
		"Pi":     randLenStr(math.Pi, gen),
		"Sqrt2":  randLenStr(math.Sqrt2, gen),
		"SqrtE":  randLenStr(math.SqrtE, gen),
		"SqrtPi": randLenStr(math.SqrtPi, gen),
		"Ln2":    randLenStr(math.Ln2, gen),
		"Ln10":   randLenStr(math.Ln10, gen),
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

Чтобы узнать количество цифр после десятичной точки, воспользуйтесь функцией `IndexRune()` пакета `strings` для того чтобы определить индекс точки в строке. Затем вычислите длину подстроки после точки. В этом вам поможет встроенная функция `len()`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"strconv"
	"strings"
)

func randLenStr(val float64, pseudoRandGen *rand.Rand) string {
	// дробная часть чисел не длиннее 10 знаков
	const prec = 10
	// значение в интервале [1, 11)
	fractionalPartLen := pseudoRandGen.Intn(prec) + 1
	strVal := strconv.FormatFloat(val, 'f', prec, 64)
	integerPartLen := strings.IndexRune(strVal, '.') + 1
	return strVal[:integerPartLen + fractionalPartLen]
}

func calculateConsts(seed int64) map[string]string {
	// генератор псевдослучайных чисел
	gen := rand.New(rand.NewSource(seed))

	return map[string]string{
		"E":      randLenStr(math.E, gen),
		"Pi":     randLenStr(math.Pi, gen),
		"Sqrt2":  randLenStr(math.Sqrt2, gen),
		"SqrtE":  randLenStr(math.SqrtE, gen),
		"SqrtPi": randLenStr(math.SqrtPi, gen),
		"Ln2":    randLenStr(math.Ln2, gen),
		"Ln10":   randLenStr(math.Ln10, gen),
	}
}

func chooseConsts(consts map[string]string) {
	const fractionalPartMinLen = 5

	var keysToDelete []string

	for key, val := range consts {
		pointIndex := strings.IndexRune(val, '.')
		if len(val[pointIndex + 1:]) < fractionalPartMinLen {
			keysToDelete = append(keysToDelete, key)
		}
	}

	for _, key := range keysToDelete {
		delete(consts, key)
	}
}

func main() {
	// аргумент можно менять для отладки 
	consts := calculateConsts(42) 
	fmt.Println(consts)
	chooseConsts(consts)
	fmt.Println(consts)
}
```

## Проверка наличия элемента в отображении
Когда вы обращаетесь к элементу отображения, то всегда получаете значение. Не важно, содержится элемент в отображении или просто имеет значение по умолчанию. В большинстве случаев это нормально, однако иногда нужно точно знать, содержит ли отображение данный элемент. На самом деле, когда вы обращаетесь к элементу отображения по ключу, то возвращается не только значение, но флаг. Он равен `true`, если элемент содержится в отображении и `false` — в противном случае. Часто его присваивают переменной с именем `ok`:

```go {.example_for_playground .example_for_playground_010}
catsWeights := make(map[string]int)
catsWeights["Barsik"] = 10
catsWeights["Busa"] = 2

if catsWeight, ok := catsWeights["Sharik"]; !ok {
	fmt.Println("no key Sharik in the catsWeights")
	fmt.Println("catsWeight =", catsWeight)
} 
```

Если переменная `ok` не нужна, то объявлять ее не следует:

```go  {.example_for_playground .example_for_playground_011}
catsWeight := catsWeights["Sharik"]
fmt.Println("catsWeight =", catsWeight)
```

Как и срезы, отображения нельзя сравнивать друг с другом. Отображение можно сравнить только со значением `nil`. Напишите тело функции `isEqual`, которая проверяет, содержат ли два среза типа `map[string]int` одинаковые ключи и связанные с ними значения. Функция `isEqual` возвращает `true`, если это так, и `false` — в противном случае. {.task_text}

```go {.task_source #golang_chapter_0060_task_0030}
package main

import "fmt"

func isEqual(x, y map[string]int) bool {
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

	fmt.Println(isEqual(catsWeights, dogsWeights))
}
```

Проверьте длины передаваемых отображений. Если длины не равны, то возвращаем `false`. В противном случае организуйте цикл по элементам отображения и сравните их. Учтите: когда вы обращаетесь к элементу отображения, то элемент может как содержать значение по умолчанию, так и вовсе отсутствовать в отображении. В обоих случаях вы получите значение по умолчанию. Две эти ситуации необходимо различать. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func isEqual(x, y map[string]int) bool {
	if len(x) != len(y) {
		return false
	}

	for key, xVal := range x {
		if yVal, ok := y[key]; !ok || yVal != xVal {
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

	fmt.Println(isEqual(catsWeights, dogsWeights))
}
```

## Реализация множеств
Во многих современных языках, например в C++ и Python, существуют множества — коллекции уникальных элементов. В Go множества отсутствуют, однако вместо них можно использовать отображения. Отображение хранит пары: уникальные ключи и привязанные к ним значения. Для решения реальных задач значения нужны не всегда. Зачастую достаточно хранить только ключи без значений.

Если создать отображение с ключом требуемого типа и значением типа пустой структуры `struct{}`, то мы получим отображение, имитирующее множество. В качестве значения всегда указывается `struct{}{}`. Пустая структура, во-первых, не занимает места. Во-вторых, она подчеркивает намерение: нам нужны ключи без значений. Попробуйте реализовать эту идею в задаче подсчета уникальных слов в тексте.

В переменной `s` типа `string` содержится некоторый текст. Допишите функцию `countUniqWords()`, которая возвращает количество уникальных слов в тексте. Словом будем считать любую последовательность символов, отделенную пробелами. {.task_text}

```go {.task_source #golang_chapter_0060_task_0040}
package main

import (
	"fmt"
	"strings"
)

func countUniqWords(s string) int {
	// ваш код здесь
}

func main() {
	s := "Go Rust Go C++ Lisp Lisp"

	fmt.Println(countUniqWords(s))
}
``` 

Создайте отображение типа `map[string]struct{}`. Добавляйте в цикле каждое слово в качестве ключа отображения, а в качестве его значения — `struct{}{}`. Таким образом, вы получите отображение с уникальными словами. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"strings"
)

func countUniqWords(s string) int {
	if len(s) == 0 {
		return 0
	}

	uniqWords := make(map[string]struct{})

	for _, word := range strings.Split(s, " ") {
		uniqWords[word] = struct{}{}
	}

	return len(uniqWords)
}

func main() {
	s := "Go Rust Go C++ Lisp Lisp"

	fmt.Println(countUniqWords(s))
}
```

## Резюме
1. В Go хеш-таблицы реализованы через тип `map` (отображение). Отображение — это ссылка на хеш-таблицу.
2. Отображения содержат пары «ключ — значение» с уникальными ключами.
3. При попытке узнать значение несуществующего элемента отображения через `[]` вы получите значение типа по умолчанию.
4. При обращении к элементу отображения через `[]` возвращается не только значение по ключу, но и флаг. Флаг позволяет понять, содержится ли искомый ключ в отображении.
5. Встроенная функция `delete` позволяет удалить элемент из отображения, `len` — узнать длину отображения, ключевое слово `range` — перебрать элементы отображения.
6. Порядок элементов отображения не определен.
7. В Go отсутствует тип `set` (множество уникальных элементов). На практике он заменяется отображением со значениями типа `struct{}`.