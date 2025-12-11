# Глава 15. Дженерики

## Дженерики-функции

Начиная с Go 1.18, в языке появились дженерики — **generics**. Дженерики позволяют писать обобщенный код, способный обрабатывать разные типы данных. Благодаря ним достаточно реализовать алгоритм лишь однажды. После этого его применяют к разным типам. 

Дженерики-функции — **generic functions** — легче понять на примере. Допустим, мы реализовали функцию `unique`, которая возвращает срез, состоящий из уникальных значений:

``` go {.example_for_playground}
package main

import "fmt"

func unique(slice []int) []int {
	seen := make(map[int]struct{})
	result := []int{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}

func main() {
	input := []int{1, 2, 3, 3, 4, 5}
	output := unique(input)
	fmt.Println(output)
}
```
```
[1 2 3 4 5]
```

Что делать, если мы хотим работать не только с `int`, но и, например, с `float64`? Мы могли бы решить повторить эту функцию для `float64`: 

``` go {.example_for_playground}
package main

import "fmt"

func unique(slice []int) []int {
	seen := make(map[int]struct{})
	result := []int{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}

func unique(slice []float64) []float64 {
	seen := make(map[float64]struct{})
	result := []float64{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}


func main() {
	fmt.Println(unique([]int{1, 2, 3, 3, 4, 5}))
	fmt.Println(unique([]float64{1.8, 2.2, 3.2, 3.2}))
}
```
```
cmd/main.go:20:6: unique redeclared in this block
```

Получаем ошибку компиляции: двух функций с одинаковым именем быть не может. Выходит, что мы должны еще и придумывать им разные имена! 

```go {.example_for_playground}
package main

import "fmt"

func uniqueInt(slice []int) []int {
	seen := make(map[int]struct{})
	result := []int{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}

func uniqueFloat64(slice []float64) []float64 {
	seen := make(map[float64]struct{})
	result := []float64{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}


func main() {
	fmt.Println(uniqueInt([]int{1, 2, 3, 3, 4, 5}))
	fmt.Println(uniqueFloat64([]float64{1.8, 2.2, 3.2, 3.2}))
}
```
```
[1 2 3 4 5]
[1.8 2.2 3.2]
```

А если нужны и `float32`? Придется каждый раз писать одинаковый код для разных типов. То есть копировать один и тот же алгоритм и заменять лишь типы данных, которые он обрабатывает. Более того, нужно именовать по смыслу одинаковые функции разными именами. Можно воспользоваться [кодогенерацией](https://go.dev/blog/generate) — переложить эту рутинную работу на компилятор. Но проще написать обобщенный код, используя дженерики:

``` go {.example_for_playground}
package main

import "fmt"

func unique[T int | float64](slice []T) []T {
	seen := make(map[T]struct{})
	result := []T{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}
func main() {
	input := []int{1, 2, 3, 3, 4, 5}
	output := unique(input)
	fmt.Println(output)
	anotherInput := []float64{1.4, 1.3, 1.4, 1.5, 2.7}
	anotherOutput := unique(anotherInput)
	fmt.Println(anotherOutput)
}
```
```
[1 2 3 4 5]
[1.4 1.3 1.5 2.7]
```

В квадратных скобках после имени функции описывается новый тип, с которым мы будем работать. В нашем случае `T` — имя этого типа. Его вы даете сами, но на практике чаще всего используют букву `T`. Типы, которые может принимать `T`, перечисляются через вертикальную черту `|`. После того, как мы описали новый тип, он становится доступен всюду внутри функции. Типов может быть несколько. В общем случае, синтаксис дженерика-функции такой: 

```
func имяФункции[тип1,тип2,...типN](параметры)(возвращемыеЗначения){
	тело
}
```

Конструкция `[тип1,тип2,...типN]` называется параметры-типы — **type parameters.**

Что делать, если мы хотим работать не только с `int` и `float64`, но и с другими сравнимыми типами? Например, `float32` или даже `string`. Мы могли бы воспользоваться интерфейсом `any`, чтобы передать любой тип в функцию. Но как понять, что элементы этого типа можно сравнивать? Мы могли бы перечислить все типы, с которыми мы можем работать явно, но это утомительно и громоздко. За нас это уже сделали в интерфейсе `comparable`.


Интерфейсы в Go бывают двух видов:

* Базовые — **basic** — определяются через список методов.
* Общие — **generic** — могут включать не только методы, но и типы.

Интерфейс `comparable` определяется через все сравнимые типы. Он является общим. Общие интерфейсы не могут быть использованы как обычные типы. Их применяют совместно с дженериками. Для нашего примера как раз подойдет такой интерфейс: 

``` go {.example_for_playground}
package main

import "fmt"

func unique[T comparable](slice []T) []T {
	seen := make(map[T]struct{})
	result := []T{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}

func main() {
	input := []int{1, 2, 3, 3, 4, 5}
	output := unique(input)
	fmt.Println(output)
	anotherInput := []float64{1.4, 1.3, 1.4, 1.5, 2.7}
	anotherOutput := unique(anotherInput)
	fmt.Println(anotherOutput)
	thirdInput := []string{"juice", "orange", "juice", "apple"}
	thirdOutput := unique(thirdInput)
	fmt.Println(thirdOutput)
}
```
```
[1 2 3 4 5]
[1.4 1.3 1.5 2.7]
[juice orange apple]
```

В качестве типа `T` мы использовали некоторый обобщенный тип `comparable`. Теперь мы можем работать со всеми сравнимыми типами.

Что выведет следующий код? В случае ошибки напишите error. {.task_text}

```go {.example_for_playground}
package main

import "fmt"

func unique[T comparable](slice []T) []T {
	seen := make(map[T]struct{})
	result := []T{}

	for _, item := range slice {
		if _, ok := seen[item]; !ok {
			seen[item] = struct{}{}
			result = append(result, item)
		}
	}
	return result
}

func main() {
	input := [][]int{{1, 2, 3}, {1, 2, 3}, {1, 4, 6}}
	output := unique(input)
	fmt.Println(output)
}
```

```consoleoutput {.task_source #golang_chapter_0150_task_0010}
```
Типу `comparable` соответствуют только сравнимые типы. {.task_hint}

```{.task_answer}
error
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
func Filter(slice []any,
	predicate func(any) bool) []any {
	var result []any
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}

func main() {
	slice := []string{"anton@yandex.ru",
		"https://go.dev/", "go@best@hackers"}
	newSlice := Filter(slice, valid)
	fmt.Println(newSlice)
	consts := []float64{3.141592653589, 2.718281828}
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

func Filter[T any](slice []T,
	predicate func(T) bool) []T {
	var result []T
	for _, v := range slice {
		if predicate(v) {
			result = append(result, v)
		}
	}
	return result
}
func main() {
	slice := []string{"anton@yandex.ru",
		"https://go.dev/", "go@best@hackers"}
	newSlice := Filter(slice, valid)
	fmt.Println(newSlice)
	consts := []float64{3.141592653589, 2.718281828}
	newConsts := Filter(consts, func(p float64) bool {
		return p < 3
	})
	fmt.Println(newConsts)
}
```

## Дженерики-типы 

Когда в структуре есть поля, в которых необходимо хранить данные различных типов, бывает удобно использовать дженерики-типы — **generic types**:
 
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

Как уже было сказано, ничто не мешает использовать в дженериках более одного типа: 

```go {.example_for_playground}
package main

import "fmt"

type Pair[T any, U any] struct {
	First  T
	Second U
}

func NewPair[T any, U any](first T, second U) Pair[T, U] {
	return Pair[T, U]{First: first, Second: second}
}

func main() {
	nameAge := NewPair("Alice", 30)
	fmt.Println("Name and age:", nameAge)

	coordinates := NewPair(10, 20.5)
	fmt.Println("Coordinates:", coordinates)
}
```
```
Name and age: {Alice 30}
Coordinates: {10 20.5}
```

Кеш `Cache` позволяет хранить данные в хеш-таблице с ключами типа `string` и значениями типа `cacheItem`. Поле `ttl` структуры `Cache` расшифровывается как *time to live* — время жизни. Его тип — `time.Duration`, псевдоним для `int64`. Оно хранит количество наносекунд, в течение которых кеш актуален. {.task_text}

Поле `value` из `cacheItem` хранит само значение типа `int`, а `expiresAt` — время, после которого значение перестает быть актуальным. Тип `expiresAt` — `time.Time` — структура, которая хранит конкретное время. {.task_text}

Функция `NewCache` возвращает указатель на вновь созданный кеш. Функция `Set` позволяет установить значение кеша по его ключу. Функция `Get` возвращает значение кеша по его ключу и признак того, что актуальное значение присутствует в нем. Функция `Delete` удаляет значение кеша по его ключу. {.task_text}

Модифицируйте код над функцией `main` таким образом, чтобы в качестве ключей кеша можно было использовать любой сравнимый тип. В качестве значений кеша также должны выступать данные любого типа, необязательного сравнимого. {.task_text}

В результате ваших модификаций код из `main` должен выполнится без ошибок. {.task_text}

```go {.task_source #golang_chapter_0150_task_0030}
package main

import (
	"fmt"
	"time"
)

type Cache struct {
	data map[string]cacheItem
	ttl  time.Duration
}

type cacheItem struct {
	value     int
	expiresAt time.Time
}

func NewCache(ttl time.Duration) *Cache {
	return &Cache{
		data: make(map[string]cacheItem),
		ttl:  ttl,
	}
}

func (c *Cache) Set(key string, value int) {
	c.data[key] = cacheItem{
		value:     value,
		expiresAt: time.Now().Add(c.ttl),
	}
}

func (c *Cache) Get(key string) (int, bool) {
	item, exists := c.data[key]
	if !exists || time.Now().After(item.expiresAt) {
		return 0, false
	}
	return item.value, true
}

func (c *Cache) Delete(key string) {
	delete(c.data, key)
}

func main() {
	// Кеш для числовых значений
	intCache := NewCache[string, int](10 * time.Minute)
	intCache.Set("request_count", 42)
	intCache.Set("active_users", 150)

	if count, ok := intCache.Get("request_count"); ok {
		fmt.Printf("Requests: %d\n", count)
	}

	intCache.Delete("request_count")
	intCache.Delete("active_users")

	// Кеш для структур
	type User struct {
		Name  string
		Email string
	}

	userCache := NewCache[int, User](time.Hour)
	userCache.Set(1, User{Name: "Bob", Email: "bob@example.com"})
	userCache.Set(2, User{Name: "Charlie", Email: "charlie@test.com"})

	if user, ok := userCache.Get(1); ok {
		fmt.Printf("User: %+v\n", user)
	}

	userCache.Delete(1)
	userCache.Delete(2)
}
```

Используйте дженерики-типы и дженерики-функции. В качестве типа ключей будет выступать `comparable`, а в качестве типа значений — `any`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"time"
)

type Cache[K comparable, V any] struct {
	data map[K]cacheItem[V]
	ttl  time.Duration
}

type cacheItem[V any] struct {
	value     V
	expiresAt time.Time
}

func NewCache[K comparable, V any](ttl time.Duration) *Cache[K, V] {
	return &Cache[K, V]{
		data: make(map[K]cacheItem[V]),
		ttl:  ttl,
	}
}

func (c *Cache[K, V]) Set(key K, value V) {
	c.data[key] = cacheItem[V]{
		value:     value,
		expiresAt: time.Now().Add(c.ttl),
	}
}

func (c *Cache[K, V]) Get(key K) (V, bool) {
	item, exists := c.data[key]
	if !exists || time.Now().After(item.expiresAt) {
		var zero V
		return zero, false
	}

	return item.value, true
}

func (c *Cache[K, V]) Delete(key K) {
	delete(c.data, key)
}

func main() {
	// Кеш для числовых значений
	intCache := NewCache[string, int](10 * time.Minute)
	intCache.Set("request_count", 42)
	intCache.Set("active_users", 150)

	if count, ok := intCache.Get("request_count"); ok {
		fmt.Printf("Requests: %d\n", count)
	}

	intCache.Delete("request_count")
	intCache.Delete("active_users")

	// Кеш для структур
	type User struct {
		Name  string
		Email string
	}

	userCache := NewCache[int, User](time.Hour)
	userCache.Set(1, User{Name: "Bob", Email: "bob@example.com"})
	userCache.Set(2, User{Name: "Charlie", Email: "charlie@test.com"})

	if user, ok := userCache.Get(1); ok {
		fmt.Printf("User: %+v\n", user)
	}

	userCache.Delete(1)
	userCache.Delete(2)
}
```

## Конструкция с тильдой

Следующий пример подсчитывает количество заранее заданных лексем. Обратите внимание, что лексемы представлены строками. Функция `wordCounter` также работает со строками.

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

Что, если мы используем псевдоним `lexeme` для типа `string`? Функция `wordCounter` перестанет работать, потому что мы умеем обрабатывать только строки.

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

Получаем ошибку компиляции. Но ведь по смыслу ничего не изменилось! Псевдоним `lexeme` — это все тот же `string`, только с другим названием. Чтобы функция принимала не только базовый тип, но и его псевдонимы, удобно применить дженерик с символом тильды `~`. Достаточно просто поставить символ тильды перед базовым типом при перечислении параметров-типов: 

```go
func wordCounter[T ~string]() func(word T) map[T]int {
	...
}
```

Для встроенных типов базовым типом является сам тип. Например, для `float64` — это `float64`. Для составных типов базовый тип определяется по их структуре. Например, для `type Point struct { x int }` базовый тип — `struct{ x int }`. Для псевдонимов базовый тип — это тот тип, для которого объявлен псевдоним. В нашем случае базовый тип `lexeme` — `string`.

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

Реализуйте функцию-обертку `hasPrefix` над встроенной `strings.HasPrefix`. Первый ее аргумент — текст, второй — префикс. Она  должна проверить, содержит ли текст соответствующий префикс. Функция возвращает `true`, если содержит, и `false` — в противном случае. В качестве типов первого и второго аргумента функция должна принимать все псевдонимы типа `string`. Эти псевдонимы необязательно должны быть одинаковыми.  {.task_text}

```go {.task_source #golang_chapter_0150_task_0040}
package main

import (
	"fmt"
	"strings"
)

// ваш код здесь 

func main() {
	type Username string
	var user1 Username = "angel336"
	type Prefix string
	var p Prefix = "angel"
	fmt.Println(hasPrefix(user1, p))
}
```

Используйте конструкции с тильдой. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"strings"
)

func hasPrefix[T ~string,
	T2 ~string](s T, prefix T2) bool {
	return strings.hasPrefix(string(s), string(prefix))
}

func main() {
	type Username string
	var user1 Username = "angel336"
	type Prefix string
	var p Prefix = "angel"
	fmt.Println(hasPrefix(user1, p))
}
```
## Резюме 

1. Дженерики бывают удобны, когда необходимо выполнять одинаковые действия над различными типами.
2. В Go существуют базовые — **basic**, и  общие — **generic** интерфейсы. Общие интерфейсы могут определяться не только через набор методов, но и через конкретные типы. Они используются только в дженериках. 
3. Дженерики используют не только с функциями, но и со структурами. 
4. Тильда рядом с базовым типом означает, что дженерик работает не только с этим типом. Он работает со всеми его псевдонимами.
