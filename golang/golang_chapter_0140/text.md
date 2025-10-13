# Глава 14. Некоторые приемы по работе с интерфейсами
## Сравнение интерфейсов
Интерфейсы сравнимы, если сравнимы их конкретные типы: 
```go {.example_for_playground}
package main

import "fmt"

func main() {
	var a interface{} = 10
	var b interface{} = 20
	fmt.Println(a == b)
}
``` 
```
false
```

Что выведет следующий код? В случае ошибки напишите `error`. {.task_text}

```go {.example_for_playground}
package main

import "fmt"

type musicTrack struct {
	id     int
	name   string
	author string
	albums []int
}

func (m musicTrack) String() string {
	return fmt.Sprintf("%s: %s", m.author, m.name)
}

func main() {
	var m fmt.Stringer = musicTrack{10,
		"Come Together",
		"The Beatles",
		[]int{1, 2, 3},
	}
	var m2 fmt.Stringer = musicTrack{10,
		"Come Together",
		"The Beatles",
		[]int{2, 3},
	}
	fmt.Println(m == m2)
}
```

```consoleoutput {.task_source #golang_chapter_0140_task_0010}
```
Вспомните, как сравниваются срезы. {.task_hint}
```go {.task_answer}
error
```

## Сортировка
На практике нам часто приходится сталкиваться с тем, чтобы отсортитровать какие-либо данные. Например, при выдаче списка файлов в папке мы можем отсортировать его по имени, дате или размеру файла. В Go для того чтобы организовать сортировку данных, удобно использовать `sort.Interface`.

Функция `Sort` пакета `sort` умеет сортировать переменные типа `sort.Interface`. Чтобы тип удовлетворял `sort.Interface`, необходимо реализовать три метода:

```go
type Interface interface {
	// Len возвращает количество элементов коллекции.
	Len() int

	// Less сообщает, должен ли элемент 
	// с индексом i сортироваться
    // перед элементом с индесом j
	Less(i, j int) bool

	// Swap меняет местами 
	// элементы с индексами i и j
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
	Interface
}

func (r reverse) Less(i, j int) bool {
	return r.Interface.Less(j, i)
}

func Reverse(data Interface) Interface {
	return &reverse{data}
}
```

Память `memory` выделяется частями `chunk` по 16 значений типа `int`. Вся выделенная память представляется в виде односвязного списка `linkedList`, в котором каждое значение представлено в виде `memory`. Каждый чанк имеет `id`. Отсортируйте выделенную память по возрастанию `id` чанков.  {.task_text}


```go {.task_source #golang_chapter_0140_task_0020}
package main

import (
	"fmt"
	"sort"
)

type memory struct {
	id    int
	chunk [16]int
}

type linkedNode struct {
	next  *linkedNode
	value memory
}

type linkedList struct {
	begin *linkedNode
}

func (list *linkedList) print() {
	node := list.begin
	for node != nil {
		fmt.Print(node.value)
		fmt.Print("->")
		node = node.next
	}
	fmt.Print("nil")
}

func exampleList() *linkedList {
	mem0 := memory{2, [16]int{1, 2, 3}}
	mem1 := memory{3, [16]int{0, 0, 4, 5, 6}}
	mem2 := memory{1, [16]int{7, 0, 8}}
	mem3 := memory{0, [16]int{9, 10}}
	return &linkedList{&linkedNode{
		&linkedNode{
			&linkedNode{
				&linkedNode{nil,
					mem3}, mem2}, mem1}, mem0}}
}

func main() {
	lst := exampleList()
	lst.print()
	fmt.Println()
	sort.Sort(lst)
	lst.print()
	fmt.Println()
}

```

Чтобы отсортировать односвязный список, достаточно изменить адреса соответствующих указателей. {.task_hint}

```go  {.task_answer}
package main

import (
	"fmt"
	"sort"
)

const errNilNode = "one of the nodes is nil"

type memory struct {
	id    int
	chunk [16]int
}

type linkedNode struct {
	next  *linkedNode
	value memory
}

type linkedList struct {
	begin *linkedNode
}

func (list *linkedList) Len() int {
	n := list.begin
	counter := 0

	for n != nil {
		counter++
		n = n.next
	}
	return counter
}

func (list *linkedList) Less(i, j int) bool {
	iNode := list.numberNode(i)

	if iNode == nil {
		panic(errNilNode)
	}
	jNode := list.numberNode(j)
	if jNode == nil {
		panic(errNilNode)
	}
	return iNode.value.id < jNode.value.id
}

func (list *linkedList) Swap(i, j int) {
	iNode := list.numberNode(i)
	
	if iNode == nil {
		panic(errNilNode)
	}

	jNode := list.numberNode(j)
	if jNode == nil {
		panic(errNilNode)
	}

	var iPrev *linkedNode = nil
	var jPrev *linkedNode = nil

	if i-1 >= 0 {
		iPrev = list.numberNode(i - 1)
	}
	if j-1 >= 0 {
		jPrev = list.numberNode(j - 1)
	}

	if iPrev != nil {
		iPrev.next = jNode
	}
	if jPrev != nil {
		jPrev.next = iNode
	}
	iNode.next, jNode.next = jNode.next, iNode.next

	if i == 0 {
		list.begin = jNode
	}
	if j == 0 {
		list.begin = iNode
	}
}

func (list *linkedList) numberNode(number int) *linkedNode {
	if list.begin == nil || number < 0 {
		return nil
	}

	inode := list.begin

	for k := 0; k < number; k++ {
		inode = inode.next
		if inode == nil {
			return nil
		}
	}
	return inode
}

func (list *linkedList) print() {
	node := list.begin
	for node != nil {
		fmt.Print(node.value)
		fmt.Print("->")
		node = node.next
	}
	fmt.Print("nil")
}

func exampleList() *linkedList {
	mem0 := memory{2, [16]int{1, 2, 3}}
	mem1 := memory{3, [16]int{0, 0, 4, 5, 6}}
	mem2 := memory{1, [16]int{7, 0, 8}}
	mem3 := memory{0, [16]int{9, 10}}
	return &linkedList{&linkedNode{
		&linkedNode{
			&linkedNode{
				&linkedNode{nil,
					mem3}, mem2}, mem1}, mem0}}
}

func main() {
	lst := exampleList()
	lst.print()
	fmt.Println()
	sort.Sort(lst)
	lst.print()
	fmt.Println()
}
```
## Пользовательские ошибки
Необязательно использовать ошибки такими, какие они даются из коробки. Иногда полезно создать пользовательскую ошибку. Например, когда мы используем значения ошибок, которые нигде не меняются. Сделать переменную типа `error` константой невозможно:
```go {.example_for_playground}
package main

import (
	"errors"
	"fmt"
)

func main() {
	const err = errors.New("some error")
	fmt.Println(err)
}
```
```
./main.go:9:14: errors.New("some error") (value of interface type error) is not constant (exit status 1)
```

Однако выход из этой ситуации есть. Тип `error` является интерфейсом, который требует реализовать единственный метод:
```go
type error interface {
	Error() string
}
``` 

Сделаем псеводним для типа `string`, а затем реализуем этот метод для него: 
```go
package main

type customErr string

func (e customErr) Error() string {
	return string(e)
}

func main() {
	const customErr = customErr("some error")
	customErr = "new error"
}
```
```
./main.go:11:2: cannot assign to customErr (neither addressable nor a map index expression) (exit status 1)
```
Теперь можем работать с ошибкой-константой. Попытка изменить константу приводит к ошибке компиляции.

Пользователи приложения могут использовать различные поисковые движки. Когда пользователь использует поисковой движок, для него вызывается соответствующий метод. Например, `yandexSearch` или `googleSearch`. Они возвращают пользовательскую ошибку `customError`. Реализуйте `customError` так, чтобы ошибка представляла собой структуру. Первым полем этой структуры должен быть `userId` типа `int` — идентификатор пользователя, вторым — сообщение об ошибке `message` типа `string`. При формировании строки с ошибкой используйте следующий формат: {.task_text}
```
<id пользователя> : <сообщение об ошибке> {.task_text}
```
Например: 
```
3 : failed to log in
```
```go {.task_source #golang_chapter_0140_task_0030}
package main

import "fmt"

type appUser struct {
	id    int
	login string
}

func (a appUser) yandexSearch() error {
	// некоторая логика
	// ...
	return customErr{a.id, "no internet"}
}

func (a appUser) googleSearch() error {
	// некоторая логика
	// ...
	return customErr{a.id, "failed to log in"}
}

func main() {
	user := appUser{3, "buba"}
	err := user.yandexSearch()
	if err != nil {
		fmt.Println(err)
	}
	err = user.googleSearch()
	if err != nil {
		fmt.Println(err)
	}
}
```

Создайте структуру `customErr` и реализуйте метод `Error() string` для получателя типа `customErr`. {.task_hint}

```go  {.task_answer}
package main

import "fmt"

type customErr struct {
	userId  int
	message string
}

func (e customErr) Error() string {
	return fmt.Sprintf("%d : %s", e.userId, e.message)
}

type appUser struct {
	id    int
	login string
}

func (a appUser) yandexSearch() error {
	// некоторая логика
	// ...
	return customErr{a.id, "no internet"}
}

func (a appUser) googleSearch() error {
	// некоторая логика
	// ...
	return customErr{a.id, "failed to log in"}
}

func main() {
	user := appUser{3, "buba"}
	err := user.yandexSearch()
	if err != nil {
		fmt.Println(err)
	}
	err = user.googleSearch()
	if err != nil {
		fmt.Println(err)
	}
}
```
## Декларация типов 
Рассмотрим следующий код: 
```go {.example_for_playground}
package main

import (
	"fmt"
)

type appUser struct {
	id    int
	login string
}

type yandexUser struct {
	appUser
}
type googleUser struct {
	appUser
}

type searcher interface {
	search()
}

func (y yandexUser) String() string {
	return fmt.Sprintf("%d : %s", y.id, y.login)
}

func (g googleUser) String() string {
	return fmt.Sprintf("%d : %s", g.id, g.login)
}

func (y yandexUser) search() {
	fmt.Printf("searching for yandex, %d : %s...",
		y.id, y.login)
}
func (g googleUser) search() {
	fmt.Printf("searching for google, %d : %s...",
		g.id, g.login)
}

func main() {
	var s fmt.Stringer
	s = yandexUser{appUser{3, "buba"}}
	fmt.Println(s)
	s.search()
}
```
```
./main.go:43:4: s.search undefined (type fmt.Stringer has no field or method search) (exit status 1)
```

В функции `main` мы создали переменную `s` типа `fmt.Stringer` и присвоили ей значение типа `yandexUser`. Тип `yandexUser` удовлетворяет интерфейсу `fmt.Stringer`, поскольку реализует метод `String() string`. Мы можем вывести содержимое этой переменной в желаемом формате на экран. 

Однако, что если теперь мы захотим выполнить метод `search()` для переменной `s`? Фактически в ней хранится переменная типа `yandexUser`, однако компилятор не даст нам выполнить метод. Ведь мы объявили переменную `s` с типом `fmt.Stringer`. На помощь приходит **декларация типов**:

```go
func main() {
	var s fmt.Stringer
	s = yandexUser{appUser{3, "buba"}}
	fmt.Println(s)
	s.(yandexUser).search() // декларация типа
}
```
```
3 : buba
searching for yandex, 3 : buba...
```

Деклариция типа позволяет сказать компилятору В случае, когда 
## Интерфейс с нулевым указателем

Этот раздел рассказывает про одно неочевидное поведение компилятора Go. Интерфейс с нулевым указателем не нулевой. Чтобы лучше это понять, обратимся к следующему примеру.

```go {.example_for_playground}
package main

import (
	"errors"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type filmT struct {
	id           int
	name         string
	director     string
	screenwriter string
}

type musicTrackT struct {
	id     int
	name   string
	author string
	albums []int
}

type mediaReader interface {
	io.Reader
}

func (f *filmT) Read(p []byte) (n int, err error) {
	values := strings.Split(string(p), ",")
	if len(values) != 4 {
		return 0, errors.New("invalid format")
	}
	id, err := strconv.Atoi(values[0])
	if err != nil {
		// fmt.Errorf возвращает ошибку в нужном формате
		return 0, fmt.Errorf("invalid format: %s", err.Error())
	}
	f.id = id

	// убираем все пробельные символы в начале и в конце
	f.name = strings.TrimSpace(values[1])
	f.director = strings.TrimSpace(values[2])
	f.screenwriter = strings.TrimSpace(values[3])
	return len(p), nil
}

func (m *musicTrackT) Read(p []byte) (n int, err error) {
	// некоторая логика
	// ...
	return 0, nil
}

func AddNewMedia(m mediaReader,
	media []mediaReader) []mediaReader {
	if m != nil {
		return append(media, m)
	}
	return media
}

func main() {
	var film *filmT = &filmT{}
	_, err := film.Read([]byte("1, Forrest Gump, Robert Zemeckis, Eric Roth"))
	if err != nil {
		panic(err)
	}
	var media []mediaReader
	media = AddNewMedia(film, media)
	var musicTrack *musicTrackT
	media = AddNewMedia(musicTrack, media)
	fmt.Println(media)
}
```
```
[0xc00010e0c0 <nil>]
```
Внутри `AddNewMedia` мы проверяем переменную `m` на `nil`. Переменная `musicTrack`, переданная в `m` в функции `main`, является нулевым указателем. Однако сама `m` не ялвяется `nil`. В итоге получаем, что в срезе `media` лежит `nil`. Очевидно, это не то, что мы хотели.

Проблема решается использованием всюду типа `mediaReader`. Перепешим код из `main`, подставив в качестве типа для `film` и `musicTrack` — `mediaReader`. 
```go
func main() {
	var film mediaReader = &filmT{}
	_, err := film.Read([]byte("1, Forrest Gump, Robert Zemeckis, Eric Roth"))
	if err != nil {
		panic(err)
	}
	var media []mediaReader
	media = AddNewMedia(film, media)
	var musicTrack mediaReader
	media = AddNewMedia(musicTrack, media)
	fmt.Println(media)
	fmt.Println(media[0])
}
```
```
[0xc00010e0c0]
&{1 Forrest Gump Robert Zemeckis Eric Roth}
```