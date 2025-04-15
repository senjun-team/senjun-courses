# Глава 8. Функции как объекты

## Значения-функции
Функции, подобно другим значениям, присваивают переменным: 
```go {.example_for_playground}
package main

import "fmt"

func hello(user string) {
    fmt.Printf("Hello %s!\n", user)
}

func goodbuy(user string) {
    fmt.Printf("Goodbuy %s!\n", user)
}

func main() {
    userMessage := hello
    userMessage("gopher")
    userMessage = goodbuy
    userMessage("gopher")
}
```

В данном случае `userMessage` имеет тип `func(string)`. Попытка присвоить в переменную `userMessage` функцию с другой сигнатурой приведет к ошибке компиляции:

```go {.example_for_playground}
package main

import (
    "fmt"
    "net/mail"
)

func hello(user string) {
    fmt.Printf("Hello %s!\n", user)
}

func goodbuy(user string) {
    fmt.Printf("Goodbuy %s!\n", user)
}

func valid(email string) bool {
    _, err := mail.ParseAddress(email)
    return err == nil
}

func main() {
    userMessage := hello
    userMessage("gopher")
    userMessage = goodbuy
    userMessage("gopher")
    // нельзя присвоить значение типа func(string) bool
    // переменной типа func(string)
    userMessage = valid // ошибка компиляции! 
}
```

По умолчанию переменная типа функции содержит значение `nil`. Вызов функции со значением `nil` приведет к панике:
```go {.example_for_playground}
package main

import "fmt"

func main() {
    var f func() string
    fmt.Println(f())
}
```

Если вы не уверены в том, что функция не нулевая, то перед вызовом ее нужно сравнить со значением `nil`:
```go {.example_for_playground}
package main

import "fmt"

func main() {
    var f func() string
    if f != nil {
        fmt.Println(f())
    }
}
```

Сами значения-функции не сравнимы друг с другом, поэтому, например, их нельзя использовать в качестве ключей хеш-таблицы.

Иногда, чтобы избежать множественного ветвления, бывает удобно вместо `switch-case` использовать срез функций, каждая из которых возвращает `bool`. Если условие внутри функции выполнилось, то она вернет `true`, иначе — `false`. Перебирая функции внутри этого среза, можно реализовать ветвление без использования `switch-case`. Попробуйте реализовать эту идею в данной задаче. {.task_text}  

Функции `Sun`, `Rain` и `Snow` предсказывают солнце, дождь и снег соответственно, в зависимости от города. Например, если в городе идет снег, то функция `Snow` печатает сообщение об этом и возвращает `true`. В противном случае она ничего не печатает и возвращает `false`. Реализуйте функцию `predictWeather`, которая принимает на вход название города и печатает погоду в нем. Если погода неизвестна, то напечатайте `Unknown`. {.task_text}
```go {.task_source #golang_chapter_0080_task_0010}
package main

import "fmt"

func main() {
	predictWeather("London")
}

func predictWeather(city string) {
	// ваш код здесь 
}

func Sun(city string) bool {
	if city == "Moscow" {
		fmt.Println("Sun")
		return true
	}
	return false
}

func Rain(city string) bool {
	if city == "London" {
		fmt.Println("Rain")
		return true
	}
	return false
}

func Snow(city string) bool {
	if city == "Novosibirsk" {
		fmt.Println("Snow")
		return true
	}
	return false
}
```

Создайте срез из функций Sun, Rain и Snow. Пройдите в цикле по этому срезу и, если функция вернула `true`, выйдите из него. Если ни одна из функций не вернула `true`, значит печатаем `Unknown`. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func main() {
	predictWeather("London")
}

func predictWeather(city string) {
	weather := []func(string) bool{Sun, Rain, Snow}
	var isFound bool

	for _, f := range weather {
		if f(city) {
			isFound = true
			break
		}
	}

	if !isFound {
		fmt.Println("Unknown")
	}
}

func Sun(city string) bool {
	if city == "Moscow" {
		fmt.Println("Sun")
		return true
	}
	return false
}

func Rain(city string) bool {
	if city == "London" {
		fmt.Println("Rain")
		return true
	}
	return false
}

func Snow(city string) bool {
	if city == "Novosibirsk" {
		fmt.Println("Snow")
		return true
	}
	return false
}
``` 

## Анонимные функции
Анонимные функции — это функции без имени:

```go {.example_for_playground}
package main

import "fmt"

func main() {
    func() {
        fmt.Println("Hello from anonymous world!")
    }()
}
```

В данном случае мы создали анонимную функцию внутри `main` и вызвали ее через символы скобочек `()`. Если нужно использовать анонимную функцию в другом месте, то ее присваивают переменной: 

```go {.example_for_playground}
package main

import "fmt"

func main() {
    const sep = "============================"
    hello := func() {
        fmt.Println("Hello from anonymous world!")
    }
    fmt.Println(sep)
    hello()
    fmt.Println(sep)
}
```

Вот так передают анонимную функцию в качестве параметра другой функции: 

```go {.example_for_playground}
package main

import "fmt"

func main() {
    const sep = "==================================="
    startServer(sep, "127.0.0.1", "8080", []string{"192.168.23.48:4040", "192.168.23.48:6060"},
        func(host string, port string) {
            fmt.Printf("Hello from %s:%s!\n", host, port)
        })
}

func startServer(sep string, servHost string, servPort string,
    clients []string,
    helloFrom func(host string, port string)) {
    for _, client := range clients {
        fmt.Println(sep)
        fmt.Printf("%s:%s-->%s\n", servHost, servPort, client)
        helloFrom(servHost, servPort)
    }
    fmt.Println(sep)
}
```

Функция initGame инициализирует двумерную игру. Она принимает переменную типа функции `hero`, которая должна возвращать местоположение героя и символа, которым он обозначен на игровой карте. Передайте функции `initGame` в качестве параметра анонимную функцию, которая возвращает местоположение `1, 2` и значок `^`. Напечатайте результат `res` игровой карты с героем, который вернет функция `initGame`, на экран. {.task_text}

```go {.task_source #golang_chapter_0080_task_0020}
package main

import (
	"errors"
	"fmt"
)

func main() {
	// ваш код здесь 
}

func initGame(hero func() ([2]int, string)) (res string, err error) {
	rows := 3
	cols := 4

	pos, h := hero()

	if pos[0] < 0 || pos[0] >= rows || pos[1] < 0 || pos[1] >= cols {
		return "", errors.New("hero out of map")
	}

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			if i == pos[0] && j == pos[1] {
				res += h
			} else {
				res += "*"
			}
		}
		res += "\n"
	}

	return res, nil
}
```

Не забудьте обработать ошибку, которую возвращает функция `initGame`. Напечатайте ее на экран в случае, если ошибка произойдет. В случае, если ошибка не произойдет, напечатайте на экран результат `res`. {.task_hint}

```go {.task_answer}
package main

import (
	"errors"
	"fmt"
)

func main() {
	g, err := initGame(func() ([2]int, string) { return [2]int{1, 2}, "^" })
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Println(g)
	}
}

func initGame(hero func() ([2]int, string)) (res string, err error) {
	rows := 3
	cols := 4

	pos, h := hero()

	if pos[0] < 0 || pos[0] >= rows || pos[1] < 0 || pos[1] >= cols {
		return "", errors.New("hero out of map")
	}

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			if i == pos[0] && j == pos[1] {
				res += h
			} else {
				res += "*"
			}
		}
		res += "\n"
	}

	return res, nil
}
```
## Захват переменных значениями-функциями 
До версии Go 1.22 переменные, объявленные в цикле `for`, создавались при входе в цикл и обновлялись на каждой итерации. Это приводило к ошибкам при использовании таких переменных в замыканиях:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	users := make(map[int]string)
	var makeUsers []func(userName string)
	const usersNumber = 5

	for idx := 0; idx < usersNumber; idx++ {
		makeUsers = append(makeUsers, func(userName string) {
			users[idx] = userName
		})
	}

	userNames := []string{"Ivanov", "Petrov", "Sidorov", "Nikolaeva", "Avgustinov"}

	for i, makeUser := range makeUsers {
		makeUser(userNames[i])
	}
	fmt.Println(users)
}
```

Вероятно, вы ожидаете, что код выведет хеш-таблицу `map[0:Ivanov 1:Petrov 2:Sidorov 3:Nikolaeva 4:Avgustinov]`. Это может показаться удивительным, но программа, собранная компилятором Go до версии 1.22, выведет `map[5:Avgustinov]`.
Дело в том, что значения-функции в данном случае «захватывают» переменную `idx` и совместно используют адрес этой переменной. 

Чтобы избежать этого, раньше необходимо было создавать еще одну переменную внутри цикла. Чаще всего делали переменную с тем же именем:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	users := make(map[int]string)
	var makeUsers []func(userName string)
	const usersNumber = 5

	for idx := 0; idx < usersNumber; idx++ {
		idx := idx // нужно, чтобы избежать «захвата» переменной!
		makeUsers = append(makeUsers, func(userName string) {
			users[idx] = userName
		})
	}

	userNames := []string{"Ivanov", "Petrov", "Sidorov", "Nikolaeva", "Avgustinov"}

	for i, makeUser := range makeUsers {
		makeUser(userNames[i])
	}
	fmt.Println(users)
}
```

Начиная с версии Go 1.22, такой проблемы больше нет. Теперь переменные, объявленные в цикле с использованием `:=`, являются разными экземплярами на каждой итерации. Последующие ссылки на них видят значения на момент итерации, в которой они были объявлены, а не значения на момент более поздней итерации. Изначальный код выводит `map[0:Ivanov 1:Petrov 2:Sidorov 3:Nikolaeva 4:Avgustinov]`, как и ожидается. 

## Замыкания 
Когда мы возвращаем некоторую функцию из другой функции, нам может потребоваться запомнить внешнее значение. Когда мы вернем эту функцию снова, то мы воспользуемся старым значением.  Говоря иначе, некоторые функции имеют *состояние*. Такой прием предоставляют нам *замыкания (closures)*. Мы как бы замыкаем в себя контекст. Следующая программа вычисляет числа Фибоначчи с использованием замыкания:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	fib := nextFibonacci()
	for i := 0; i < 10; i++ {
		fmt.Printf("%d\t", fib())
	}
}

func nextFibonacci() func() int {
	firstNumber := -1
	secondNumber := 1

	return func() int {
		resNumber := firstNumber + secondNumber
		firstNumber = secondNumber
		secondNumber = resNumber
		return resNumber
	}
}
```

При каждом вызове `fib()` мы помним о предыдущих значениях `firstNumber`и `secondNumber`.

Замыкания часто используют, когда необходимо написать промежуточную логику `middleware`, при вызове обработчика сервера:  

```go
package main

import (
	"fmt"
	"net/http"
)

// основная логика обработчика 
func logic(w http.ResponseWriter, r *http.Request) {
	fmt.Println("Execute the logic")
}
func main() {
	handlerLogic := http.HandlerFunc(logic)
	mainHandler := middleware(handlerLogic)
	mainHandler.ServeHTTP(nil, nil)
}

// промежуточная логика 
func middleware(handler http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Println("Middleware execution before request")
		handler.ServeHTTP(w, r)
		fmt.Println("Middleware execution after response")
	})
}
```
Обработчик сервера — это тот код, который выполняется при переходе по конкретному адресу. Вызов `mainHandler.ServeHTTP(nil, nil)` запускает наш обработчик. Однако мы использовали замыкание и обернули основную логику обработки `logic` в `middleware`. Теперь при запуске обработчика вызовется `middlware`, в котором мы производим нужные действия до и после основной логики. 

Следующий код показывает еще один способ применения замыканий:

```go {.example_for_playground}
package main

import "fmt"

func receiver(protocol string) func(client string) string {
	return func(client string) string {
		return fmt.Sprintf("Getting mail via %s with clients %s", protocol, client)
	}
}

func main() {
	receive_via_imap := receiver("IMAP")
	receive_via_pop3 := receiver("POP3")

	// Getting mail via IMAP with client Thunderbird
	fmt.Println(receive_via_imap("Thunderbird"))

	// Getting mail via POP3 with client kmail
	fmt.Println(receive_via_pop3("kmail"))

} 
```

Здесь мы используем замыкание, чтобы создать получатель `receiver` по одному из известных протоколов. Создав получателей по протоколам IMAP и POP3, мы можем получать сообщения через программы, которые работают по этим протоколам.

Реализовано три функции: `verySlowFunc()`, `slowFunc()` и `fastFunc()`. Напишите функцию `bestFunc()`, которая принимает на вход одну из этих функций и ее псевдоним, а возвращает псевдоним наиболее быстрой функции из тех, что были переданы ранее. Так, код ниже должен вывести три значения: `firstFunc`, `firstFunc`, `thirdFunc`. Для того, чтобы узнать время выполнения функции в миллисекундах, воспользуйтесь стандартным пакетом `time`. Запомните текущее время с помощью `start := time.Now()`, а затем посчитайте количество миллисекунд с этой момента: `time.Since(start).Milliseconds()`.{.task_text}

```go {.task_source #golang_chapter_0080_task_0030}
package main

import (
    "fmt"
    "time"
)

func main() {
    measure := bestFunc()
    fmt.Println(measure(slowFunc, "firstFunc"))
    fmt.Println(measure(verySlowFunc, "secondFunc"))
    fmt.Println(measure(fastFunc, "thirdFunc"))
}

// ваш код здесь 

func verySlowFunc() {
    time.Sleep(500 * time.Millisecond)
}

func slowFunc() {
    time.Sleep(200 * time.Millisecond)
}

func fastFunc() {
    time.Sleep(1 * time.Millisecond)
}
``` 

Реализуйте идею замыкания. Функция `bestFunc` должна хранить наименьшее время выполнения и соответствующий псевдоним функции-аргумента. Возвращает `bestFunc` последний псевдоним, который она запомнила. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math"
	"time"
)

func main() {
	measure := bestFunc()
	fmt.Println(measure(slowFunc, "firstFunc"))
	fmt.Println(measure(verySlowFunc, "secondFunc"))
	fmt.Println(measure(fastFunc, "thirdFunc"))
}

func bestFunc() func(f func(), alias string) string {
	var res int64 = math.MaxInt64
	var fastestAlias string

	return func(f func(), alias string) string {
		start := time.Now()
		f()
		localRes := time.Since(start).Milliseconds()
		if localRes < res {
			res = localRes
			fastestAlias = alias
		}
		return fastestAlias
	}
}

func verySlowFunc() {
	time.Sleep(500 * time.Millisecond)
}

func slowFunc() {
	time.Sleep(200 * time.Millisecond)
}

func fastFunc() {
	time.Sleep(1 * time.Millisecond)
}
```
## Проблема утечки памяти

С замыканиями также связана проблема утечки памяти. Допустим, мы оперируем страницами памяти и используем для этого замыкание:

```go
const size = 1024

func page() func(valToSet int) [size]int {
	var pageMemory [size]int
	var i int

	return func(valToSet int) [size]int {
		if i < size {
			pageMemory[i] = valToSet
			i++
		}
		return pageMemory
	}
}
```

Если мы вызовем функцию `page` из `main`, то в памяти образуется массив длиной `1024` элементов. Сборщик мусора никогда не узнает о том, в какой момент этот массив больше вам не нужен. Поэтому он будет в памяти все время, до окончания работы программы. Используйте замыкания осторожно. 

## Резюме
1. Функции используются в качестве значений, как обычные переменные.
2. Функциям необязательно давать имя. Например, если функция короткая и нужна только для того, чтобы передать ее аргументом, разумно использовать анонимную функцию. 
3. До версии Go 1.22 существовала проблема захвата переменных значениями-функциями. Она была решена в новых версиях Go. Отметим, что для сохранения обратной совместимости новые компиляторы Go также поддерживают и старое поведение. Если в файле `go.mod` стоит версия Go ниже 1.22, а программа собирается новым компилятором, то будет использовано старое поведение.
4. Функции способны хранить *состояние*, т.е. обладать некоторой памятью. Такие функции называются замыканиями.
5. Замыкания могут приводить к утечкам памяти. Не используйте в качестве контекста переменные, требующие большого объема памяти.
