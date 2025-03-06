# Глава 8. Функции как объекты
## Значения-функции
Функции, подобно другим значениям, присваивают переменным: 
```go {.example_for_playground .example_for_playground_012}
package main

import (
    "fmt"
)

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

В данном случае `userMessage` имеет тип `func(string)`. Попытка присвоить в эту переменную функцию с другой сигнатурой приведет к ошибке компиляции:

```go {.example_for_playground .example_for_playground_013}
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

Нулевым значением типа функции является `nil`. Вызов функции со значением `nil` приведет к панике:
```go {.example_for_playground .example_for_playground_014}
func main() {
    var f func() string
    fmt.Println(f())
}
```

Если вы не уверены в том, что функция не нулевая, то перед вызовом ее нужно сравнить со значением `nil`:
```go {.example_for_playground .example_for_playground_015}
func main() {
    var f func() string
    if f != nil {
        fmt.Println(f())
    }
}
```

Сами значения-функции не сравнимы друг с другом, поэтому, например, их нельзя использовать в качестве ключей хеш-таблицы.

## Анонимные функции
Анонимные функции — это функции без имени:

```go {.example_for_playground .example_for_playground_016}
func main() {
    func() {
        fmt.Println("Hello from anonymous world!")
    }()
}
```

В данном случае мы создали анонимную функцию внутри `main` и вызвали ее через символы скобочек `()`. Если нужно использовать анонимную функцию в другом месте, то ее присваивают переменной: 

```go {.example_for_playground .example_for_playground_017}
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

```go {.example_for_playground .example_for_playground_018}
package main

import (
    "fmt"
)

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
## Замыкания 
Когда мы возвращаем некоторую функцию из другой функции, нам может потребоваться запомнить некоторое значение. Когда мы вернем эту функцию снова, то мы воспользуемся старым значением. Говоря более строго, некоторые функции имеют *состояние*. Такой прием предоставляют нам *замыкания (closures)*. Следующая программа вычисляет числа Фибоначчи с использованием замыкания:

```go {.example_for_playground .example_for_playground_019}
func main() {
    fib := nextFibonacci()
    for i := 0; i < 10; i++ {
        fmt.Printf("%d\t", fib())
    }
}

func nextFibonacci() func() int {
    a1 := -1
    a2 := 1

    return func() int {
        a3 := a1 + a2
        a1 = a2
        a2 = a3
        return a3
    }
}
```

При каждом вызове `fib()` мы помним о предыдущих значениях `a1`и `a2`.

Реализовано три функции: `verySlowFunc()`, `slowFunc()` и `fastFunc()`. Напишите функцию `bestFunc()`, которая принимает на вход одну из этих функций и ее псевдоним, а возвращает псевдоним наиболее быстрой функции из тех, что были переданы ранее. Так, код ниже должен вывести три значения: `firstFunc`, `firstFunc`, `thirdFunc`. Для того, чтобы узнать время выполнения функции в миллисекундах, воспользуйтесь стандартным пакетом `time`. Запомните текущее время с помощью `start := time.Now()`, а затем посчитайте количество миллисекунд с этой момента: `time.Since(start).Milliseconds()`.{.task_text}

```go {.task_source #golang_chapter_0070_task_0040}
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
    "time"
)

func main() {
    measure := bestFunc()
    fmt.Println(measure(slowFunc, "firstFunc"))
    fmt.Println(measure(verySlowFunc, "secondFunc"))
    fmt.Println(measure(fastFunc, "thirdFunc"))
}

func bestFunc() func(f func(), alias string) string {
    var maxInt64 int64 = 9223372036854775807
    var res int64 = maxInt64
    var fastestAlias string

    return func(f func(), alias string) string {
        start := time.Now()
        f()
        res2 := time.Since(start).Milliseconds()
        if res2 < res {
            res = res2
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

## Резюме.
1. Функции используются в качестве значений, как обычные переменные.
2.  Функциям необязательно давать имя. Например, если функция короткая и нужна только для того, чтобы передать ее аргументом, разумно использовать анонимную функцию. 
3.  Функции способны хранить *состояние*, т.е. обладать некоторой памятью. Такие функции называются замыканиями.