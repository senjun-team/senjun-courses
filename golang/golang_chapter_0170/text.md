# Глава 17. Подробнее про каналы 

В общих чертах, мы изучили горутины и каналы. Часть вопросов, связанная с каналами, осталась не рассмотренной. Эти темы достаточно обширные, поэтому они были вынесены в отдельную главу.   

## Буферизованные каналы

В Go есть каналы, которые обладают некоторым буфером. Элементы буфера выстраиваются в очередь, максимальный размер которой определяется при создании канала. Этот размер передается вторым параметром в функцию `make`:

```go
ch := make(chan int, 5)
```

Когда элемент отправляется в канал, то он встает в конец очереди. Чтение элемента происходит из головы очереди, после чего элемент удаляется из нее.

Если канал заполнен, то операция отправки нового значения в канал блокирует горутину. Блокировка длится до того момента, пока другая горутина не прочтет хотя бы одно значение. Если канал пуст, то операция чтения блокирует горутину до тех пор, пока другая горутина не положит в него хотя бы одно значение. 

Если необходимо узнать размер буфера канала, то для этого нужно вызвать встроенную функцию `cap`:

```go
fmt.Println(cap(ch)) // 5
```

Если требуется получить количество элементов в очереди, то используют встроенную функцию `len`:

```go
ch <- 5
ch <- 7
fmt.Println(len(ch)) // 2
```

Не используйте буферизованные каналы, в пределах одной горутины, просто как очередь. В противном случае есть риск вовсе заблокировать программу. Для этого достаточно, например, читать из канала, в которой никто не пишет. Каналы прежде всего связаны с горутинами. Это средство для пареллелизма. Когда вам нужна только очередь, используйте срез. 

Пул соединений (connection pool) — это кеш открытых подключений к базе данных. Соединения из пула могут использоваться многократно. Благодаря нему нет необходимости устанавливать соединение с базой данных каждый раз. Такой прием повышает производительность приложения. {.task_text} 

В следующем примере для пяти задач используется пул из трех соединений. Реализуйте создание нового пула размера `size` через функцию `newPool`. Внутри функции `newPool` необходимо создать буферизованный канал из соединений размером `size`. Инициализируйте соединения идентификаторами `1..size`. {.task_text} 

Код из `main` должен выполниться без ошибок. {.task_text} 

```go {.task_source #golang_chapter_0170_task_0010}
package main

import (
	"fmt"
	"time"
)

type Connection struct {
	ID int
}

func newPool(size int) chan *Connection {
	// ваш код здесь
}

func main() {
	const poolSize = 3
	const taskSize = 5
	pool := newPool(poolSize)
	// Используем соединения
	done := make(chan struct{}, taskSize)
	for i := 1; i <= taskSize; i++ {
		go func(taskID int) {
			// Берем соединение из пула
			conn := <-pool
			fmt.Printf("Task %d using connection %d\n", taskID, conn.ID)
			time.Sleep(time.Second)
			// Возвращаем в пул
			pool <- conn
			fmt.Printf("Task %d returned connection %d\n", taskID, conn.ID)
			done <- struct{}{}
		}(i)
	}
	// ожидаем завершения всех горутин
	for range taskSize {
		<-done
	}
}
```

Для создания пула используйте встроенную функцию `make`. Для его инициализации организуйте цикл от 1 до `size` включительно. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"time"
)

type Connection struct {
	ID int
}

func newPool(size int) chan *Connection {
	// Пул из size соединений
	pool := make(chan *Connection, size)

	// Инициализируем пул
	for i := 1; i <= size; i++ {
		pool <- &Connection{ID: i}
	}
	return pool
}

func main() {
	const poolSize = 3
	const taskSize = 5
	pool := newPool(poolSize)
	// Используем соединения
	done := make(chan struct{}, taskSize)
	for i := 1; i <= taskSize; i++ {
		go func(taskID int) {
			// Берем соединение из пула
			conn := <-pool
			fmt.Printf("Task %d using connection %d\n", taskID, conn.ID)
			time.Sleep(time.Second)
			// Возвращаем в пул
			pool <- conn
			fmt.Printf("Task %d returned connection %d\n", taskID, conn.ID)
			done <- struct{}{}
		}(i)
	}
	// ожидаем завершения всех горутин
	for range taskSize {
		<-done
	}
}
```
## Небуферизованный канал и канал с буфером на один элемент 

Небуферизованный канал и канал с буфером на один элемент — это не одно и то же. 

Когда отправитель посылает значение в небуферизованный канал, то он блокируется и ждет. После чтения этого значения из канала другой горутиной, отправитель будет разблокирован:

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func main() {

	ch := make(chan int)
	go func() {
		time.Sleep(time.Second)
		fmt.Println("Приемник готов")
		// Снимает блокировку с отправителя
		<-ch
	}()
	fmt.Println("Отправка...")
	// Блокируется на 1 секунду, пока получатель не проснется
	ch <- 42
	fmt.Println("Отправлено")
}
```
```
Отправка...
Приемник готов
Отправлено
```

Когда отправитель посылает значение в канал с пустым буфером на один элемент, то отправитель не блокируется. Он продолжает свою работу, независимо от получателя: 

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func main() {

	ch := make(chan int, 1)
	go func() {
		time.Sleep(time.Second)
		fmt.Println("Приемник готов")
		// Забирает из буфера
		<-ch
	}()
	fmt.Println("Отправка...")
	// Не блокируется, кладет в буфер и продолжает
	ch <- 42
	fmt.Println("Отправлено (но получатель еще спит)")
}
```
```
Отправка...
Отправлено (но получатель еще спит)
```

## Состояние гонки 

Рассмотрим пример банковского счета `bankAccount`. Запустим `100` горутин, каждая из которых кладет на счет 10 условных единиц. Результат работы этой программы варьируется. На счету у `Alice` от запуска к запуску программы оказывается разная сумма.

```go {.example_for_playground}
package main

import (
	"fmt"
)

type bankAccount struct {
	balance float64
	owner   string
}

func newBankAccount(owner string, initialBalance float64) *bankAccount {
	return &bankAccount{
		balance: initialBalance,
		owner:   owner,
	}
}

func (a *bankAccount) deposit(amount float64) error {
	if amount <= 0 {
		return fmt.Errorf("the sum must be positive")
	}
	a.balance += amount
	return nil
}

func main() {
	account := newBankAccount("Alice", 1000.0)

	// Запускаем 100 горутин, каждая пытается внести 10
	const operations = 100
	done := make(chan struct{}, operations)
	for range operations {
		go func() {
			account.deposit(10.0)
			done <- struct{}{}
		}()
	}
	for range operations {
		<-done
	}
	fmt.Println(account.balance)
}
```

Так получается, потому что одна из горутин читает старый баланс, который другая горутина еще не успела изменить. В итоге у `Alice` на счету оказывается меньше, чем мы ожидаем. Подобное поведение называется **состоянием гонки — race condition**.

Чтобы этого избежать, используют специальное средство — **мьютекс**. Мьютекс — это примитив синхронизации, который обеспечивает доступ к общему ресурсу только для одной горутины в текущий момент времени. В данном случае нам необходимо ввести мьютекс для структуры `bankAccount`:

```go
package main

import (
	"fmt"
	"sync"
)

type bankAccount struct {
	mu      sync.Mutex // Вводим мьютекс
	balance float64
	owner   string
}

func (a *bankAccount) deposit(amount float64) error {
	if amount <= 0 {
		return fmt.Errorf("the sum must be positive")
	}

	a.mu.Lock()         // Захватить мьютекс
	defer a.mu.Unlock() // Освободить мьютекс
	a.balance += amount
	return nil
}
```
```
2000
```

Когда горуина вызовет `deposit`, то она захватит мьютекс. Пока одна горутина держит мьютекс, другие горутины не могут получить доступ к `bankAccount`. Они ожидают до тех пор, пока горутина не освободит мьютекс. Таким образом разрешается ситуация гонки, и мы получаем ожидаемый результат.

Важно понимать, что мьютекс — это просто переменная. Она не обязана быть частью структуры. Когда мьютекс захвачен горутиной, то другие горутины не могут в тот же момент времени захватить этот мьютекс. Мы могли бы вынести мьютекс за пределы структуры: 

```go
var mu sync.Mutex // Вводим мьютекс
type bankAccount struct {
	balance float64
	owner   string
}

func (a *bankAccount) deposit(amount float64) error {
	if amount <= 0 {
		return fmt.Errorf("the sum must be positive")
	}

	mu.Lock()         // Захватить мьютекс
	defer mu.Unlock() // Освободить мьютекс
	a.balance += amount
	return nil
}
```
```
2000
```

В итоге мы получим тот же результат. Однако хорошей практикой считается все же делать мьютекс частью той структуры, которую он защищает. Благодаря этому приему код становится более наглядным.

Также отметим, что мьютекс замедляет работу программы, потому что горутины для данного участка кода выполняются последовательно. Другими словами, нет смысла все защищать мьютексами. В противном случае производительность может стать такой же, как у программы с единственной горутиной. Мьютексами нужно защищать только те данные, которые действительно в этом нуждаются. Подробнее о мьютексах мы поговорим в следующих главах. 

Некоторые программисты предпочитают вместо `sync.Mutex` использовать канал с буфером на один элемент. Делать так — дурной тон. Лучше все-таки работать с мьютексом, поскольку это явно подчеркивает намерение. Тем не менее, знать о таком способе применения каналов тоже нужно. Такое встречается в чужом коде. Попробуйте реализовать эту идею в данной задаче. {.task_text}

Следующий код реализует кеш. Функция `newCache` возвращает указатель на новый кеш. Метод `set` позволяет установить значение кеша по его ключу. Метод `get` возвращает значение кеша по его ключу, а также флаг. Этот флаг равен `true`, если значение есть в кеше. В противном случае — `false`. Метод `modify` модифицирует символ с индексом `i` значения по его ключу. {.task_text}

При запуске метода `modify` в единственной горутине все работает правильно. Однако при запуске нескольких горутин возникает конфликт. Результат работы `modify` непредсказуемый. Модифицируйте программу. Используйте для этого канал с буфером на один элемент. В результате работы функции `main` должна быть напечатана следующая строка: {.task_text}

```
res: 9876543210
```

```go {.task_source #golang_chapter_0170_task_0020}
package main

import (
	"errors"
	"fmt"
	"time"
)

type cache struct {
	store map[string]string
}

func newCache() *cache {
	c := make(map[string]string)
	return &cache{c}
}

func (c *cache) set(key string, value string) {
	c.store[key] = value
}

func (c *cache) modify(key string, i int, value string) error {
	s, ok := c.get(key)
	if !ok {
		return errors.New("no value")
	}
	if i < 0 || i >= len(s) {
		return errors.New("index is out of value")
	}
	c.store[key] = s[:i] + value + s[i+1:]
	return nil
}

func (c *cache) get(key string) (string, bool) {
	value, ok := c.store[key]
	// Имитация долгой обработки
	time.Sleep(1 * time.Millisecond)
	return value, ok
}

func main() {
	const codeLength = 10
	cache := newCache()
	cache.set("code", "0123456789")

	done := make(chan struct{}, codeLength)
	for i := range codeLength {
		go func(i int) {
			err := cache.modify("code", i, fmt.Sprintf("%d", 9-i))
			if err != nil {
				fmt.Printf("error: %s\n", err.Error())
			}
			done <- struct{}{}
		}(i)
	}

	// Ждем все горутины
	for i := 0; i < codeLength; i++ {
		<-done
	}
	res, ok := cache.get("code")
	if !ok {
		fmt.Println("error: no value in the cache")
	} else {
		fmt.Printf("res: %s\n", res)
	}
}
```

Добавьте внутри структуры `cache` поле `lock` типа `chan struct{}`. Внутри функции `newCache` запишите в этот канал значение. Это признак того, что «мьютекс» разблокирован. Внутри метода `modify` захватите «мьютекс», прочитав значение. Освободите «мьютекс» после того, как метод отработает. {.task_hint}

```go {.task_answer}
package main

import (
	"errors"
	"fmt"
	"time"
)

type cache struct {
	store map[string]string
	// "мьютекс"
	lock  chan struct{}
}

func newCache() *cache {
	c := make(map[string]string)
	lock := make(chan struct{}, 1)
	// разблокированный "мьютекс"
	lock <- struct{}{}
	return &cache{c, lock}
}

func (c *cache) set(key string, value string) {
	c.store[key] = value
}

func (c *cache) modify(key string, i int, value string) error {
	// заблокировать "мьютекс"
	<-c.lock
	// разблокировать "мьютекс"
	defer func() { c.lock <- struct{}{} }()
	s, ok := c.get(key)
	if !ok {
		return errors.New("no value")
	}
	if i < 0 || i >= len(s) {
		return errors.New("index is out of value")
	}
	c.store[key] = s[:i] + value + s[i+1:]
	return nil
}

func (c *cache) get(key string) (string, bool) {
	value, ok := c.store[key]
	// имитация долгой обработки
	time.Sleep(1 * time.Millisecond)
	return value, ok
}

func main() {
	const codeLength = 10
	cache := newCache()
	cache.set("code", "0123456789")

	done := make(chan struct{}, codeLength)
	for i := range codeLength {
		go func(i int) {
			err := cache.modify("code", i, fmt.Sprintf("%d", 9-i))
			if err != nil {
				fmt.Printf("error: %s\n", err.Error())
			}
			done <- struct{}{}
		}(i)
	}

	// ждем все горутины
	for i := 0; i < codeLength; i++ {
		<-done
	}
	res, ok := cache.get("code")
	if !ok {
		fmt.Println("error: no value in the cache")
	} else {
		fmt.Printf("res: %s\n", res)
	}
}
```

## Каналы и ключевое слово range

В следующем примере мы обрабатываем задачи в отдельной горутине `processTasks`. Результаты она записывает в канал. В горутине `main` мы читаем эти значения из канала, пока `processTasks` его не закроет. 

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func processTasks(ch chan int) {
	for i := 0; i < 5; i++ {
		ch <- i
		// Обработка задачи
		time.Sleep(500 * time.Millisecond)
	}

	close(ch)
}

func main() {
	ch := make(chan int)

	go processTasks(ch)

	for {
		value, ok := <-ch
		if !ok {
			break
		}
		fmt.Printf("Got: %d\n", value)
	}
}
```
```
Got: 0
Got: 1
Got: 2
Got: 3
Got: 4
```

Синтаксис цикла выглядит странно: 

```go
for {
	value, ok := <-ch
	if !ok {
		break
	}
	fmt.Printf("Got: %d\n", value)
}
```

Поэтому вместо этого используют ключевое слово `range`. Следующий код эквивалентен предыдущему:

```go
for value := range ch {
	fmt.Printf("Got: %d\n", value)
}
```

## Однонаправленные каналы 

В следующем коде допущена ошибка. Функция `sendResponse`, которая должна отправить данные в канал, наоборот читает из него. Это приводит к тому, что все горутины «зависают».  

```go {.example_for_playground}
package main

func sendResponse(response string, ch chan string) {
	response = <-ch
}

func main() {
	ch := make(chan string)
	go sendResponse("Hi senior!", ch)
	<-ch
}
```
```
fatal error: all goroutines are asleep - deadlock!
```

Чтобы избежать неправильного поведения еще во время компиляции, используют однонаправленные каналы. Они позволяют передавать значение лишь в одну сторону: либо писать, либо читать. Тип `chan<-string` означает канал для типа `string` только на запись, а тип `<-chan string` — только на чтение.

Мы могли бы переписать функцию `sendResponse` следующим образом: 

```go
func sendResponse(response string, ch chan<- string) {
	response = <-ch
}
```
```
./main.go:4:15: invalid operation: cannot receive from send-only channel ch (variable of type chan<- string) (exit status 1)
```

Таким образом, мы сразу поймем, где и почему возникает проблема. 

Вызов `sendResponse` неявно преобразует двунаправленный канал в однонаправленный. Такое преобразование всегда разрешается, однако обратное неверно. 

Закрыть канал, предназначенный  только для чтения, невозможно. Это приведет к ошибке компиляции: 

```go {.example_for_playground}
package main

func getResponse(ch <-chan string) string {
	response := <-ch
	close(ch)
	return response
}

func main() {
	ch := make(chan string)
	ch <- "Hi senior!"
	go getResponse(ch)
}
```
```
./main.go:5:8: invalid operation: cannot close receive-only channel ch (variable of type <-chan string) (exit status 1)
```

Дело в том, что закрытие канала означает: в этот канал больше не будут передаваться значения. Но в канал, доступный только для чтения, и так не может быть передано ни одно значение! Поэтому операция не имеет смысла. Если это необходимо, то канал должна закрыть отправляющая сторона.

Что выведет следующий код? В случае ошибки напишите `error`. {.task_text}

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func generateCode(message string) int {
	// Имитация вычислений
	time.Sleep(time.Second)
	return 4 * len(message)
}

func worker(messages <-chan string, res chan<- int) {
	var finalCode int
	for message := range messages {
		finalCode += generateCode(message) / 2
	}
	res <- finalCode
}

func main() {
	messages := make(chan string, 5)
	for i := range 5 {
		var message string
		for range i {
			message += "A"
		}
		messages <- message
	}
	close(messages)
	res := make(chan int, 3)
	for range 3 {
		go worker(messages, res)
	}
	var code int
	for range 3 {
		code += <-res
	}
	fmt.Println(code)
}
```

```consoleoutput {.task_source #golang_chapter_0170_task_0030}
```
В первом цикле внутри `main` будет сформировано пять сообщений: пустое сообщение, `A`, `AA`, `AAA` и `AAAA`. Каждый из трех воркеров берет сообщения из канала `messages` и пишет результат обработки этих сообщений в `res`. На выходе получаем сумму результатов обработки. {.task_hint}

```{.task_answer}
20
```

## Резюме

1. Буферизованные каналы обладают буфером некоторого размера. Это позволяет горутинам писать в канал многократно до того, пока будет выполнено первое чтение.
2. Небуферизованный канал и канал с буфером на один элемент — это разные каналы. Небуферизованный канал используется для синхронизации горутин. Канал с буфером на один элемент позволяет горутине отправить значение в буфер и продолжать свою работу.
3. Канал с буфером на один элемент иногда используют в качестве мьютекса. Однако, если есть возможность работать с мьютексом, лучше применить мьютекс. Это явно подчеркивает намерение.
4. Когда необходимо в цикле читать значения из канала до момента его закрытия, используют ключевое слово `range`.
5. Каналы бывают однонаправленные и двунаправленные.
