# Глава 17. Управление параллельным исполнением

Мы познакомились с горутинами и каналами. Нам пришлось поработать с программами, которые выполняются параллельно. Однако для простоты были опущены некоторые важные вопросы и приемы. Они рассматриваются в данной главе. 

## Буферизованные каналы

Буферизованный канал обладает некоторым буфером. Элементы буфера выстраиваются в очередь, максимальный размер которой определяется при создании канала. Максимальный размер передается вторым параметром в функцию `make`:

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

Мьютекс — это переменная, которую горутины могут захватывать и освобождать. Захватывая мьютекс, горутина блокирует доступ к переменным для других горутин. Это бывает нужно, когда одновременный доступ к переменным нескольких горутин приводит к ошибкам.  {.task_text} 

В Go для мьютекса есть специальный тип данных — `sync.Mutex`. Однако некоторые программисты предпочитают вместо `sync.Mutex` использовать канал с буфером на один элемент. Лучше все-таки работать с мьютексом, поскольку это явно подчеркивает намерение. Тем не менее, знать о таком способе применения каналов тоже нужно. Такое встречается в чужом коде. Попробуйте реализовать эту идею в данной задаче. Подробнее о мьютексах мы поговорим в следующих главах. {.task_text}

Следующий код реализует кеш. Функция `newCache` возвращает указатель на новый кеш. Метод `set` позволяет установить значение кеша по его ключу. Метод `get` возвращает значение кеша по его ключу, а также флаг. Этот флаг равен `true`, если значение есть в кеше. В противном случае — `false`. Метод `modify` модифицирует символ с индексом `i` значения по его ключу. {.task_text}

При запуске метода `modify` в единственной горутине все работает правильно. Однако при запуске несколько горутин возникает конфликт. Результат работы `modify` непредсказуемый. Модифицируйте программу. Используйте для этого канал с буфером на один элемент. В результате работы функции `main` должна быть напечатана следующая строка: {.task_text}

```
res: 9876543210
```

```go {.task_source #golang_chapter_0160_task_0020}
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

## Конвейеры

Горутины могут взаимодействовать друг с другом таким образом, что выход одной из них является входом для другой. Такое поведение достигается через каналы и называется **конвейером.**

Следующий пример демонстрирует конвейер для генерации и обработки заказов интернет-магазина:

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

// Заказ
type Order struct {
	ID     int
	Amount float64
	Status string
}

const ordersNumber = 10

func main() {
	// Создаем каналы для каждого этапа конвейера
	orders := make(chan Order, ordersNumber)
	processedOrders := make(chan Order, ordersNumber)

	// Генерация заказов
	go generateOrders(orders)

	// Обработка заказов
	go processOrder(orders, processedOrders)

	for order := range processedOrders {
		fmt.Printf("Order №%d was processed, sum %.2f was got\n",
			order.ID, order.Amount)
	}
}

func generateOrders(out chan<- Order) {
	for i := 1; i <= ordersNumber; i++ {
		order := Order{
			ID:     i,
			Amount: float64(i * 100),
			Status: "new",
		}
		fmt.Printf("Order №%d for sum %.2f was made\n", order.ID, order.Amount)
		out <- order
		time.Sleep(100 * time.Millisecond) // Имитация задержки
	}
	close(out)
}

func processOrder(in <-chan Order, out chan<- Order) {
	for order := range in {
		time.Sleep(200 * time.Millisecond) // Имитация обработки платежа

		order.Status = "processed"
		out <- order
	}
	close(out)
}
```
```
Order №1 for sum 100.00 was made
Order №2 for sum 200.00 was made
Order №3 for sum 300.00 was made
Order №1 was processed, sum 100.00 was got
Order №4 for sum 400.00 was made
Order №5 for sum 500.00 was made
Order №2 was processed, sum 200.00 was got
Order №6 for sum 600.00 was made
Order №3 was processed, sum 300.00 was got
Order №7 for sum 700.00 was made
Order №8 for sum 800.00 was made
Order №9 for sum 900.00 was made
Order №4 was processed, sum 400.00 was got
Order №10 for sum 1000.00 was made
Order №5 was processed, sum 500.00 was got
Order №6 was processed, sum 600.00 was got
Order №7 was processed, sum 700.00 was got
Order №8 was processed, sum 800.00 was got
Order №9 was processed, sum 900.00 was got
Order №10 was processed, sum 1000.00 was got
```


## Выбор между буферизованным и небуферизованным каналами 

У некоторых программистов возникают трудности с тем, какой канал выбрать. Кроме того, в случае буферизованного канала возникает вопрос: какого размера буфер использовать? Рассмотрим типичные ситуации с буферизованными и небуферизованными каналами. 

- Необходимость синхронизировать работу горутин. Для этого подойдет небуферизованный канал. Каждой операции отправления должна соответствовать операция получения. Поэтому такой способ достаточно надежный.
- Мы заранее знаем максимальный размер очереди. Если он в разумных пределах, то есть смысл использовать буферизованный канал с этим размером буфера. Кроме того, нет ничего плохого в том, чтобы заполнить буфер полностью, еще до чтения из него. 
- Выбор может быть сделан в пользу буферизованного канала, если это увеличит производительность. Например, в случае с интернет-магазином одна горутина генерирует заказы, а другая их обрабатывает. Генерация происходит быстрее обработки. Если бы между горутинами не было буферизованного канала, то горутина генерации простаивала бы, пока горутина обработки не закончится. Это бы ничем не отличалось по производительности от приложения с единственной горутиной. Буфер позволяет генерировать заказы дальше, пока другая горутина их обрабатывает. 

Брокер сообщений получает сообщения, декодирует их, валидирует и отправляет результат в канал. Чтобы запустить брокер, используется функция `brokerStart`. Реализуйте идею конвейера, чтобы ускорить время выполнения программы. {.task_text}

Добавьте вторым параметром в функцию `getData` однонаправленный канал `out`. Канал должен принимать значения типа `[]rune`. Функция `getData` должна писать сообщения в этот канал и ничего не возвращать. Вместо параметра `data` функции `decode` используйте два канала: `in` — в качестве первого параметра и `out` — в качестве второго. Функция читает из канала `in`, декодирует информацию и пишет в канал `out`. Из канала `in` читаются значения типа `[]rune`. Канал `out` внутри функции `decode` принимает значения типа `message`. Метод `valid` замените на функцию `valid`. Она будет иметь сигнатуру, аналогичную функции `decode`. Однако оба канала для `valid` работают со значениями типа `message`. Функция `valid` пропускает далее только валидные сообщения. Функция `brokerStart` должна запустить три горутины: `getData`, `decode` и `valid`. В качестве буфера для всех каналов используйте буфер размера `maxMessageNumber`. {.task_text}

Обратите внимание на исходное время выполнения программы и на то, что получится у вас. {.task_text}

```go {.task_source #golang_chapter_0160_task_0030}
package main

import (
	"fmt"
	"math/rand"
	"time"
)

type message struct {
	data   string
	length int
}

func getData(r *rand.Rand) [][]rune {
	const maxMessageLen = 42
	const minMessageLen = 1
	const maxMessageNumber = 15
	const minMessageNumber = 5
	letters := []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	var res [][]rune
	messageNumber := r.Intn(maxMessageNumber-minMessageNumber) + minMessageNumber
	for range messageNumber {
		messageLen := r.Intn(maxMessageLen-minMessageLen) + minMessageLen
		b := make([]rune, messageLen)
		for i := range b {
			b[i] = letters[r.Intn(len(letters))]
		}
		res = append(res, b)
	}
	return res
}

func decode(data []rune) message {
	// Имитация задержки
	time.Sleep(1 * time.Millisecond)
	return message{string(data), len(string(data))}
}

func (m message) valid() bool {
	const minLength = 1
	// Имитация задержки
	time.Sleep(2 * time.Millisecond)
	return m.length >= minLength
}

func brokerStart(r *rand.Rand, commits chan<- message) {
	// Получение сообщений
	dataFrames := getData(r)
	for _, frame := range dataFrames {
		// Декодирование
		msg := decode(frame)
		// Валидация
		if !msg.valid() {
			continue
		}
		// Подтверждение обработки
		commits <- msg
	}
	close(commits)
}

func main() {
	start := time.Now()
	seed := int64(42)
	r := rand.New(rand.NewSource(seed))
	commits := make(chan message)
	go brokerStart(r, commits)
	for commit := range commits {
		fmt.Println(commit)
	}
	end := time.Now()
	elapsed := end.Sub(start).Milliseconds()
	fmt.Printf("%d milliseconds", elapsed)
}
```

Не забудьте закрыть каналы везде, где это необходимо. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math/rand"
	"time"
)

const maxMessageNumber = 15
const maxMessageLen = 42
const minMessageLen = 1
const minMessageNumber = 5

type message struct {
	data   string
	length int
}

func getData(r *rand.Rand, out chan<- []rune) {
	letters := []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	messageNumber := r.Intn(maxMessageNumber-minMessageNumber) + minMessageNumber
	for range messageNumber {
		messageLen := r.Intn(maxMessageLen-minMessageLen) + minMessageLen
		b := make([]rune, messageLen)
		for i := range b {
			b[i] = letters[r.Intn(len(letters))]
		}
		out <- b
	}
	close(out)
}

func decode(in <-chan []rune, out chan<- message) {
	for data := range in {
		// Имитация задержки
		time.Sleep(1 * time.Millisecond)
		out <- message{string(data), len(string(data))}
	}
	close(out)
}

func valid(in <-chan message, out chan<- message) {
	const minLength = 1
	// Имитация задержки
	time.Sleep(2 * time.Millisecond)
	for msg := range in {
		if msg.length >= minLength {
			out <- msg
		}
	}
	close(out)
}

func brokerStart(r *rand.Rand, commits chan<- message) {
	inData := make(chan []rune, maxMessageNumber)
	inMessage := make(chan message, maxMessageNumber)
	go getData(r, inData)
	go decode(inData, inMessage)
	go valid(inMessage, commits)
}

func main() {
	start := time.Now()
	seed := int64(42)
	r := rand.New(rand.NewSource(seed))
	commits := make(chan message, maxMessageNumber)
	go brokerStart(r, commits)
	for commit := range commits {
		fmt.Println(commit)
	}
	end := time.Now()
	elapsed := end.Sub(start).Milliseconds()
	fmt.Printf("%d milliseconds", elapsed)
}
```

## Утечка горутин 

Следующий пример демонстрирует утечку горутин. Мы запускаем 10 горутин для асинхронной обработки сообщений. Результат обработки сообщений отправляется в канал уведомлений, откуда их читает другая горутина. Проблема в том, что «горутины» зависают. Возникает утечка памяти. {.task_text}  

Исправьте ошибку в коде. Обработчики сообщений должны завершиться. Количество горутин вначале и в конце должно быть одинаковым. {.task_text}  

```go {.task_source #golang_chapter_0160_task_0040}
package main

import (
	"fmt"
	"runtime"
	"time"
)

func main() {
	// Запускаем воркер для обработки уведомлений
	const chanSize = 5
	var notificationChan = make(chan string, chanSize)
	go notificationWorker(notificationChan)

	fmt.Printf("Begin! Goroutines number: %d\n", runtime.NumGoroutine())

	// Запускаем обработку 10 сообщений
	const messageNumber = 10
	for i := 0; i < messageNumber; i++ {
		go processTaskAsync("data", notificationChan)
	}
	fmt.Printf("All goroutines started! Goroutines number: %d\n", runtime.NumGoroutine())

	// Показываем утечку
	time.Sleep(5 * time.Second)
	fmt.Printf("End! Goroutined number after 5 seconds: %d\n", runtime.NumGoroutine())
}

func processTaskAsync(data string, notificationChan chan<- string) {
	for {
		// Имитируем долгую обработку
		result := expensiveProcessing(data)
		// Отправляем уведомление
		notificationChan <- result
	}
}

func expensiveProcessing(data string) string {
	time.Sleep(100 * time.Millisecond) // Имитация работы
	return "processed: " + data
}

// Воркер для уведомлений
func notificationWorker(notificationChan <-chan string) {
	for msg := range notificationChan {
		// Имитация отправки уведомления
		time.Sleep(50 * time.Millisecond)
		fmt.Printf("Message sent: %s\n", msg)
	}
}
```

Уберите вечный цикл внутри `processTaskAsync`. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"runtime"
	"time"
)

func main() {
	// Запускаем воркер для обработки уведомлений
	const chanSize = 5
	var notificationChan = make(chan string, chanSize)
	go notificationWorker(notificationChan)

	fmt.Printf("Begin! Goroutines number: %d\n", runtime.NumGoroutine())

	// Запускаем обработку 10 сообщений
	const messageNumber = 10
	for i := 0; i < messageNumber; i++ {
		go processTaskAsync("data", notificationChan)
	}
	fmt.Printf("All goroutines started! Goroutines number: %d\n", runtime.NumGoroutine())

	// Показываем утечку
	time.Sleep(5 * time.Second)
	fmt.Printf("End! Goroutined number after 5 seconds: %d\n", runtime.NumGoroutine())
}

// ПРОБЛЕМА была ЗДЕСЬ: эта функция использовала вечный цикл
// горутины никогда не завершались
func processTaskAsync(data string, notificationChan chan<- string) {
	// Имитируем долгую обработку
	result := expensiveProcessing(data)
	// Отправляем уведомление
	notificationChan <- result
}

func expensiveProcessing(data string) string {
	time.Sleep(100 * time.Millisecond) // Имитация работы
	return "processed: " + data
}

// Воркер для уведомлений
func notificationWorker(notificationChan <-chan string) {
	for msg := range notificationChan {
		// Имитация отправки уведомления
		time.Sleep(50 * time.Millisecond)
		fmt.Printf("Message sent: %s\n", msg)
	}
}
```

## Счетчик sync.WaitGroup

Когда запущено несколько горутин, бывает удобно использовать счетчик горутин. Каждая горутина должна при старте увеличить счетчик на единицу и уменьшить его после завершения. Когда счетчик вновь окажется равным нулю, то это значит, что все запущенные горутины завершили свою работу. Такой счетчик в Go называется `sync.WaitGroup`. Для работы с ним сначала нужно импортировать пакет `sync`, а потом объявить переменную данного типа:

```go
var wg sync.WaitGroup
```

Затем с помощью метода `Add` счетчику сообщают о том, на какое значение его нужно увеличить. Необязательно передавать методу `Add` единицу перед запуском каждой горутины. Бывает удобно сразу передать в `Add` общее количество горутин, которые будут запущены. Например, для пяти горутин:

```go
wg.Add(5)
```

После того, как горутина выполнится, она должна вызвать метод `wg.Done`. Обычно это делают с помощью `defer`, чтобы гарантировать вызов `wg.Done`, даже в случае ошибки: 

```go
defer wg.Done()
```

Метод `wg.Wait` ожидает выполнения всех горутин: 

```go
wg.Wait()
```

Метод `Add` должен быть вызван перед запуском горутин, а не внутри них! В противном случае мы не можем быть уверены, что вызвали его до метода `Wait`.

Следующий пример демонстрирует использование `sync.WaitGroup`. Программма должна обработать матрицу оборудования размером 1 млн. элементов. Каждый элемент представляет собой структуру из имени оборудования и признака того, что оно валидно. Допустимым именем оборудования является любая строка, в которой отсутствуют символы `"!@#$%^&*()"`. В противном случае имя является недопустимым. Необходимо проставить признак допустимости имени для каждого элемента. 

Программа выполняется в двух режимах: последовательном и параллельном. В случае параллельного режима мы сначала запрашиваем число процессеров через функцию `runtime.NumCPU()`. Число горутин определяется количеством процессоров. Поделив матрицу на части, мы запускаем по горутине на каждую часть. Переменная `wg` типа `sync.WaitGroup` используется в качестве счетчика горутин. 

Обратите внимание на время выполнения последовательной и параллельной версий. 

```go {.example_for_playground}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"slices"
	"sync"
	"time"
)

const elementsNumber = 1000000
const (
	// iota присваивает целые значения
	// константам: 0, 1, 2...
	unknown = iota
	valid
	invalid
)

type equipmentT struct {
	name    string
	isValid int
}

// генерирует случайное имя оборудования
func randName(r *rand.Rand, n int) string {
	var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	var invalidSymbols = []rune("!@#$%^&*()")
	allSymbols := append(letters, invalidSymbols...)
	b := make([]rune, n)
	for i := range b {
		b[i] = allSymbols[r.Intn(len(allSymbols))]
	}
	return string(b)
}

// воркер, проверяющий свою часть матрицы
func worker(input *[elementsNumber]equipmentT, min int, max int) {
	for i := min; i < max; i++ {
		input[i].isValid = check(input[i].name)
	}
}

// функция проверки имени оборудования
func check(name string) int {
	var invalidSymbols = []rune("!@#$%^&*()")
	for _, r := range name {
		// slices.Contains проверяет вхождение
		// элемента в срез
		if slices.Contains(invalidSymbols, r) {
			return invalid
		}
	}
	return valid
}

/*
напечатать статистику по матрице:
validNumber - число валидных имен
invalidNumber - число невалидных имен
unknownNumber - число непроверенных имен
*/
func statisticPrintln(result *[elementsNumber]equipmentT) {
	validNumber := 0
	invalidNumber := 0
	for _, equipment := range result {
		switch equipment.isValid {
		case valid:
			validNumber++
		case invalid:
			invalidNumber++
		}
	}
	fmt.Printf("validNumber=%d, invalidNumber=%d, unknownNumber=%d\n",
		validNumber, invalidNumber, elementsNumber-validNumber-invalidNumber)
}
func main() {
	// подготовка данных
	// матрица для последовательных вычислений
	equipmentMatrix := [elementsNumber]equipmentT{}
	// матрица для параллельных вычислений
	workerEquipmentMatrix := [elementsNumber]equipmentT{}
	seed := int64(42)
	source := rand.NewSource(seed)
	r := rand.New(source)
	for i := range elementsNumber {
		equipmentMatrix[i] = equipmentT{name: randName(r, 10),
			isValid: unknown}
		workerEquipmentMatrix[i] = equipmentMatrix[i]
	}
	// последовательная часть программы
	t1 := time.Now()
	for i := range len(equipmentMatrix) {
		equipmentMatrix[i].isValid = check(equipmentMatrix[i].name)
	}
	t2 := time.Now()
	resTime := t2.Sub(t1).Milliseconds()
	fmt.Println("equipmentMatrix:")
	statisticPrintln(&equipmentMatrix)
	fmt.Printf("time: %d milliseconds\n", resTime)
	fmt.Println()
	// параллельная часть программы
	workersNumber := runtime.NumCPU()
	// math.Ceil округляет значение в большую сторону
	chunk := int(math.Ceil(float64(elementsNumber) / float64(workersNumber)))
	// ЗДЕСЬ работаем с sync.waitGroup
	var wg sync.WaitGroup
	wg.Add(workersNumber)
	min := 0
	max := min + chunk
	if max > elementsNumber {
		max = elementsNumber
	}
	t1 = time.Now()
	for range workersNumber {
		go func(min int, max int) {
			defer wg.Done()
			worker(&workerEquipmentMatrix, min, max)
		}(min, max)
		min = max
		max = min + chunk
		if max > elementsNumber {
			max = elementsNumber
		}
	}
	wg.Wait()
	t2 = time.Now()
	resTime = t2.Sub(t1).Milliseconds()
	fmt.Println("workerEquipmentMatrix:")
	statisticPrintln(&workerEquipmentMatrix)
	fmt.Printf("time: %d milliseconds\n", resTime)
}
```
```
equipmentMatrix:
validNumber=171790, invalidNumber=828210, unknownNumber=0
time: 145 milliseconds

workerEquipmentMatrix:
validNumber=171790, invalidNumber=828210, unknownNumber=0
time: 19 milliseconds
```

## Ключевое слово select 
## Захват глобальных переменных горутиной
## Мьютекс
