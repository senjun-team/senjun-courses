# Глава 18. Управление параллельным исполнением 

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

## Резюме 
1. Различают буферизованные и небуферизованные каналы. Небуферизованные каналы бывают полезны, когда необходимо синхронизировать работу горутин. Буферизованные каналы позволяют увеличить эффективность взаимодействия горутин. 
2. Конвейер — это прием, при котором выход одной горутины является входом для другой.