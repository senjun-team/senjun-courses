# Глава 18. Управление параллельным исполнением

Мы познакомились с горутинами и каналами. Нам пришлось поработать с программами, которые выполняются параллельно. Однако для простоты были опущены некоторые важные вопросы и приемы. Они рассматриваются в данной главе. 

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