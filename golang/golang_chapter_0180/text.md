# Глава 18. Управление параллельным исполнением

Мы познакомились с горутинами и каналами. Нам пришлось поработать с программами, которые выполняются параллельно. Однако для простоты были опущены некоторые важные вопросы и приемы. Они рассматриваются в данной главе. 

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