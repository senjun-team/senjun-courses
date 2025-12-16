# Глава 16. Горутины и каналы 

## Понятие горутины

В Go программа исполняется как одна или несколько горутин. **Горутина** — это легковесный поток, который управляется средой выполнения Go. Функция `main` также выполняется в горутине. До сих пор все наши программы выполнялись в единственной горутине. 

Чтобы запустить из `main` новую горутину, достаточно поставить перед вызовом функции ключевое слово `go`. В следующем примере мы запускаем горутину для вывода сообщения о загрузке. В это время, в функции `main`, параллельно выполняется некоторая работа. Когда эта работа закончится, функция `main` завершится. Вместе с ней остановятся и все горутины, которые были запущены из `main`:

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func printLoad(seconds time.Duration) {
	const pointsNumber = 10
	millisconds := seconds * 1000
	msPerPoint := (millisconds / pointsNumber)
	fmt.Print("Loading")
	for range pointsNumber {
		time.Sleep(msPerPoint * time.Millisecond)
		fmt.Print(".")
	}
}

func heavyWorker() {
	// объемная работа
	time.Sleep(3 * time.Second)
	fmt.Println("\nDone")
}

func main() {
	go printLoad(5)
	heavyWorker()
}
```
```
Loading.....
Done
```

## Горутины изнутри

Как уже было сказано, горутина — легковесный поток, но все же горутина — это не то же самое, что поток операционной системы.

Поток операционной системы содержит стек. Стек хранит локальные переменные вызовов функций. Он имеет фиксированный размер. Как правило, 2 Мбайта. Этот размер оказывается большим для потоков, которые нужны, например, чтобы показывать надпись загрузки. Однако этот размер окажется маленьким для сложных функций. Например, для таких, которые обладают глубокой рекурсией. 

Горутина имеет стек переменного размера. Это повышает ее эффективность. Она начинает выполняться со стеком небольшого размера. Как правило, 2 Кбайта. Такой стек, аналогично стеку потока операционной системы, хранит локальные переменные вызовов функций. Он увеличивается, по мере необходимости. Его размер может достигать 1 Гбайта. 

Потоки операционной системы планируются в ее ядре. Процессор периодически прерывается по таймеру, и запускается планировщик. Планировщик приостанавливает поток, сохраняет значения его регистров в памяти и решает, какой из потоков запустить следующим. Для этого он восстанавливает регистры данного потока и возобновляет его выполнение. Все это происходит медленно, поскольку требуется многократное обращение к памяти, обладающей слабой локальностью. Слабая локальность означает, что вероятность повторного обращения к те же ячейкам памяти невелика. Вероятность обращения к ячейкам памяти, расположенным рядом, также небольшая. 

Среда выполнения Go имеет свой планировщик. Он мультиплексирует выполнение `m` горутин на `n` потоках операционной системы. Этот метод известен как **m:n планирование**. Планировщик Go работает аналогично планировщику операционной системы, но его задания касаются только горутин единственной программы на Go. 

Планировщик Go вызывается не по таймеру, а неявными инструкциями языка Go. Он работает быстро, потому что у него нет необходимости обращаться к ядру операционной системы и взаимодействовать с потоками.

Планировщик Go использует параметр с именем **GOMAXPROCS**. Этот параметр содержит максимальное количество потоков, которое можно использовать для одновременного запуска горутин. До Go 1.25 этот параметр равнялся количеству логических ЦП на машине. Начиная с версии Go 1.25, разработчики приняли во внимание контейнерную среду некоторых программ. Для программ в контейнере теперь [учитываются](https://go.dev/blog/container-aware-gomaxprocs) ограничения этого контейнера. Если программа на Go выполняется внутри контейнера с ограничением ресурсов ЦП, GOMAXPROCS по умолчанию соответствует этому ограничению. Таким образом, GOMAXPROCS оказывается меньше числа реальных ЦП. Иногда переменную окружения GOMAXPROCS устанавливают явно, либо для этого вызывают функцию `runtime.GOMAXPROCS` из кода.

Важной особенностью горутин является то, что  они не имеют идентификатора, который может быть получен как обычное значение. Он недоступен программисту. Это было сделано разработчиками Go сознательно. В противном случае могли бы возникнуть зависимости функции не только от ее аргументов. Она могла бы зависеть и от идентификатора потока, в котором функция выполняется. Разработчики Go уверяют, что это не нужно и поощряют простоту в написании программ.

## Небуферизованные каналы 

Каналы позволяют горутинам «общаться» между собой. Они позволяют одной горутине передать какое-либо значение другой горутине. Канал передает данные определенного типа — **типа элементов канала.** Каналы бывают буферизованные и небуферизованные. Пока что мы будем говорить только про небуферизованные каналы. Чтобы создать такой канал, нужно воспользоваться встроенной функцией `make`. Например, для значений типа `bool`:

```go
ch := make(chan bool)
```

Следующие три инструкции демонстрируют, как писать в канал и читать из него: 

```go
ch <- true  // Записать true в канал
val := <-ch // Прочесть значение из канала в val
<-ch        // Результат чтения не используется
```

Закрывается канал через встроенную функцию `close`:

```go
close(ch)
```

Канал закрывают, если по этому каналу больше не будут передаваться значения.   
Попытка закрыть уже закрытый канал приведет к панике: 

```
panic: close of closed channel
```

Попытка отправить значение в закрытый канал также приведет к панике:

```
panic: send on closed channel
```

При чтении из закрытого небуферизованного канала вы получите значение типа по умолчанию.

Иногда нужно точно знать, получили мы реальное значение или нет. Например, при чтении из закрытого небуферизованного канала в реальности никакого значения получено не будет. На самом деле, при чтении из канала возвращается два значения. В этом случае используют следующую конструкцию: 

```go
val, ok := <-ch
```
Если `ok` равен `true`, то значение прочитано. В противном случае — нет.

Некоторая задача `task` выолняется на сервере `server`. Сервер не запущен все время. Он запускается по мере необходимости. На старт сервера затрачивается некоторое время. Для выполнения задачи нужно подготовить данные. Эта операция также выполняется не сразу. {.task_text}

Код ниже выполняется последовательно. Модифицируйте код таким образом, чтобы он выполнялся параллельно. Данные должны готовиться во время того, как уже стартует сервер. В отладочных сообщениях вы должны увидеть следующий текст: {.task_text}

```
starting server...
task data prepared
calculating...
stopping server...
```

```go {.task_source #golang_chapter_0160_task_0010}
package main

import (
	"fmt"
	"time"
)

type deviceConfig struct {
	id   int
	name string
	job  func()
}

func main() {
	server := deviceConfig{1, "server", func() {
		fmt.Println("starting server...")
		time.Sleep(1 * time.Second)
		fmt.Println("calculating...")
		time.Sleep(1 * time.Second)
		fmt.Println("stopping srever...")
	}}
	task := deviceConfig{2, "task", func() {
		time.Sleep(2 * time.Second)
		fmt.Println("task data prepared")
	}}
	task.job()
	server.job()
}
```

Использйуйте каналы типа `struct{}`. Передача значения в такой канал будет сигнализировать о том, что работа выполнена. Не забудьте, что `main` — это тоже горутина. Она не будет ждать выполнения всех других горутин, если не организовать такое поведение явно. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"time"
)

type deviceConfig struct {
	id   int
	name string
	job  func()
}

func main() {
	dataReady := make(chan struct{})
	done := make(chan struct{})

	server := deviceConfig{1, "server", func() {
		fmt.Println("starting server...")
		time.Sleep(1 * time.Second)
		<-dataReady
		fmt.Println("calculating...")
		time.Sleep(1 * time.Second)
		fmt.Println("stopping server...")
		done <- struct{}{}
	}}
	task := deviceConfig{2, "task", func() {
		time.Sleep(2 * time.Second)
		fmt.Println("task data prepared")
		dataReady <- struct{}{}
	}}

	go server.job()
	go task.job()
	<-done
}
```


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

func randSeq(r *rand.Rand, n int) string {
	var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
	var invalidSymbols = []rune("!@#$%^&*()")
	allSymbols := append(letters, invalidSymbols...)
	b := make([]rune, n)
	for i := range b {
		b[i] = allSymbols[r.Intn(len(allSymbols))]
	}
	return string(b)
}

func worker(input *[elementsNumber]equipmentT, min int, max int) {
	for i := min; i < max; i++ {
		input[i].isValid = check(input[i].name)
	}
}

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

func printlnResult(result *[elementsNumber]equipmentT) {
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
	equipmentMatrix := [elementsNumber]equipmentT{}
	workerEquipmentMatrix := [elementsNumber]equipmentT{}
	seed := int64(42)
	source := rand.NewSource(seed)
	r := rand.New(source)
	for i := range elementsNumber {
		equipmentMatrix[i] = equipmentT{name: randSeq(r, 10),
			isValid: unknown}
		workerEquipmentMatrix[i] = equipmentMatrix[i]
	}

	t1 := time.Now()
	for i := range len(equipmentMatrix) {
		equipmentMatrix[i].isValid = check(equipmentMatrix[i].name)
	}
	t2 := time.Now()
	resTime := t2.Sub(t1).Milliseconds()
	fmt.Println("equipmentMatrix:")
	printlnResult(&equipmentMatrix)
	fmt.Printf("time: %d ms\n", resTime)
	fmt.Println()
	workersNumber := runtime.NumCPU()
	// math.Ceil округляет значение в большую сторону
	chunk := int(math.Ceil(float64(elementsNumber) / float64(workersNumber)))
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
	printlnResult(&workerEquipmentMatrix)
	fmt.Printf("time: %d ms\n", resTime)
}
```