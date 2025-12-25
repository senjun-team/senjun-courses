```go
/* TODO
* select
* захват глобальных переменных горутиной - см. тг 
*/
```
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

Горутина имеет стек переменного размера. Это повышает ее эффективность. Она начинает выполняться с небольшим стеком. Как правило, 2 Кбайта. Такой стек, аналогично стеку потока операционной системы, хранит локальные переменные вызовов функций. Он увеличивается, по мере необходимости. Его размер может достигать 1 Гбайта. 

Потоки операционной системы планируются в ее ядре. Процессор периодически прерывается по таймеру, и запускается планировщик. Планировщик приостанавливает поток, сохраняет значения его регистров в памяти и решает, какой из потоков запустить следующим. Для этого он восстанавливает регистры данного потока и возобновляет его выполнение. Все это происходит медленно, поскольку требуется многократное обращение к памяти, обладающей слабой локальностью. Слабая локальность означает, что вероятность повторного обращения к те же ячейкам памяти невелика. Вероятность обращения к ячейкам памяти, расположенным рядом, также небольшая. 

Среда выполнения Go имеет свой планировщик. Он мультиплексирует выполнение `m` горутин на `n` потоках операционной системы. Этот метод называется **m:n планирование**. Планировщик Go работает аналогично планировщику операционной системы, но его задания касаются только горутин единственной программы на Go. 

Планировщик Go вызывается не по таймеру, а неявными инструкциями языка Go. Он работает быстро, потому что у него нет необходимости обращаться к ядру операционной системы и управлять потоками.

Планировщик Go использует параметр с именем **GOMAXPROCS**. Этот параметр содержит максимальное количество потоков, которое можно использовать для одновременного запуска горутин. До Go 1.25 этот параметр равнялся количеству логических ЦП на машине. Начиная с версии Go 1.25, разработчики приняли во внимание контейнерную среду некоторых программ. Для программ в контейнере теперь [учитываются](https://go.dev/blog/container-aware-gomaxprocs) ограничения этого контейнера. Если программа на Go выполняется внутри контейнера с ограничением ресурсов ЦП, GOMAXPROCS по умолчанию соответствует этому ограничению. Таким образом, GOMAXPROCS оказывается меньше числа реальных ЦП. Иногда переменную окружения GOMAXPROCS устанавливают явно, либо для этого вызывают функцию [runtime.GOMAXPROCS](https://pkg.go.dev/runtime#GOMAXPROCS) из кода.

Важной особенностью горутин является то, что  они не имеют идентификатора, который может быть получен как обычное значение. Он недоступен программисту. Это было сделано разработчиками Go сознательно. В противном случае могли бы возникнуть зависимости функции не только от ее аргументов. Она могла бы зависеть и от идентификатора потока, в котором функция выполняется. Разработчики Go уверяют, что это не нужно и поощряют простоту в написании программ.

## Каналы 

Каналы дают возможность горутинам «общаться» между собой. Они позволяют одной горутине передать какое-либо значение другой горутине. Канал передает данные определенного типа — **типа элементов канала.** 

Каналы бывают **буферизованные** и **небуферизованные**. Другими словами, с буфером и без буфера. Чтобы создать небуферизованный канал, нужно воспользоваться встроенной функцией `make`. Например, для значений типа `bool`:

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

Канал закрывают, если по этому каналу больше не будут передаваться значения. Закрывать канал каждый раз нет необходимости. Ресурсы недоступного канала освобождаются сборщиком мусора автоматически. Канал закрывают только тогда, когда это нужно по логике программы. Например, чтобы сообщить горутине, что все данные переданы. 

Попытка закрыть уже закрытый канал приведет к панике: 

```go {.example_for_playground}
package main

func main() {
	ch := make(chan bool)
	close(ch)
	close(ch)
}
```
```
panic: close of closed channel
```

Попытка отправить значение в закрытый канал также приведет к панике:

```go {.example_for_playground}
package main

func main() {
	ch := make(chan bool)
	close(ch)
	ch <- true
}
```
```
panic: send on closed channel
```

При чтении из закрытого небуферизованного канала вы получите значение типа по умолчанию.

Иногда нужно точно знать, получили мы реальное значение или нет. Например, при получении значения из закрытого небуферизованного канала никакого значения прочитано из него не будет. На самом деле, при чтении из канала возвращается два значения. В этом случае используют следующую конструкцию: 

```go
val, ok := <-ch
```
Если `ok` равен `true`, то значение прочитано. В противном случае — нет.

Каналы являются ссылочным типом. Они представляют собой ссылку на некоторую структуру данных. Их можно сравнивать. Если два канала ссылаются на одну и ту же структуру данных, то результат сравнения — `true`. В противном случае — `false`:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	ch := make(chan bool)
	ch2 := ch
	fmt.Println(ch == ch2)
}
```
```
true
```

```go {.example_for_playground}
package main

import "fmt"

func main() {
	ch := make(chan bool)
	ch2 := make(chan bool)
	fmt.Println(ch == ch2)
}
```
```
false
```

Также допустимо сравнивать каналы с `nil`.

Небуферизованные каналы удобно использовать для синхронизации работы горутин. Пока горутина ожидает получения значения из канала, она блокируется. Аналогично блокируется отправитель, пока значение из канала не будет прочитано. Используйте эту идею для решения следующего задания. {.task_text}

Некоторая задача `task` выполняется на сервере `server`. Сервер не запущен все время. Он запускается по мере необходимости. На старт сервера затрачивается некоторое время. Для выполнения задачи нужно подготовить данные. Эта операция также выполняется не сразу. {.task_text}

Код ниже выполняется последовательно. Модифицируйте его таким образом, чтобы он работал параллельно. Данные должны готовиться во время того, как уже стартует сервер. В отладочных сообщениях вы должны увидеть следующий текст: {.task_text}

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

Используйте небуфиризованные каналы типа `struct{}`. Передача значения в такой канал будет сигнализировать о том, что работа выполнена. Не забудьте, что `main` — это тоже горутина. Она не будет ждать выполнения всех других горутин, если не организовать такое поведение явно. {.task_hint}

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
	go task.job()
	go server.job()
	<-done
}
```

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

```go {.task_source #golang_chapter_0160_task_0010}
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

## Ключевое слово select 

## Утечка горутин 

Важно помнить, что сборщик мусора не занимается тем, чтобы останавливать «зависшие» горутины. Вы должны сами позаботиться о том, чтобы каждая горутина успешно завершилась. В противном случае возникнет утечка памяти. 

Следующий пример демонстрирует утечку горутин. Мы запускаем 10 горутин для асинхронной обработки сообщений. Результат обработки сообщений отправляется в канал уведомлений, откуда их читает другая горутина. Проблема в том, что «горутины» зависают. Возникает утечка памяти. {.task_text}  

Исправьте ошибку в коде. Обработчики сообщений должны завершиться. Количество горутин вначале и в конце должно быть одинаковым. {.task_text}  

```go {.task_source #golang_chapter_0160_task_0020}
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
		select {
		case notificationChan <- result:
			// Успешно отправили
		case <-time.After(1 * time.Second):
			// Таймаут
			fmt.Println("Timeout message!")

		}
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
	select {
	case notificationChan <- result:
		// Успешно отправили
	case <-time.After(1 * time.Second):
		// Таймаут
		fmt.Println("Timeout message!")

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

## Резюме