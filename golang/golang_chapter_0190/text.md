# Глава 19. Управление параллельным исполнением

В этой главе мы познакомимся со счетчиком `sync.WaitGroup`. Он позволяет дождаться окончания работы нескольких горутин. До сих пор мы использовали для этого каналы. Счетчик `sync.WaitGroup` предоставляет более удобные средства для этих целей. 

Мы затронем ключевое слово `select`. Это слово позволяет выбрать результат работы той горутины, которая выполнится раньше.

Мы поговорим про мьютекс чтения/записи `sync.RWMutex`. Он позволяет существенно ускорить работу некоторых программ.

Напоследок рассмотрим проблему захвата горутиной глобальных переменных. Начинающие разработчики часто сталкиваются с ней. Бывает сложно найти и исправить ошибки, связанные с таким захватом, если не знать про эту особенность.

## Счетчик sync.WaitGroup

Когда запущено несколько горутин, бывает удобно использовать счетчик горутин. Каждая горутина должна при старте увеличить счетчик на единицу и уменьшить его после завершения. Когда счетчик вновь окажется равным нулю, то это значит, что все запущенные горутины завершили свою работу. Такой счетчик в Go называется `sync.WaitGroup`. Для работы с ним сначала нужно импортировать пакет `sync`, а потом объявить переменную данного типа:

```go
var wg sync.WaitGroup
```

Затем с помощью метода `Add` счетчику сообщают о том, на какое значение его нужно увеличить. Необязательно передавать методу `Add` единицу перед запуском каждой горутины. Бывает удобно сразу передать в `Add` общее количество горутин, которые будут запущены. Например, для пяти горутин:

```go
wg.Add(5)
```

После того как горутина выполнится, она должна вызвать метод `wg.Done`. Обычно это делают с помощью `defer`, чтобы гарантировать вызов `wg.Done`, даже в случае ошибки: 

```go
defer wg.Done()
```

Метод `wg.Wait` ожидает выполнения всех горутин: 

```go
wg.Wait()
```

Метод `Add` должен быть вызван перед запуском горутин, а не внутри них! В противном случае мы не можем быть уверены, что вызвали его до метода `Wait`.

Следующий пример демонстрирует использование `sync.WaitGroup`. Программа должна обработать матрицу оборудования размером *1 млн.* элементов. Каждый элемент представляет собой структуру из имени оборудования и признака того, что оно валидно. Допустимым именем оборудования является только строка, состоящая из букв, цифр и символа пробела. Необходимо проставить признак допустимости имени для каждого элемента. 

Программа выполняется в двух режимах: последовательном и параллельном. В случае параллельного режима мы сначала запрашиваем число ядер через функцию `runtime.NumCPU()`. Число горутин определяется количеством ядер. Поделив матрицу на части, мы запускаем по горутине на каждую часть. Переменная `wg` типа `sync.WaitGroup` используется в качестве счетчика горутин. 

Обратите внимание на время выполнения последовательной и параллельной версий. 

```go {.example_for_playground}
package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"sync"
	"time"
	"unicode"
)

const elementsNumber = 1000000
const (
	// iota присваивает целые значения
	// константам: 0, 1, 2...
	unknown = iota
	valid
	invalid
)
const letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
const digits = "0123456789"
const invalidSymbols = "!@#$%^&*()"
const allSymbols = letters + digits + invalidSymbols

type equipmentT struct {
	name    string
	isValid int
}

// Функция randName генерирует случайное имя оборудования
func randName(r *rand.Rand, n int) string {
	b := make([]byte, n)
	for i := range b {
		b[i] = allSymbols[r.Intn(len(allSymbols))]
	}
	return string(b)
}

// Функция worker проверяет свою часть матрицы
func worker(input []equipmentT) {
	for i := range input {
		input[i].isValid = check(input[i].name)
	}
}

// Функция check проверяет имя оборудования
func check(name string) int {
	for _, r := range name {
		if !(unicode.IsLetter(r) ||
			unicode.IsDigit(r) ||
			r == ' ') {
			return invalid
		}
	}
	return valid
}

/*
Функция statisticPrintln печатает
статистику по матрице:
validNumber - число валидных имен
invalidNumber - число невалидных имен
unknownNumber - число непроверенных имен
*/
func statisticPrintln(result []equipmentT) {
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
	// Подготовка данных
	// Матрица для последовательных вычислений
	equipmentMatrix := [elementsNumber]equipmentT{}
	// Матрица для параллельных вычислений
	workerEquipmentMatrix := [elementsNumber]equipmentT{}
	seed := int64(42)
	source := rand.NewSource(seed)
	r := rand.New(source)
	for i := range elementsNumber {
		equipmentMatrix[i] = equipmentT{name: randName(r, 10),
			isValid: unknown}
		workerEquipmentMatrix[i] = equipmentMatrix[i]
	}
	// Последовательная часть программы
	start := time.Now()
	for i := range equipmentMatrix {
		equipmentMatrix[i].isValid = check(equipmentMatrix[i].name)
	}
	resTime := time.Since(start).Milliseconds()
	fmt.Println("equipmentMatrix:")
	statisticPrintln(equipmentMatrix[:])
	fmt.Printf("time: %d milliseconds\n", resTime)
	fmt.Println()
	// Параллельная часть программы
	workersNumber := runtime.NumCPU()
	// math.Ceil округляет значение в большую сторону
	chunk := int(math.Ceil(float64(elementsNumber) / float64(workersNumber)))
	// ЗДЕСЬ работаем с sync.waitGroup
	var wg sync.WaitGroup
	wg.Add(workersNumber)
	minIdx := 0
	maxIdx := minIdx + chunk
	maxIdx = min(elementsNumber, maxIdx)
	start = time.Now()
	for range workersNumber {
		go func(minIdx int, maxIdx int) {
			defer wg.Done()
			worker(workerEquipmentMatrix[minIdx:maxIdx])
		}(minIdx, maxIdx)
		minIdx = maxIdx
		maxIdx = minIdx + chunk
		maxIdx = min(elementsNumber, maxIdx)
	}
	wg.Wait()
	resTime = time.Since(start).Milliseconds()
	fmt.Println("workerEquipmentMatrix:")
	statisticPrintln(workerEquipmentMatrix[:])
	fmt.Printf("time: %d milliseconds\n", resTime)
}
```
```
equipmentMatrix:
validNumber=224424, invalidNumber=775576, unknownNumber=0
time: 58 milliseconds

workerEquipmentMatrix:
validNumber=224424, invalidNumber=775576, unknownNumber=0
time: 12 milliseconds
```

Время выполнения будет немного варьироваться от запуска к запуску. 

В данном примере мы запускаем по одной горутине на ядро, поскольку это CPU-bound задача. Ядра не будут простаивать в процессе ее выполнения. В противоположность к таким задачам, существуют IO-bound задачи, связанные с вводом-выводом, и задачи с ожиданием. Для них есть смысл запускать большее число горутин. Это актуально, например, для веб-сервера. 

Допустим, на веб-сервер с `4` ядрами приходит `10 000` одновременных запросов. Если бы мы создавали потоки ОС под каждый запрос, ОС бы просто «упала» под такой нагрузкой из-за огромного потребления памяти и времени переключения контекста. В Go под каждый запрос создают свою горутину. У нас `10 000` горутин, но всего `4` потока ОС. Почти все эти `10 000` горутин большую часть времени ждут: чтения сокета, записи в сокет, чтения из БД. Пока они ждут, планировщик постоянно переключает готовые к работе горутины на доступные `4` потока.

Также отметим один важный момент. Функция `runtime.NumCPU` подсчитывает не только количество физических ядер, но и количество логических. Для некоторых задач нужно знать именно число физических ядер, потому что распараллеливать задачи по логическим не имеет смысла. Иногда это может даже замедлить работу. Например, для тяжелых математических расчетов, имеющих большое количество операций с плавающей точкой. 

Такие задачи плохо запускать параллельно на ядрах, которые имеют только один [блок для операций с плавающей точкой (FPU).](https://ru.wikipedia.org/wiki/%D0%9C%D0%B0%D1%82%D0%B5%D0%BC%D0%B0%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D0%B9_%D1%81%D0%BE%D0%BF%D1%80%D0%BE%D1%86%D0%B5%D1%81%D1%81%D0%BE%D1%80) Потоки, запущенные на одном ядре, будут конкурировать за FPU.

На момент написания курса, узнать количество физических ядер на чистом Go было непростой задачей. К счастью, есть сторонние библиотеки, которые позволяют это сделать. Например, с помощью библиотеки [gopsutil](https://github.com/shirou/gopsutil): 

```go
package main

import (
	"fmt"
	"log"

	"github.com/shirou/gopsutil/cpu"
)

func main() {
	// Для получения числа
	// физических ядер - false.
	// Для получения логических
	// с учетом Hyper-Threading - true.
	cpuNumber, err := cpu.Counts(false)
	if err != nil {
		// печатаем ошибку в лог
		// и завершаем программу
		// с кодом 1
		log.Fatal(err)
	}
	fmt.Printf("cpuNumber = %d\n", cpuNumber)
}
```
```
cpuNumber = 6
```

Бывает сложно сказать, может ли задача быть выполнена параллельно на логических ядрах. Проще всего провести эксперимент с числом горутин по количеству физических ядер, а затем — по количеству всех ядер. Выбрать стоит тот вариант, который работает быстрее. 

## Ключевое слово select 

Иногда необходимо выбрать результат работы той горутины среди всех горутин, которая завершится раньше.

Рассмотрим пример. Допустим, у нас есть некоторая тяжелая задача `slowTask`. Мы хотим ждать ее выполнения лишь до некоторого таймаута. Мы могли бы написать такой код:

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func slowTask(timeout time.Duration) (string, error) {
	resultCh := make(chan string, 1)
	// Запускаем операцию
	go func() {
		time.Sleep(1 * time.Second)
		resultCh <- "OK"
	}()

	// Ждем таймаут
	time.Sleep(timeout)

	// Проверяем, есть ли данные в канале - 
	// неблокирующая проверка
	if len(resultCh) > 0 {
		return <-resultCh, nil
	}
	return "", fmt.Errorf("timeout")
}

func main() {
	result, err := slowTask(1 * time.Millisecond)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Result:", result)
	}
}
```
```
Error: timeout
```

Но это неудобно! Код внутри функции становится сложным и запутанным. Кроме того, мы гарантированно ждем таймаут. А если работа будет выполнена раньше этого таймаута? Получится простой. Чтобы избежать такой проблемы, удобно использовать конструкцию `select-case`: 

```go {.example_for_playground}
package main

import (
	"fmt"
	"time"
)

func slowTask() <-chan string {
	ch := make(chan string, 1)
	go func() {
		time.Sleep(1 * time.Second)
		ch <- "OK"
	}()
	return ch
}

func main() {
	resultCh := slowTask()

	select {
	case res := <-resultCh:
		fmt.Println("Res:", res)
	case <-time.After(1 * time.Millisecond):
		fmt.Println("Error: timeout")
	}
}
```
```
Error: timeout
```

Внутри конструкции `select` будет выполнен тот `case`, для которого результат из канала придет раньше. В общем случае конструкция имеет такой вид: 

```
select{
	case <v1> := <-ch1:
		...
	case <v2> := <-ch2:
		...
	...
	default:
		...
}
```

Здесь в треугольных скобках `<>` обозначены необязательные переменные. Переменные используются только тогда, когда они нужны. Если переменная не нужна, то конструкция выглядит таким образом: 

```
select{
	case <-ch1:
		...
	case <-ch2:
		...
	default:
		...
}
```

Допустимо одновременно использовать часть конструкций `case` с переменной, а часть — без них.  
Слово `default` является необязательным. Этот случай срабатывает, если не были выполнены все предыдущие.  
Когда нужно досрочно прекратить выполнение `case`, пишут `break`:

```
select{
	case <-ch1:
		...
	case <-ch2:
		...
		// досрочный выход из select
		break
		...
	default:
		...
}
```

В представленной задаче функция `newServer` создает новый сервер с именем `name` и адресом `url`.  Функция `monitorServer` производит мониторинг сервера. В зависимости от состояния сервера она записывает сообщения в штатный канал, канал алертов, либо канал с признаком окончания мониторинга. Функция `statistics` должна собрать все сообщения в единый канал `resultCh`. Функция `analyze` запускает на мониторинг три сервера и собирает статистику по ним с помощью функции `stat`. {.task_text}

Допишите функцию `statistics`. {.task_text}

Функция `statistics` должна писать в следующем формате. {.task_text}

Для штатных сообщений: {.task_text}  

<имя сервера>:<сообщение> {.task_text}

Для алертов: {.task_text}

<имя сервера>:<уровень критичности>:<сообщение>   {.task_text}  

Например: {.task_text}

api-server-01:129ms  {.task_text}  

db-server-01:CRITICAL:not responding  {.task_text}  

Переноса строки в конце ставить не нужно. {.task_text}

```go {.task_source #golang_chapter_0190_task_0010}
package main

import (
	"fmt"
	"math/rand"
	"sync"
)

// Фиксируем сид для повторяемости результатов
const seed = 42

type serverT struct {
	name          string
	url           string
	healthCh      chan string   // Штатные сообщения о работе
	alertCh       chan alertT   // Ошибки и предупреждения
	monitorStopCh chan struct{} // Признак того, что мониторинг завершен
}

type alertT struct {
	severity string // Уровень критичности
	message  string // Сообщение
}

func newServer(name string, url string) *serverT {
	return &serverT{name: name, url: url,
		healthCh:      make(chan string),
		alertCh:       make(chan alertT),
		monitorStopCh: make(chan struct{}, 1)}
}

func monitorServer(server *serverT) {
	const checkNumber = 10
	r := rand.New(rand.NewSource(int64(seed)))
	for range checkNumber {
		// Имитация проверки здоровья сервера
		latency := r.Intn(200)
		isDown := r.Intn(100) < 10 // 10% вероятность сбоя
		if isDown {
			server.alertCh <- alertT{
				severity: "CRITICAL",
				message:  "not responding",
			}
		} else if latency > 150 {
			server.alertCh <- alertT{
				severity: "WARNING",
				message:  fmt.Sprintf("high latency:%dms", latency),
			}
		} else {
			server.healthCh <- fmt.Sprintf("%dms", latency)
		}
	}
	server.monitorStopCh <- struct{}{}
}

func statistics(servers []*serverT, resultCh chan<- string) (res []string) {
	var wg sync.WaitGroup
	wg.Add(len(servers))
	for _, server := range servers {
		go func(server *serverT) {
			defer wg.Done()
			// Ваш код здесь 
		}(server)
	}
	wg.Wait()
	close(resultCh)
	return res
}

func analyze(stat func([]*serverT, chan<- string) []string) (res []string) {
	servers := []*serverT{
		newServer("api-server-01", "http://192.168.23.46:8080"),
		newServer("db-server-01", "http://192.168.23.47:5432"),
		newServer("cache-server-01", "http://192.168.23.48:6379"),
	}
	const buffSize = 100
	resultCh := make(chan string, buffSize)
	// Запускаем мониторинг для каждого сервера
	for _, server := range servers {
		go monitorServer(server)
	}
	go stat(servers, resultCh)
	for msg := range resultCh {
		res = append(res, msg)
	}
	return
}

func main() {
	for _, res := range analyze(statistics) {
		fmt.Println(res)
	}
}
```

Используйте `select-case` внутри бесконечного цикла, чтобы записывать те сообщения, которые придут раньше. В случае, если придет признак конца мониторинга, прервите цикл. Помните, что `break` внутри `select-case` выполняет свою задачу. Он не прервет цикл. Чтобы выйти из цикла, воспользуйтесь `break` по метке. {.task_hint}

```go {.task_answer}
package main

import (
	"fmt"
	"math/rand"
	"sync"
)

// Фиксируем сид для повторяемости результатов
const seed = 42

type serverT struct {
	name          string
	url           string
	healthCh      chan string   // Штатные сообщения о работе
	alertCh       chan alertT   // Ошибки и предупреждения
	monitorStopCh chan struct{} // Признак того, что мониторинг завершен
}

type alertT struct {
	severity string // Уровень критичности
	message  string // Сообщение
}

func newServer(name string, url string) *serverT {
	return &serverT{name: name, url: url,
		healthCh:      make(chan string),
		alertCh:       make(chan alertT),
		monitorStopCh: make(chan struct{}, 1)}
}

func monitorServer(server *serverT) {
	const checkNumber = 10
	r := rand.New(rand.NewSource(int64(seed)))
	for range checkNumber {
		// Имитация проверки здоровья сервера
		latency := r.Intn(200)
		isDown := r.Intn(100) < 10 // 10% вероятность сбоя
		if isDown {
			server.alertCh <- alertT{
				severity: "CRITICAL",
				message:  "not responding",
			}
		} else if latency > 150 {
			server.alertCh <- alertT{
				severity: "WARNING",
				message:  fmt.Sprintf("high latency:%dms", latency),
			}
		} else {
			server.healthCh <- fmt.Sprintf("%dms", latency)
		}
	}
	server.monitorStopCh <- struct{}{}
}

func statistics(servers []*serverT, resultCh chan<- string) (res []string) {
	var wg sync.WaitGroup
	wg.Add(len(servers))
	for _, server := range servers {
		go func(server *serverT) {
			defer wg.Done()
		loop:
			for {
				select {
				case health := <-server.healthCh:
					resultCh <- fmt.Sprintf("%s:%s",
						server.name,
						health)
				case alert := <-server.alertCh:
					resultCh <- fmt.Sprintf("%s:%s:%s",
						server.name,
						alert.severity,
						alert.message)
				case <-server.monitorStopCh:
					break loop
				}
			}
		}(server)
	}
	wg.Wait()
	close(resultCh)
	return res
}

func analyze(stat func([]*serverT, chan<- string) []string) (res []string) {
	servers := []*serverT{
		newServer("api-server-01", "http://192.168.23.46:8080"),
		newServer("db-server-01", "http://192.168.23.47:5432"),
		newServer("cache-server-01", "http://192.168.23.48:6379"),
	}
	const buffSize = 100
	resultCh := make(chan string, buffSize)
	// Запускаем мониторинг для каждого сервера
	for _, server := range servers {
		go monitorServer(server)
	}
	go stat(servers, resultCh)
	for msg := range resultCh {
		res = append(res, msg)
	}
	return
}

func main() {
	for _, res := range analyze(statistics) {
		fmt.Println(res)
	}
}
```

## Мьютекс чтения/записи sync.RWMutex

Мы говорили о мьютексах [ранее](/courses/golang/chapters/golang_chapter_0170/#block-mutex). Как уже было сказано, мьютексы защищают данные, предотвращая состояние гонки. Примитив синхронизации `sync.Mutex` блокирует доступ к данным для всех других горутин, пока текущая горутина не завершит свою работу с этими данными. Однако это не всегда хорошо. Что, если `10` горутин одновременно читают данные? Если использовать `sync.Mutex`, то чтение окажется последовательным. В действительности эти `10` горутин не мешают друг другу. Может быть, тогда вообще обойтись без мьютекса? Это правильное решение в случае, когда никто эти данные не пишет. В случае, когда есть писатель, хотелось бы одновременно читать, но блокировать горутину на запись. Примитив `sync.RWMutex` решает эту задачу.

Переменные типа `sync.RWMutex` имеют те же методы, что и `sync.Mutex`, а также еще два: 
* `RLock`. Блокирует всех писателей, однако не блокирует других читателей.
* `RUnlock`. Снимает блокировку `Rlock`.


Примитив синхронизации `sync.RWMutex` выгоднее, так как множественные чтения могут выполняться параллельно. Чтобы убедиться в этом, рассмотрим пример. В нем метод `get` работает с двумя типам блокировок: `Lock` и `Rlock`. Обратите внимание на время выполнения в каждом из двух случаев. Чем больше читателей и меньше писателей, тем больше выигрыш по производительности.

```go {.example_for_playground}
package main

import (
	"fmt"
	"sync"
	"time"
)

type cache struct {
	data  map[string]string
	mutex sync.RWMutex
}

func (c *cache) get(key string, useRW bool) string {
	if useRW {
		c.mutex.RLock()
		defer c.mutex.RUnlock()
	} else {
		c.mutex.Lock()
		defer c.mutex.Unlock()
	}
	// имитация долгой работы
	time.Sleep(1 * time.Millisecond)
	return c.data[key]
}

func (c *cache) set(key, value string) {
	c.mutex.Lock()
	defer c.mutex.Unlock()
	// имитация долгой работы
	time.Sleep(1 * time.Millisecond)
	c.data[key] = value
}

func benchmark(name string, readers, writers, ops int,
	useRW bool) time.Duration {
	const readSize = 10
	var wg sync.WaitGroup
	c := &cache{data: make(map[string]string), mutex: sync.RWMutex{}}
	start := time.Now()
	wg.Add(readers)
	for i := 0; i < readers; i++ {
		go func() {
			defer wg.Done()
			for j := 0; j < ops; j++ {
				c.get(fmt.Sprintf("key%d", j%readSize), useRW)
			}
		}()
	}
	wg.Add(writers)
	for i := 0; i < writers; i++ {
		go func(id int) {
			defer wg.Done()
			for j := 0; j < ops; j++ {
				c.set(fmt.Sprintf("key%d", id), "value")
			}
		}(i)
	}
	wg.Wait()
	return time.Since(start)
}

func main() {
	// Количество операций на одну горутину
	const ops = 30
	fmt.Println("=== Сравнение RWMutex vs Mutex ===\n")
	// Сценарий 1: много читателей (80%), мало писателей (20%)
	fmt.Println("1. Много читателей (80%), мало писателей (20%):")
	rwTime := benchmark("RWMutex", 8, 2, ops, true)
	mutexTime := benchmark("Mutex", 8, 2, ops, false)
	fmt.Printf("   RWMutex: %d milliseconds\n",
		rwTime.Milliseconds())
	fmt.Printf("   Mutex:   %d milliseconds\n",
		mutexTime.Milliseconds())
	fmt.Printf("   RWMutex быстрее в %.2f раза\n\n",
		float64(mutexTime.Milliseconds())/
			float64(rwTime.Milliseconds()))
	// Сценарий 2: равное количество (50%/50%)
	fmt.Println("2. Равное количество (50% чтений, 50% записей):")
	rwTime = benchmark("RWMutex", 5, 5, ops, true)
	mutexTime = benchmark("Mutex", 5, 5, ops, false)
	fmt.Printf("   RWMutex: %d milliseconds\n",
		rwTime.Milliseconds())
	fmt.Printf("   Mutex:   %d milliseconds\n",
		mutexTime.Milliseconds())
	fmt.Printf("   RWMutex быстрее в %.2f раза\n\n",
		float64(mutexTime.Milliseconds())/
			float64(rwTime.Milliseconds()))
	// Сценарий 3: много писателей (80%), мало читателей (20%)
	fmt.Println("3. Много писателей (80%), мало читателей (20%):")
	rwTime = benchmark("RWMutex", 2, 8, ops, true)
	mutexTime = benchmark("Mutex", 2, 8, ops, false)
	fmt.Printf("   RWMutex: %d milliseconds\n",
		rwTime.Milliseconds())
	fmt.Printf("   Mutex:   %d milliseconds\n",
		mutexTime.Milliseconds())
	fmt.Printf("   RWMutex быстрее в %.2f раза\n\n",
		float64(mutexTime.Milliseconds())/
			float64(rwTime.Milliseconds()))
}
```
```
=== Сравнение RWMutex vs Mutex ===

1. Много читателей (80%), мало писателей (20%):
   RWMutex: 100 milliseconds
   Mutex:   349 milliseconds
   RWMutex быстрее в 3.49 раза

2. Равное количество (50% чтений, 50% записей):
   RWMutex: 209 milliseconds
   Mutex:   339 milliseconds
   RWMutex быстрее в 1.62 раза

3. Много писателей (80%), мало читателей (20%):
   RWMutex: 311 milliseconds
   Mutex:   344 milliseconds
   RWMutex быстрее в 1.11 раза
```

Вывод будет немного отличаться от запуска к запуску.  
Пока мы не рассматривали, как правильно делать бенчмарки. Для единообразия запуска в плейграунде мы сделали их прямо в `main`. 

Что выведет следующий код? В случае ошибки напишите `error`. {.task_text}

```go {.example_for_playground}
package main

import (
	"fmt"
	"sync"
)

type bankAccount struct {
	id    int
	name  string
	value int
	m     sync.RWMutex
}

func (b *bankAccount) Balance() int {
	b.m.RLock()
	defer b.m.Unlock()
	return b.value
}

func main() {
	var b bankAccount
	b.value = 10
	fmt.Println(b.Balance())
}
```

```consoleoutput {.task_source #golang_chapter_0190_task_0020}
```
Методу `RLock` всегда должен соответствовать метод `RUnlock`, а не просто `Unlock`. {.task_hint}

```{.task_answer}
error
```

## Захват глобальных переменных горутиной

Существует известная проблема захвата глобальных переменных горутиной. Рассмотрим пример. На момент написания курса проблема все еще существовала. Для проверки этого кода использовался компилятор Go 1.26.0. Возможно, что на более новых версиях поведение будет изменено.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	i := 0
	ch := make(chan int, 1)
	ch <- 5
	go func() {
		ch <- i
	}()

	time.Sleep(100 * time.Millisecond)
	i += 1
	fmt.Println(<-ch)
	fmt.Println(<-ch)
}
```
```
5
1
```

Вы возможно ожидаете увидеть в выводе `5` и `0`. Однако горутина захватила глобальную переменную `i`. В результате последующий инкремент повлиял на результат. Но это еще не все. Добавим внутри горутины к `i` значение `0`.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	i := 0
	ch := make(chan int, 1)
	ch <- 5
	go func() {
		ch <- i + 0 // добавили ноль!
	}()

	time.Sleep(100 * time.Millisecond)
	i += 1
	fmt.Println(<-ch)
	fmt.Println(<-ch)
}
```
```
5
0
```

Теперь внутри горутины создается локальная переменная, на основе глобальной `i`, поэтому мы получаем другой результат. Таким образом, сложно предсказать, каким будет поведение программы в результате захвата глобальной переменной. Поэтому всегда необходимо передавать горутине те переменные, которые она использует:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	i := 0
	ch := make(chan int, 1)
	ch <- 5
	// передаем i в горутину!
	go func(i int) {
		ch <- i
	}(i)

	time.Sleep(100 * time.Millisecond)
	i += 1
	fmt.Println(<-ch)
	fmt.Println(<-ch)
}
```
```
5
0
```

## Резюме

1. Счетчик `sync.WaitGroup` позволяет дождаться выполнения всех запущенных горутин. 
2. Ключевое слово `select` упрощает программный код, когда нужно выбрать результат работы наискорейшей горутины.
3. Мьютекс чтения/записи `sync.RWMutex` предоставляет более эффективные средства для множественного чтения.
4. Захват горутиной глобальных переменных часто приводит к ошибкам. Чтобы избежать этого, необходимо всегда передавать переменные в качестве параметров.
