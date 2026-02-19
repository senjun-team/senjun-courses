# Глава 19. Управление параллельным исполнением

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

Следующий пример демонстрирует использование `sync.WaitGroup`. Программа должна обработать матрицу оборудования размером 1 млн. элементов. Каждый элемент представляет собой структуру из имени оборудования и признака того, что оно валидно. Допустимым именем оборудования является любая строка, в которой отсутствуют символы `"!@#$%^&*()"`. В противном случае имя является недопустимым. Необходимо проставить признак допустимости имени для каждого элемента. 

Программа выполняется в двух режимах: последовательном и параллельном. В случае параллельного режима мы сначала запрашиваем число ядер через функцию `runtime.NumCPU()`. Число горутин определяется количеством ядер. Поделив матрицу на части, мы запускаем по горутине на каждую часть. Переменная `wg` типа `sync.WaitGroup` используется в качестве счетчика горутин. 

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

Время выполнения будет немного варьироваться от запуска к запуску. 

В данном примере мы запускаем по одной горутине на ядро, поскольку это CPU-интенсивная задача. Ядра не будут простаивать в процессе ее выполнения. В противоположность к таким задачам, существуют I/O-bound задачи, связанные с вводом-выводом и задачи с ожиданием. Для них есть смысл запускать большее число горутин. Это актуально, например, для веб-сервера. 

Допустим, на веб-сервер с `4` ядрами приходит `10 000` одновременных запросов. Если бы мы создавали потоки ОС под каждый запрос, ОС бы просто «упала» под такой нагрузкой из-за огромного потребления памяти и времени переключения контекста. В Go под каждый запрос создают свою горутину. У нас `10 000` горутин, но всего `4` потока ОС. Почти все эти `10 000` горутин большую часть времени ждут: чтения сокета, записи в сокет, чтения из БД. Пока они ждут, планировщик постоянно переключает готовые к работе горутины на доступные `4` потока.

Также отметим один важный момент. Функция `runtime.NumCPU` подсчитывает не только количество физических ядер, но и количество логических. Для некоторых задач нужно знать именно число физических ядер, потому что распараллеливать задачи по логическим не имеет смысла. Иногда это может даже замедлить работу. Например, для тяжелых математических расчетов, имеющих большое количество операций с плавающей точкой. 

Такие задачи плохо запускать параллельно на ядрах, которые имеют только один блок для операций с плавающей точкой (FPU). Потоки запущенные, на одном ядре, будут конкурировать за FPU.

На момент написания курса, узнать количество физических ядер на чистом Go было непростой задачей. К счастью, есть сторонние библиотеки, которые позволяют это сделать. Например, с помощью библиотеки [gopsutil](https://github.com/shirou/gopsutil): 

```go
package main

import (
	"fmt"

	"github.com/shirou/gopsutil/cpu"
)

func main() {
	cpuNumber, err := cpu.Counts(false)
	if err != nil {
		panic(err)
	}
	fmt.Printf("cpuNumber = %d", cpuNumber)
}
```
```
cpuNumber = 6
```

Понятное дело, что вывод индивидуален, в зависимости от машины.

Бывает сложно сказать, может ли задача быть выполнена параллельно на логических ядрах. Проще всего провести эксперимент с числом горутин по количеству физических ядер, а затем — по количеству всех ядер. Выбрать стоит тот вариант, который работает быстрее. 

## Ключевое слово select 
## Захват глобальных переменных горутиной
## Мьютекс
