# Глава 17. Управление параллельным исполнением

## Ключевое слово select 
## sync.WaitGroup

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

## Захват глобальных переменных горутиной