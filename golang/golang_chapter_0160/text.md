# Глава 16. Горутины и каналы 

## Горутины

В Go программа исполняется как одна или несколько горутин. Горутина — это легковесный поток, который управляется рантаймом Go. Функция `main` также выполняется в горутине. До сих пор все наши программы выполнялись в единственной горутине. Чтобы запустить из `main` новую горутину, достаточно поставить перед вызовом функции ключевое слово `go`.

```go
package main

import (
	"fmt"
	"math/rand"
)

const (
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

func worker(input []int, min int, max int) {

}

func check(name string) {
	var invalidSymbols = []rune("!@#$%^&*()")
	for r := range name {

	}
}

func main() {
	const elementsNumber = 10000
	equipmentMatrix := [elementsNumber]equipmentT{}
	seed := int64(42)
	source := rand.NewSource(seed)
	r := rand.New(source)
	for i := range elementsNumber {
		equipmentMatrix[i] = equipmentT{name: randSeq(r, 10),
			isValid: unknown}
	}

	fmt.Println(equipmentMatrix)
}
```