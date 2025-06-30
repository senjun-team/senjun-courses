# Глава 13. Интерфейсы

## Понятие интерфейса 

До сих пор мы имели дело с конкретными типами. Интерфейс — это *абстрактный тип*. Интерфейс представляет собой соглашение о том, какие методы должны быть реализованы конкретным типом. Если конкретный тип реализует эти методы, то его экземпляры являются экземплярами интерфейса. Другими словами, такой тип *соответствует* или *удовлетворяет* интерфейсу. В примере ниже объявлен интерфейс `equipmentPrinter`. Структуры `computer` и `monitor` реализуют метод `sprintf` этого интерфейса, поэтому экземпляры этих структур являются экземплярами интерфейса `equipmentPrinter`. Это означает, что теперь мы можем передавать `computer` и `monitor` в качестве аргументов функциям `showEquipment` и `saveEquipment`: 

```go {.example_for_playground}
package main

import "fmt"

type computer struct {
	opertionSystem string
	ramInGb        int
	coresNumber    int
	cpu            string
	company        string
}

type monitor struct {
	size    [2]int
	company string
}

func (c computer) sprintf() (s string) {
	return fmt.Sprintf("Computer: %s OS, %d RAM, %d cores, %s, %s",
		c.opertionSystem, c.ramInGb, c.coresNumber, c.cpu, c.company)
}

func (m monitor) sprintf() (s string) {
	return fmt.Sprintf("Monitor: %dx%d, %s",
		m.size[0], m.size[1], m.company)
}

type equipmentPrinter interface {
	sprintf() (s string)
}

func showEquipment(eq equipmentPrinter) {
	fmt.Println(eq.sprintf())
}

func saveEquipment(eqs []string,
	eq equipmentPrinter) []string {
	eqs = append(eqs, eq.sprintf())
	return eqs
}

func main() {
	c := computer{"Ubuntu", 16, 8, "Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz", "Dell"}
	m := monitor{[2]int{1280, 1024}, "Dell"}
	showEquipment(c)
	showEquipment(m)
	var equipment []string
	equipment = saveEquipment(equipment, c)
	equipment = saveEquipment(equipment, m)
	fmt.Println("\nAll equipments:")
	for id, eq := range equipment {
		fmt.Printf("%d. %s\n", id+1, eq)
	}
}
```
```
Computer: Ubuntu OS, 16 RAM, 8 cores, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell
Monitor: 1280x1024, Dell

All equipments:
1. Computer: Ubuntu OS, 16 RAM, 8 cores, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell
2. Monitor: 1280x1024, Dell
```

Указывать факт того, что функция реализует интерфейс явным образом, нет необходимости. В Go используется так называемая «утиная типизация»: «если что-то крякает как утка, то это утка». Это означает, что если тип реализует все методы интерфейса, то он удовлетворяет интерфейсу.

Отметим также, что название интерфейса `equipmentPrinter` начинается со строчной буквы. Так мы поступили здесь, поскольку весь код находится в единственном пакете `main`. Однако чаще всего это не так, и имена интерфейсов начинаются с прописной буквы, чтобы они были доступны из других пакетов. То же самое относится и к методам, которые требует реализовать интерфейс.
