# Глава 13. Интерфейсы

## Понятие интерфейса 

До сих пор мы имели дело с конкретными типами. Интерфейс — это *абстрактный тип*. Абстрактный тип — это такой тип, который определяется не его внутренней реализацией, а только его поведением. Абстрактный тип описывает, что можно делать с данными, а не как они хранятся. 

Интерфейс представляет собой соглашение о том, какие методы должны быть реализованы конкретным типом. Если конкретный тип реализует эти методы, то его экземпляры являются экземплярами интерфейса. Другими словами, такой тип *соответствует* или *удовлетворяет* интерфейсу. 

Для того, чтобы объявить интерфейс, используют ключевое слово `interface`:
```
type <имя интерфейса> interface{
	<метод 1>	
	<метод 2>
	...
	<метод N>
}
```

В примере ниже объявлен интерфейс `equipmentPrinter`. Структуры `systemUnit` и `monitor` реализуют метод `sprintf` этого интерфейса, поэтому экземпляры этих структур являются экземплярами интерфейса `equipmentPrinter`. Это означает, что теперь мы можем передавать `systemUnit` и `monitor` в качестве аргументов функциям `showEquipment` и `saveEquipment`: 

```go {.example_for_playground}
package main

import "fmt"

type equipmentPrinter interface {
	sprintf() (s string)
}

type systemUnit struct {
	opertionSystem string
	ramInGb        int
	coresNumber    int
	cpu            string
	company        string
}

type monitor struct {
	resolution    [2]int
	company string
}

func (c systemUnit) sprintf() (s string) {
	return fmt.Sprintf("System unit: %s OS, %d RAM, %d cores, %s, %s",
		c.opertionSystem, c.ramInGb, c.coresNumber, c.cpu, c.company)
}

func (m monitor) sprintf() (s string) {
	return fmt.Sprintf("Monitor: %dx%d, %s",
		m.resolution[0], m.resolution[1], m.company)
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
	c := systemUnit{"Ubuntu", 16, 8, "Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz", "Dell"}
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
System unit: Ubuntu OS, 16 RAM, 8 cores, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell
Monitor: 1280x1024, Dell

All equipments:
1. System unit: Ubuntu OS, 16 RAM, 8 cores, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell
2. Monitor: 1280x1024, Dell
```

Вам не нужно явно указывать, что функция реализует какой-то интерфейс. В Go используется так называемая «утиная типизация»: «если что-то крякает как утка, то это утка». Это означает, что если тип реализует все методы интерфейса, то он удовлетворяет интерфейсу.

Отметим также, что имя интерфейса `equipmentPrinter` начинается со строчной буквы. Так мы поступили здесь, поскольку весь код находится в единственном пакете `main`. Однако чаще всего это не так, и имена интерфейсов начинаются с прописной буквы, чтобы они были доступны из других пакетов. То же самое относится и к методам, которые требует реализовать интерфейс.


Создайте интерфейс `equipmentParser` с единственным методом `parse(s string) error`. Реализуйте этот метод в структурах `systemUnit` и `monitor` так, чтобы эти структуры удовлетворяли интерфейсу. Метод должен разобрать значения полей структур по входной строке. Значения отделяются запятой. Не забудьте удалить пробельные символы вначале и в конце строк. Этого легко достичь с помощью метода `strings.TrimSpace()`.{.task_text}

Создайте функцию `parseAll`, которая принимает в качестве параметров срез из `equipmentParser` и срез из строк. Она должна разобрать все строки, которые ей передадут, в соответствующие аргументы. Функция возвращает ошибку `failed to parse`, либо `nil`. {.task_text}


```go {.task_source #golang_chapter_0130_task_0010}
package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

type equipmentPrinter interface {
	sprintf() (s string)
}

type systemUnit struct {
	opertionSystem string
	ramInGb        int
	coresNumber    int
	cpu            string
	company        string
}

type monitor struct {
	resolution    [2]int
	company string
}

func (c *systemUnit) sprintf() (s string) {
	return fmt.Sprintf("System unit: %s OS, %d RAM, %d cores, %s, %s",
		c.opertionSystem, c.ramInGb, c.coresNumber, c.cpu, c.company)
}

func (m *monitor) sprintf() (s string) {
	return fmt.Sprintf("Monitor: %dx%d, %s",
		m.resolution[0], m.resolution[1], m.company)
}

func showEquipment(eq equipmentPrinter) {
	fmt.Println(eq.sprintf())
}

func main() {
	c := &systemUnit{}
	m := &monitor{}
	err := parseAll([]equipmentParser{c, m},
		[]string{"Ubuntu OS, 16, 8, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell",
			"1280x1024, Dell"})
	if err != nil {
		fmt.Println(err)
	}
	showEquipment(c)
	showEquipment(m)
}

```

Чтобы получить из строки срез по разделителю, воспользуйтесь функцией `strings.Split`. Она принимает строку в качестве первого аргумента и разделитель — в качестве второго. Чтобы преобразовать строку к числу, воспользуйтесь функцией `strconv.Atoi`. Она вернет результат и ошибку. {.task_hint}

```go  {.task_answer}
package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

type equipmentPrinter interface {
	sprintf() (s string)
}

type equipmentParser interface {
	parse(s string) error
}

type systemUnit struct {
	opertionSystem string
	ramInGb        int
	coresNumber    int
	cpu            string
	company        string
}

type monitor struct {
	resolution    [2]int
	company string
}

func (c *systemUnit) sprintf() (s string) {
	return fmt.Sprintf("System unit: %s OS, %d RAM, %d cores, %s, %s",
		c.opertionSystem, c.ramInGb, c.coresNumber, c.cpu, c.company)
}

func (c *systemUnit) parse(s string) error {
	res := strings.Split(s, ",")
	elementsNumber := 5
	if len(res) != elementsNumber {
		return errors.New("elements number is wrong")
	}
	for i := 0; i < len(res); i++ {
		res[i] = strings.TrimSpace(res[i])
	}
	var err error
	c.ramInGb, err = strconv.Atoi(res[1])
	if err != nil {
		return errors.New("could not parse ramInGb as integer")
	}
	c.coresNumber, err = strconv.Atoi(res[2])
	if err != nil {
		return errors.New("could not parse coresNumber as integer")
	}
	c.opertionSystem = res[0]
	c.cpu = res[3]
	c.company = res[4]
	return nil
}
func (m *monitor) sprintf() (s string) {
	return fmt.Sprintf("Monitor: %dx%d, %s",
		m.resolution[0], m.resolution[1], m.company)
}

func (m *monitor) parse(s string) error {
	res := strings.Split(s, ",")
	if len(res) != 2 {
		return errors.New("elements number is wrong")
	}
	for i := 0; i < len(res); i++ {
		res[i] = strings.TrimSpace(res[i])
	}
	m.company = res[1]
	mResolution := strings.Split(res[0], "x")
	if len(mResolution) != 2 {
		return errors.New("elements resolution number is wrong")
	}
	for i := 0; i < len(mResolution); i++ {
		mResolution[i] = strings.TrimSpace(mResolution[i])
	}
	firstResolution, err := strconv.Atoi(mResolution[0])
	if err != nil {
		return errors.New("could not parse first resolution as integer")
	}
	secondResolution, err := strconv.Atoi(mResolution[1])
	if err != nil {
		return errors.New("could not second resolution as integer")
	}
	m.resolution = [2]int{firstResolution, secondResolution}
	return nil
}

func showEquipment(eq equipmentPrinter) {
	fmt.Println(eq.sprintf())
}

func parseAll(eqps []equipmentParser, s []string) error {
	errToRet := errors.New("failed to parse")
	if len(eqps) != len(s) {
		return errToRet
	}
	for idx, eqp := range eqps {
		err := eqp.parse(s[idx])
		if err != nil {
			return errToRet
		}
	}
	return nil
}
func main() {
	c := &systemUnit{}
	m := &monitor{}
	err := parseAll([]equipmentParser{c, m},
		[]string{"Ubuntu OS, 16, 8, Intel(R) Core(TM) i5-10505 CPU @ 3.20GHz, Dell",
			"1280x1024, Dell"})
	if err != nil {
		fmt.Println(err)
	}
	showEquipment(c)
	showEquipment(m)
}
```

## Встраивание интерфейса
Часто бывает удобно использовать уже имеющиеся интерфейсы для создания новых. В следующем примере мы использовали стандартный интерфейс `fmt.Stringer` для создания своего — `UserDisplayer`. Интерфейс `fmt.Stringer` — это тип, который может описывать себя строкой.  Если конкретный тип реализует методы `fmt.Stringer`, то он также удовлетворяет интерфейсу `UserDisplayer`. Такой прием называется **встраиванием интерфейса**. Чтобы конкретный тип удовлетворял интерфейсу `fmt.Stringer`, достаточно, чтобы он реализовывал метод `String`:

```go {.example_for_playground}
package main

import (
	"fmt"
)

type UserDisplayer interface {
	fmt.Stringer
}

type ChatUser struct {
	id   int
	name string
}

type InternalUser struct {
	id      int
	fromWho Proxy
	toWhere Proxy
}

type Proxy struct {
	address string
	port    string
}

func (c ChatUser) String() string {
	return fmt.Sprintf("%d, %s", c.id, c.name)
}

func (i InternalUser) String() string {
	return fmt.Sprintf("%d, %s:%s->%s:%s", i.id, i.fromWho.address,
		i.fromWho.port, i.toWhere.address, i.toWhere.port)
}

func ShowAllUsers(u []UserDisplayer) {
	for _, user := range u {
		fmt.Println(user.String())
	}
}

func main() {
	var c ChatUser = ChatUser{1, "corefan"}
	var i InternalUser = InternalUser{2, Proxy{"192.168.23.48", "4040"},
		Proxy{"192.168.23.103", "3030"}}
	var u []UserDisplayer = []UserDisplayer{c, i}

	ShowAllUsers(u)
}
```
```
1, corefan
2, 192.168.23.48:4040->192.168.23.103:3030
```

Встраивать можно более одного интерфейса. Конкретный тип удовлетворяет такому интерфейсу, если он реализует все методы встроенных интерфейсов. В следующем коде объявите интерфейс `ConnCloser`, который требует единственный метод `CloseConn()`. Реализуйте этот метод таким образом, чтобы при его вызове выводилось сообщение `Closing N...`, где `N` — `id` сессии пользователя. Встройте интерфейс `ConnCloser` в интерфейс `AppUser`. {.task_text}

```go {.task_source #golang_chapter_0130_task_0020}
package main

import (
	"fmt"
)

type AppUser interface {
	UserDisplayer
}

type UserDisplayer interface {
	fmt.Stringer
}

type ChatUser struct {
	id   int
	name string
}

type InternalUser struct {
	id      int
	fromWho Proxy
	toWhere Proxy
}

type Proxy struct {
	address string
	port    string
}

func (c ChatUser) String() string {
	return fmt.Sprintf("%d, %s", c.id, c.name)
}

func (i InternalUser) String() string {
	return fmt.Sprintf("%d, %s:%s->%s:%s", i.id, i.fromWho.address,
		i.fromWho.port, i.toWhere.address, i.toWhere.port)
}

func ShowAllUsers(u []AppUser) {
	for _, user := range u {
		fmt.Println(user.String())
	}
}

func CloseAllConns(u []AppUser) {
	for _, user := range u {
		user.CloseConn()
	}
}

func main() {
	var c ChatUser = ChatUser{1, "corefan"}
	var i InternalUser = InternalUser{2, Proxy{"192.168.23.48", "4040"},
		Proxy{"192.168.23.103", "3030"}}
	var u []AppUser = []AppUser{c, i}

	ShowAllUsers(u)
	CloseAllConns(u)
}

```

В интерфейс `AppUser` теперь будет встроено два других интерфейса: `UserDisplayer` и `ConnCloser`. {.task_hint}

``` go  {.task_answer}
package main

import (
	"fmt"
)

type AppUser interface {
	UserDisplayer
	ConnCloser
}

type UserDisplayer interface {
	fmt.Stringer
}

type ConnCloser interface {
	CloseConn()
}

type ChatUser struct {
	id   int
	name string
}

type InternalUser struct {
	id      int
	fromWho Proxy
	toWhere Proxy
}

type Proxy struct {
	address string
	port    string
}

func (c ChatUser) CloseConn() {
	fmt.Printf("closing %d...\n", c.id)
}

func (c InternalUser) CloseConn() {
	fmt.Printf("closing %d...\n", c.id)
}

func (c ChatUser) String() string {
	return fmt.Sprintf("%d, %s", c.id, c.name)
}

func (i InternalUser) String() string {
	return fmt.Sprintf("%d, %s:%s->%s:%s", i.id, i.fromWho.address,
		i.fromWho.port, i.toWhere.address, i.toWhere.port)
}

func ShowAllUsers(u []AppUser) {
	for _, user := range u {
		fmt.Println(user.String())
	}
}

func CloseAllConns(u []AppUser) {
	for _, user := range u {
		user.CloseConn()
	}
}

func main() {
	var c ChatUser = ChatUser{1, "corefan"}
	var i InternalUser = InternalUser{2, Proxy{"192.168.23.48", "4040"},
		Proxy{"192.168.23.103", "3030"}}
	var u []AppUser = []AppUser{c, i}

	ShowAllUsers(u)
	CloseAllConns(u)
}
```

## Пустой интерфейс
Полезным приемом служит использование пустого интерфейса. С помощью следующей задачи попробуйте самостоятельно разобраться, почему.

Что выведет следующий код? В случае ошибки напишите `error`. {.task_text}

```go {.example_for_playground}
package main

import (
	"fmt"
)

func main() {
	var i interface{}
	i = 10
	i = "hello"
	fmt.Println(i)
}
```

```consoleoutput {.task_source #golang_chapter_0130_task_0030}
```
Пустой интерфейс не требует реализации ни одного метода. {.task_hint}
```go {.task_answer}
hello
```

Пустой интерфейс не требует реализации ни одного метода. Поэтому значением переменной `i` может быть все что угодно. Таким образом, в Go есть косвенная возможность для динамической типизации.

Начиная с Go 1.18, у типа `interface{}` есть псевдоним — `any`. Код из задачи эквивалентен следующему: 

```go {.example_for_playground}
package main

import (
	"fmt"
)

func main() {
	var i any
	i = 10
	i = "hello"
	fmt.Println(i)
}
```

## Резюме

1. Интерфейс требует методы, которые должны быть реализованы конкретным типом.
2. Чтобы конкретный тип удовлетворял интерфейсу, для него достаточно реализовать все методы этого интерфейса. Этот прием называется «утиная типизация».
3. Один интерфейс может содержать другие интерфейсы. Чтобы конкретный тип удовлетворял такому интерфейсу, он должен реализовывать все методы каждого из встроенных интерфейсов.
4. Бывает удобно использовать стандартные интерфейсы. Например, `fmt.Stringer`. Они ничем не отличаются от пользовательских. Подробнее о стандартных интерфейсах можно узнать на  официальном сайте Go.
5. Пустой интерфейс может содержать значение любого типа.
6. Псевдоним пустого интерфейса `interface{}` — `any`.
