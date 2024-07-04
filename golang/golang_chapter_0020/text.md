# Глава 2. Условия
## Условные выражения
В Go, аналогично другим языкам, в условиях используются условные выражения. Условные выражения должны быть типа `bool`. 

В условных выражениях используются следующие операции сравнения.

- Меньше: `<`
- Больше: `>`
- Меньше или равно: `<=`
- Больше или равно: `>=`
- Проверка на равенство: `==`
- Проверка на неравенство: `!=`

Переменной типа `bool` может быть присвоен результат сравнения. Вот так можно проверить, является ли сегодняшний день рабочим, без учета праздников:

```golang {.example_for_playground .example_for_playground_001}
weekday := time.Now().Weekday()
working := weekday != time.Sunday && weekday != time.Saturday
fmt.Println("Is it a working day:", working, "[", weekday, "]")
```

Здесь `&&` означает конъюнкцию, логическое `И`.


Наудачу бросают 20-ти гранный кубик. Выпавшее значение сохраняется в переменной `coub`. В случае, если значение `coub` больше 10, выведите на экран `true`, иначе — `false`. Для вывода значения на экран воспользуйтесь функцией `fmt.Println()`. {.task_text}

```golang {.task_source #golang_chapter_0020_task_0010}
package main

import (
    "fmt"
    "math/rand"
)

func isLucky(coub int) (lucky bool){
    // ваш код здесь 
    return 
}

func main() {
    coub := rand.Intn(19) + 1
    fmt.Println(isLucky(coub))
}
```

Задайте переменной `lucky` типа `bool` значение условия. {.task_hint}

``` golang {.task_answer}
package main

import (
    "fmt"
    "math/rand"
)

func isLucky(coub int) (lucky bool){
    lucky = coub > 10
    return
}

func main() {
    coub := rand.Intn(19) + 1
    fmt.Println(isLucky(coub))
}
```

## Логические операции
Существуют также логические операции, которые объединяют несколько условий.

- Отрицание: `!`
- Конъюнкция (логическое И): `&&`
- Дизъюнкция (логическое ИЛИ): `||`

Отрицание инвертирует результат условия. Конъюнкция возвращает `true`, когда оба операнда равны `true`. В противном случае конъюнкция возвращает `false`. Дизъюнкция возвращает `true`, когда хотя бы один из операндов равен `true`. В противном случае дизъюнкция возвращает `false`.


Реализуйте функцию implic(), выполняющую импликацию. Импликацией называется логическая операция `a → b`, которая ложна лишь тогда, когда `a` истинно, а `b` ложно. Функция implic() принимает два значения типа `bool` и возвращает значение того же типа. Для возврата результата используйте ключевое слово `return`. Более подробно функции рассматриваются в последующих главах. {.task_text}

```golang {.task_source #golang_chapter_0020_task_0020}
package main

import (
    "fmt"
)

func implic(a bool, b bool) bool {
    // ваш код здесь 
}

func main() {
    fmt.Println(implic(true, true))
    fmt.Println(implic(false, false))
    fmt.Println(implic(true, false))
    fmt.Println(implic(false, true))
}
```

Импликацию можно выразить выражением, принимающем истинное значение, если истинно отрицание `a` или истинно `b`. {.task_hint}

```golang {.task_answer}
package main

import (
    "fmt"
)

func implic(a bool, b bool) bool {
    return !a || b
}

func main() {
    fmt.Println(implic(true, true))
    fmt.Println(implic(false, false))
    fmt.Println(implic(true, false))
    fmt.Println(implic(false, true))
}
```


Функцией голосования называется функция, которая принимает на вход три аргумента типа `bool` и возвращает истину, если хотя бы два аргумента — истина. В противном случае функция возвращает ложь. Допишите тело функции голосования. {.task_text}

```golang {.task_source #golang_chapter_0020_task_0030}
package main

import (
    "fmt"
)

func vote(a bool, b bool, c bool) bool {
    // ваш код здесь
}

func main() {
    fmt.Println(vote(true, true, true))
    fmt.Println(vote(false, true, true))
    fmt.Println(vote(true, false, true))
    fmt.Println(vote(true, true, false))
    fmt.Println(vote(true, false, false))
    fmt.Println(vote(false, true, false))
    fmt.Println(vote(false, false, true))
    fmt.Println(vote(false, false, false))
}

```

Проверьте попарно истинность конъюнкции всех аргументов. {.task_hint}

```golang {.task_answer}
package main

import (
    "fmt"
)

func vote(a bool, b bool, c bool) bool {
    return a && b || a && c || b && c
}

func main() {
    fmt.Println(vote(true, true, true))
    fmt.Println(vote(false, true, true))
    fmt.Println(vote(true, false, true))
    fmt.Println(vote(true, true, false))
    fmt.Println(vote(true, false, false))
    fmt.Println(vote(false, true, false))
    fmt.Println(vote(false, false, true))
    fmt.Println(vote(false, false, false))
}

```

## Варианты условий
Условные конструкции Go представлены следующими формами:
- `if`
- `if-else`
- `if-else-if`
- `switch-case`

`if` проверяет условие и выполняет соответствующий блок кода в случае, когда условие истинно. Например:

```golang {.example_for_playground .example_for_playground_002}
a := 7
b := 8

if a != b {
    fmt.Println("a не равно b")
}
```

Перед проверкой условия можно выполнить присвоение:

```golang {.example_for_playground .example_for_playground_003}
if a := 7; a < 8 {
    fmt.Println("a < 8")
} 
```
Такая переменная видна только в рамках условия. 

`if-else` проверяет условие и выполняет блок кода внутри `if`, если условие истинно. В противном случае выполняется код внутри `else`. Например:

```golang {.example_for_playground .example_for_playground_004}
a := 7
b := 8

if a == b {
    fmt.Println("a равно b")
} else {
    fmt.Println("a не равно b")
}
```

`if-else-if` допускает несколько условий:

```golang  {.example_for_playground .example_for_playground_005}
a := 7
b := 8

if a < b { 
    fmt.Println("a < b")
} else if a > b {
    fmt.Println("a > b")
} else {
    fmt.Println("a равно b")
}
```

Для читателей, знакомых с тернарным оператором, отметим, что в языке Go он отсутствует.

Когда условий много, то в качестве альтернативы `if-else-if` можно использовать `switch-case`:

```golang  {.example_for_playground .example_for_playground_006}
country := "China"
switch country {
case "Russia":
    fmt.Println("Привет!")
case "England":
    fmt.Println("Hello!")
case "China":
    fmt.Println("你 好")
default:
    fmt.Println("Unknown language")
}
```

Перед проверкой можно также выполнить присвоение:

```golang
package main
import "fmt"

func login() string {
	fmt.Println("Здравствуйте! Задайте ваш логин")
	var name string
	fmt.Scan(&name) // консольный ввод
	return name
}

func main() {

	switch name := login(); name {

	case "admin":
		fmt.Println("Добро пожаловать, администратор!")
	case "moderator":
		fmt.Println("Добро пожаловать, модератор!")
	default:
		fmt.Print("Добро пожаловать, ")
		fmt.Print(name)
		fmt.Println("!")
	}
}
```

`fmt.Print` печатает аргумент без переноса строки.

Важно! Для читателей, знакомых с C-подобными языками, отметим, что выполнение какого-либо из `case` в языке Go не приводит к выполнению всех нижележащих `case`, независимо от условий. Поэтому писать `break` после каждого из `case` нет необходимости. Это сделано разработчиками языка Go во избежание ошибок. Изменить это поведение можно, написав после `case` ключевое слово `fallthrough`. Если так сделать, то следующий `case` выполнится, независимо от своего условия.

Также существует форма `switch-case`, позволяющая написать произвольные условия:

```golang  {.example_for_playground .example_for_playground_007}
country := "Russia"
name := "Ivan"

switch {
case country == "Russia" && name == "Ivan":
    fmt.Println("Привет, Иван!")
case country == "England" || name == "Jack":
    fmt.Println("Hello Jack!")
case country == "China":
    fmt.Println("你 好")
default:
    fmt.Println("Unknown language")
}
```


Необходимо реализовать функцию `developerGrade()`, которая возвращает номер грейда разработчика по его названию. Всего есть четыре грейда разработчика: `trainee`, `junior`, `middle`, `senior`. Номер первого грейда `trainee` — 1, номер последнего грейда `senior` — 4. Если передано название, не соответствующее ни одному из перечисленных грейдов, то функция `developerGrade()` возвращает 0. {.task_text}

```golang {.task_source #golang_chapter_0020_task_0040}
package main
import "fmt"

func developerGrade(grade string) int {
    // ваш код здесь
}

func main() {
    fmt.Println("senior", ":", developerGrade("senior"))
    fmt.Println("middle", ":", developerGrade("middle"))
    fmt.Println("junior", ":", developerGrade("junior"))
    fmt.Println("trainee", ":", developerGrade("trainee"))
    fmt.Println("bookkeeper", ":", developerGrade("bookkeeper"))
}
```

Реализуйте классическую форму `switch-case`. {.task_hint}

```golang {.task_answer}
package main
import "fmt"

func developerGrade(grade string) int {
    switch grade {
    case "trainee":
        return 1
    case "junior":
        return 2
    case "middle":
        return 3
    case "senior":
        return 4
    default:
        return 0
    }
}

func main() {
    fmt.Println("senior", ":", developerGrade("senior"))
    fmt.Println("middle", ":", developerGrade("middle"))
    fmt.Println("junior", ":", developerGrade("junior"))
    fmt.Println("trainee", ":", developerGrade("trainee"))
    fmt.Println("bookkeeper", ":", developerGrade("bookkeeper"))
}
```


## Резюме
1. В Go используются операции сравнения: `<`, `>`, `<=`, `>=`, `==`, `!=`.
2. Логические операции: `!`, `&&`, `||`.
3. Условия в языке представлены инструкциями: `if`, `if-else`,`if-else-if`, `switch-case`.
4. Внутри `if` условия может быть объявлена переменная, которая будет видна только внутри блока `if`. Аналогично для `switch-case`.
