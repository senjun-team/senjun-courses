# Глава 2
## Условия 
В `Go`, аналогично другим языкам, в условиях используются условные выражения. Условные выражения должны быть типа `bool`. 

В условных выражениях используются следующие операции сравнения.

- Меньше: `<`
- Больше: `>`
- Меньше или равно: `<=`
- Больше или равно: `>=`
- Проверка на равенство: `==`
- Проверка на неравенство: `!=`

Переменной типа `bool` может быть присвоен результат сравнения. Например: 
```golang
var b bool
b = 4 < 5
```

Объявите переменную типа `bool`, присвойте ей результат логического выражения, когда 4 не равно 5. Выведите эту переменную на экран. {.task_text}

```golang {.task_source #golang_chapter_0020_task_0010}
package main
import "fmt"

func main() {
	// ваш код здесь
}
```  
``` {.task_hint}
package main

import "fmt"

func main() {
	var b bool
	b = 4 != 5
	fmt.Println(b)
}
```

Существуют также логические операции, которые объединяют несколько условий.

- Отрицание: `!`
- Конъюнкция (логическое И): `&&`
- Дизъюнкция (логическое ИЛИ): `||`

Отрицание инвертирует результат условия. Конъюнкция возвращает `true`, когда оба операнда равны `true`. В противном случае конъюнкция возвращает `false`. Дизъюнкция возвращает `true`, когда хотя бы один из операндов равен `true`. В противном случае дизъюнкция возвращает `false`. 

Даны две переменные типа `bool`: `a` и `b`. Импликацией называется логическая связка "если..., то...". Если `a` и `b` истинны, то их импликация — истина. Если `a` и `b` ложны, то их импликация — истина. Если `a` — истина, а `b` — ложь, то их ипликация — ложь. Если `a` — ложь, а `b` — истина, то их импликация — истина. Допишите тело функции `implic` так, чтобы она выполняла импликацию. Данная функция принимает два значения типа `bool` и возвращает значение типа `bool`. Для возврата результата используйте ключевое слово `return`. Более подробно функции рассматриваются в последующих главах.
 {.task_text}

```golang {.task_source #golang_chapter_0020_task_0020}
package main

import (
	"fmt"
)

func implic(a bool, b bool) bool {
	// ваш код здесь 
}

func main() {
	a := true
	b := true
	fmt.Println(implic(a, b))
	a = false
	b = false
	fmt.Println(implic(a, b))
	a = true
	b = false
	fmt.Println(implic(a, b))
	a = false
	b = true
	fmt.Println(implic(a, b))
}
```  
``` {.task_hint}
package main

import (
	"fmt"
)

func implic(a bool, b bool) bool {
	return !a || b
}

func main() {
	a := true
	b := true
	fmt.Println(implic(a, b))
	a = false
	b = false
	fmt.Println(implic(a, b))
	a = true
	b = false
	fmt.Println(implic(a, b))
	a = false
	b = true
	fmt.Println(implic(a, b))
}
```

Условные конструкции могут быть выражены следующими приемами: 
- `if`
- `if-else`
- `if-else-if`
- `switch-case`

`if` проверяет условие и выполняет соответствующий блок кода в случае, когда условие истинно. Например: 

```golang 
a := 7
b := 8

if a != b {
    fmt.Println("a не равно b")
}
```

Перед проверкой условия можно выполнить присвоение. Например:
```golang
if a := 7; a < 8 {
    fmt.Println("a < 8")
} 
```
Такая переменная видна только в рамках условия. 

`if-else` проверяет условие и выполняет блок кода внутри `if`, если условие истинно. В противном случае выполняется код внутри `else`.

Например: 
```golang
a := 7
b := 8

if a == b {
    fmt.Println("a равно b")
}else{
    fmt.Println("a не равно b")
}
```

`if-else-if` допускает несколько условий. Например: 
```golang
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

Когда условий много, то в качестве альтернативы `if-else-if` можно использовать `switch-case`. Например: 

```golang
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
Важно! Для читателей, знакомых с C-подобными языками, отметим, что выполнение какого-либо из `case` в языке `Go` не приводит к выполнению всех нижележащих `case`, независимо от условий. Поэтому писать `break` после каждого из `case` нет необходимости. Это сделано разработчиками языка `Go` во избежание ошибок. Изменить это поведение можно, написав после `case` ключевое слово `fallthrough`. Если так сделать, то следующий `case` выполнится, независимо от своего условия. 

Также существует форма `switch-case`, позволяющая написать произвольные условия. Например:
```golang
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
Функцией голосования называется функция, которая принимает на вход три аргумента типа `bool` и возвращает истину, если хотя бы два аргумента — истина. В противном случае функция возвращает ложь. Допишите тело функции голосования  {.task_text}

```golang {.task_source #golang_chapter_0020_task_0030}
package main

import (
	"fmt"
)

func vote(a bool, b bool, c bool) bool {
	// ваш код здесь 
}

func main() {
	a := true
	b := true
	c := true

	fmt.Println(vote(a, b, c))

	a = false
	b = true
	c = true

	fmt.Println(vote(a, b, c))

	a = true
	b = false
	c = true

	fmt.Println(vote(a, b, c))

	a = true
	b = true
	c = false

	fmt.Println(vote(a, b, c))

	a = true
	b = false
	c = false

	fmt.Println(vote(a, b, c))

	a = false
	b = true
	c = false

	fmt.Println(vote(a, b, c))

	a = false
	b = false
	c = true

	fmt.Println(vote(a, b, c))

	a = false
	b = false
	c = false

	fmt.Println(vote(a, b, c))
}

```  
``` {.task_hint}
package main

import (
	"fmt"
)

func vote(a bool, b bool, c bool) bool {
	if a && b {
		return true
	}
	if a && c {
		return true
	}
	if b && c {
		return true
	}

	return false
}

func main() {
	a := true
	b := true
	c := true

	fmt.Println(vote(a, b, c))

	a = false
	b = true
	c = true

	fmt.Println(vote(a, b, c))

	a = true
	b = false
	c = true

	fmt.Println(vote(a, b, c))

	a = true
	b = true
	c = false

	fmt.Println(vote(a, b, c))

	a = true
	b = false
	c = false

	fmt.Println(vote(a, b, c))

	a = false
	b = true
	c = false

	fmt.Println(vote(a, b, c))

	a = false
	b = false
	c = true

	fmt.Println(vote(a, b, c))

	a = false
	b = false
	c = false

	fmt.Println(vote(a, b, c))
}

```

## Резюме 
1. В `Go` используются операции сравнения: `<`, `>`, `<=`, `>=`, `==`, `!=`.
2. Логические операции: `!`, `&&`, `||`.
3. Условные выражения: `if`, `if-else`,`if-else-if`, `switch-case`.