# Глава 3. Циклы
## Простейшая форма цикла `for` 
В языке Go существует один единственный вид циклов — это цикл `for`. С помощью данного цикла реализуются и другие классические циклы (например, `while` и `do-while`).

Цикл `for` позволяет выполнить код заранее заданное число раз, пока действительно некоторое условие. Цикл `for` в языке Go может быть написан очень по-разному. В целом, однако, цикл `for` соответствует следующей схеме:

```
for инициализацияСчетчика; условие; изменениеСчетчика {
    // действия
}
```

Вот как выглядит простейший цикл `for`:

```go {.example_for_playground .example_for_playground_001}
for i := 0; i < 100; i++ {
    fmt.Println("Number: ", i)
}
```

Важно! Символ `{` в Go обязательно должен стоять в конце данной строчки кода, а не вначале следующей строки. В противном случае вы получите ошибку компиляции.
Это касается любых предложений на языке Go. Например, следующий синтаксис ошибочен:

```go
for i := 0; i < 100; i++
{
    fmt.Println("Number: ", i)
}
```

При попытке компиляции вы получите ошибку:
```
./prog.go:5:26: syntax error: unexpected newline, expected { after for clause

Go build failed.
```

Вот как можно написать бесконечный цикл: 
```go
for {
    var s string
    fmt.Scan(&s)
    fmt.Println("Your input: ", s)
}
``` 

Напишите программу с использованием цикла, которая выведет на экран сообщение `Hello, gophers!` 5 раз. {.task_text}

```go {.task_source #golang_chapter_0030_task_0010}
package main
import "fmt"

func main() {
    // ваш код здесь
}
```  

Воспользуйтесь простейшей формой цикла `for`. {.task_hint}

``` golang {.task_answer}
package main

import "fmt"

func main() {
    for i := 0; i < 5; i++ {
        fmt.Println("Hello, gophers!")
    }
}
```
## Реализация других известных циклов
Ни инициализация счетчика, ни условие, ни изменение счетчика не являются обязательной частью цикла `for`. Так, опустив инициализацию счетчика и его изменение, можно получить аналог цикла `while`:

```go {.example_for_playground .example_for_playground_002}
var i int 

for i < 100 {
    fmt.Println("Number: ", i)
    i++
}
```


В Go отсутствует цикл `do-while`. Однако его можно реализовать с использованием цикла `for`. Смысл такого цикла состоит в том, чтобы тело цикла выполнилось хотя бы один раз, независимо от условия. В приведенном ниже примере цикл не выполнится ни разу, поскольку условие `anExpression` ложно. Измените цикл таким образом, чтобы его тело выполнилось один раз, независимо от условия. Программа должна вывести сообщение `Hello, gophers!` {.task_text}

```go {.task_source #golang_chapter_0030_task_0020}
package main

import "fmt"

func main() {
    var anExpression = false

    for anExpression { // измените цикл 
        fmt.Println("Hello, gophers!")
    }
}
```  

Инициализируйте счетчик переменной со значением `true`, задайте в качестве условия значение этой переменной, измените счетчик в соответствии с `anExpression`. {.task_hint}

``` golang {.task_answer}
package main

import "fmt"

func main() {
    var anExpression = false

    for ok := true; ok; ok = anExpression {
        fmt.Println("Hello, gophers!")
    }
}
```

## Операторы `break` и `continue`
Оператор `break` позволяет досрочно прервать выполнение цикла:

```go  {.example_for_playground .example_for_playground_003}
for i := 0; i < 100; i++ {
    fmt.Println(i)
    if i > 10 {
        break 
    }
}
```

Когда есть несколько вложенных циклов, бывает удобно использовать `break` по метке: 

```go  {.example_for_playground .example_for_playground_004}
loop:
	for i := 0; i < 10; i++ {
		fmt.Println("i = ", i)
		for j := 0; j < 10; j++ {
			fmt.Println("j = ", j)

			if i == 5 && j == 4 {
				fmt.Println("FINISH!")
				break loop
			}
		}
	} 
```

Как только выполнится условие внутри `if`, произойдет выход из внешнего цикла. `loop` — произвольное имя метки.

Оператор `continue` позволяет досрочно перейти на выполнение следующей итерации цикла.

```go  {.example_for_playground .example_for_playground_005}
for i := 0; i < 100; i++ {
    if i%10 == 0 {
        continue 
    }
    fmt.Println(i)
}
```

Оператор `%` возвращает остаток от деления. В данном случае числа, кратные 10, не будут выведены на экран.

Аналогично `break` по метке существует и `continue` по метке.

## Задача на закрепление 

Числа Фибоначчи — элементы числовой последовательности, в которой первые два числа равны `0` и `1`, а каждое последующее число равно сумме двух предыдущих. Напишите программу, которая вычисляет 25 первых чисел Фибоначчи и выводит их на экран. Выведите числа, разделенные символами `;` и пробел. После последнего числа данные символы вставлять не нужно. Формат вывода: `0; 1; 1; 2; 3; 5 ...` {.task_text}

 Для вывода без перевода на новую строку можно использовать функцию `fmt.Print`. {.task_text}

```go {.task_source #golang_chapter_0030_task_0030}
package main
import "fmt"

func main() {
    // ваш код здесь
}
```  

Инициализируйте первые два числа, задав им значения `0` и `1`. Организуйте цикл, внутри которого новое число вычисляется как сумма этих двух, причем в конце каждой итерации первое число меняется на второе, а второе — на последний результат. {.task_hint}

``` golang {.task_answer}
package main
import "fmt"

func main() {
    num := 25
    a1 := 0
    a2 := 1

    fmt.Print(a1, "; ")
    fmt.Print(a2, "; ")

    for i := 0; i < num-2; i++ {
        a3 := a1 + a2
        fmt.Print(a3)
        if i != num-3 {
            fmt.Print("; ")
        }
        a1 = a2
        a2 = a3
    }
}
```

## Резюме 
1. Все циклы реализуются с помощью ключевого слова `for`.
2. Цикл состоит из следующих частей: инициализации счетчика, условия и изменения счетчика.
3. Ни одна из частей цикла не является обязательной.
4. Оператор `continue` позволяет немедленно перейти к следующей итерации цикла. 
5. Оператор `break` позволяет немедленно выйти из цикла. 
6. Операторы `break` и `continue` могут использоваться по метке.
