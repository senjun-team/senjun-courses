# Глава 10. Строки

## Кодировки

Первоначально на все случаи жизни использовалась одна кодировка — [ASCII.](https://www.ascii-code.com/) Она расшифровывается как American Standard Code for Information Interchange — Американский стандартный код для обмена информацией. В ней каждому символу поставлен в соответствие некоторый код. Всего — 128 символов. В ней собраны английские буквы, цифры, знаки пунктуации и управляющие символы.  

Для представления других символов была разработана кодировка [Unicode.](https://symbl.cc/ru/unicode-table/) В ней содержится более 120 тыс. символов для различных языков. Номер символа Unicode представляется типом *int32*. В Go для этого типа существует синоним — *руна* `rune`.

Представлять каждый символ в виде 32-битного значения расточительно. Многие символы имеют номер, для которого можно было бы выделить в разы меньше памяти. Так, для всех символов ASCII было бы достаточно одного байта. Более аккуратным решением является кодировка переменной длины — [UTF-8.](https://ru.wikipedia.org/wiki/UTF-8) Она изобретена двумя из создателей языка Go, Кеном Томпсоном и Робом Пайком.

UTF-8 представляется разумным решением, однако возникает проблема. Теперь длина текста в символах необязательно равна длине этого текста в байтах. Как же быть, когда необходимо знать длину текста в символах? Для этого используют функцию [RuneCountInString](https://pkg.go.dev/unicode/utf8#RuneCountInString) из стандартного пакета [unicode/utf8](https://pkg.go.dev/unicode/utf8):

```go {.example_for_playground}
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	s := "привет!"
	fmt.Println(utf8.RuneCountInString(s))
}
```

Функция `utf8.RuneCountInString` возвращает количество рун или символов Unicode, которые закодированы в строке. А `utf8.DecodeRuneInString` возвращает руну и размер первого входящего в строку символа Unicode:

```go {.example_for_playground}
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	s := "привет!"

	for i := 0; i < len(s); {
		r, size := utf8.DecodeRuneInString(s[i:])
		fmt.Printf("%d:\t%c\n", r, r)
		i += size
	}
}
```
```
1087:	п
1088:	р
1080:	и
1074:	в
1077:	е
1090:	т
33:		!
```
Здесь мы печатаем код символа и сам символ.  
Чтобы превратить срез из рун в строку UTF-8, достаточно сконструировать из него объект типа `string`:

```go {.example_for_playground  .example_for_playground_001}
r := []rune{1087, 1088, 1080, 1074, 1077, 1090, 33}
fmt.Println(string(r))
```
```
привет!
```

В случае некорректного кода генерируется символ, который обычно выглядит как белый вопросительный знак в черном ромбе �:

```go
fmt.Println(string(1234567))
```
```
�
```

## Приемы по работе со строками

В Go строка `string` представляет собой последовательность байтов, доступную только для чтения. Встроенная функция `len` возвращает количество байтов (**не символов!**) в строке. Так, программа ниже выведет на экран число `6`.

```go {.example_for_playground}
package main

import "fmt"

func main() {
	s := "hello!"
	fmt.Println(len(s))
}
```

Однако вот эта программа выведет на экран число `13`:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	s := "привет!"
	fmt.Println(len(s))
}
```

Попытка обратиться к байту за пределами строки приведет к панике:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	s := "привет!"
	fmt.Println(s[13])
}
```
```
panic: runtime error: index out of range [13] with length 13
```
Чтобы получить подстроку, поступают следующим образом:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	s := "hello!"
	fmt.Println(s[0:4])
}
```
В результате будет напечатана подстрока строки `hello`, с нулевого (включительно) до четвертого (не включительно) байта:
```
hell
```

Если задать левую границу больше правой, то возникает паника. В случае, когда не задана левая граница, то подставляется число `0`. Когда не задана правая граница, подставляется длина строки:

```go {.example_for_playground}
package main

import "fmt"

func main() {
	s := "hello!"
	fmt.Print(s[:4])
	fmt.Println(s[5:])
}
```
```
hell!
```

Строки можно сравнивать. Сравниваются они байт за байтом, в лексиграфическом порядке. Так, строка `abda` больше строки `abcd`, поскольку третий символ первой строки `d` имеет больший номер, чем третий символ второй строки `c`.

Напишите тело функции `isPalindrome`, которая проверяет, читается ли строка в обоих направлениях одинаково. Например, для слова `kayak` функция должна вернуть `true`, а для слова `blabla` — `false`. {.task_text}

```go {.task_source #golang_chapter_0100_task_0010}
package main

import "fmt"

func isPalindrome(s string) bool {
	// ваш код здесь
}

func main() {
	fmt.Println(isPalindrome("kayak"))
	fmt.Println(isPalindrome("blabla"))
}
```

Функцию `isPalindrome` удобно реализовать как рекурсивную функцию. {.task_hint}

```go {.task_answer}
package main

import "fmt"

func isPalindrome(s string) bool {
	if len(s) < 2 {
		return true
	}
	if s[0] != s[len(s)-1] {
		return false
	}
	return isPalindrome(s[1 : len(s)-1])
}

func main() {
	fmt.Println(isPalindrome("kayak"))
	fmt.Println(isPalindrome("blabla"))
}
```

Допустима конкатенация строк через символ «+»:

```go {.example_for_playground .example_for_playground_002}
helloMessage := "hello"
name := "gopher"
fmt.Println(helloMessage + ", " + name + "!")
```
```
hello, gopher!
```

Однако для конкатенации рекомендуется использовать возможности пакета `fmt`. Это делает код более читаемым: слева оказывается сама строка, а справа — переменные.

```go {.example_for_playground .example_for_playground_003}
helloMesage := "hello"
name := "gopher"
fmt.Printf("%s, %s!\n", helloMesage, name)
```
```
hello, gopher!
```
В данном случае на места `%s` подставятся наши строки. Управляющая последовательность `\n` выполняет перенос строки. 

Чтобы не обрабатывать управляющие последовательности, а напечатать строку как есть, используют обратные одинарные кавычки:

```go {.example_for_playground .example_for_playground_004}
	helloMesage := `Hello, gopher!
Here we use raw string. 
This won't work: \n`

	fmt.Println(helloMesage)
```
```
Hello, gopher!
Here we use raw string. 
This won't work: \n
```

Переносы строки печатаются, тогда как управляющая последовательность `\n` выводится как есть. Иными словами, строка печатается буквально. 

Строки являются неизменяемыми. Даже если вы используете конкатенацию для строки, то на месте старой строки оказывается новая. Попытка изменить конкретный байт ведет к ошибке компиляции:

```go {.example_for_playground .example_for_playground_005}
helloMessage := "hello"
helloMessage[1] = "b"
```
```
./main.go:5:2: cannot assign to helloMessage[1] (neither addressable nor a map index expression) (exit status 1)
```

Такое решение было принято для эффективного использования памяти. Если строки неизменяемы, то они могут разделять общую память. Например, для получения подстроки никакой новой памяти выделено не будет. 

## Преобразование строк 

Строки могут быть преобразованы к байтам, а байты — к строкам:

```go {.example_for_playground  .example_for_playground_006}
fmt.Println([]byte("gopher"))
fmt.Println(string([]byte{103, 111, 112, 104, 101, 114}))
```
```
[103 111 112 104 101 114]
gopher
```

В данном случае при преобразовании строки `gopher` к срезу байтов, будет выделена дополнительная память под него. Аналогично — для преобразования среза байтов в строку. Чтобы избежать лишних выделений памяти, лучше избегать таких преобразований, когда в этом нет необходимости. Поэтому многие функции стандартного пакета `bytes` дублируют функции стандартного пакета `strings`. Например, для поиска подстроки в строке используется функция [strings.Index](https://pkg.go.dev/strings#Index), а для поиска подмассива в массиве байтов — [bytes.Index](https://pkg.go.dev/bytes#Index).

Иногда также требуется преобразование между числами и их строковым представлением. Для этого используются функции пакета [strconv](https://pkg.go.dev/strconv). Например, преобразование целого к строке выполняется функцией `strconv.Itoa`. Название функции — сокращение от «integer to ASCII». Для обратного преобразования подойдет `strconv.Atoi`. Для значений с плавающей точкой — `strconv.FormatFloat` превращает число в строку, а `strconv.ParseFloat` строку — в число. 

Необходимо разобрать адрес `address`, состоящий из `ip` и порта `port`. Реализуйте тело функции `parseAddress`, которая их возвращает. Например, для строки `127.0.0.1:4040` необходимо вернуть `127.0.0.1` в качестве `ip` и `4040` — в качестве порта. Четыре числа `ip` ограничены от `0` до `255` включительно. Порт ограничен значениями от `0` до `65535` включительно. В случае неправильной строки с помощью функции `errors.New` верните ошибку с сообщением `invalid address`.  {.task_text}

```go {.task_source #golang_chapter_0100_task_0020}
package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

func parseAddress(address string) (ip string, port string, err error) {
	// ваш код здесь 
}

func main() {
	fmt.Println(parseAddress("192.168.23.48:6060"))
	fmt.Println(parseAddress("127.0.0.1:4040"))
	fmt.Println(parseAddress("192.168.256.48:6060"))
	fmt.Println(parseAddress("192.168.23.48:60b8"))
}
```

Индекс подстроки в строке позволяет найти функция `strings.Index`. Чтобы преобразовать строку в число, воспользуйтесь функцией `strconv.Atoi`. {.task_hint}

```go {.task_answer}
package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

func parseAddress(address string) (ip string, port string, err error) {
	invalid := errors.New("invalid address")

	i := strings.Index(address, ":")
	if i == -1 {
		err = invalid
		return
	}
	ip = address[:i]
	port = address[i+1:]
	rest := ip
	counter := 0

	// check ip
	for {
		idx := strings.Index(rest, ".")
		if idx == -1 {
			idx = len(rest)
		}

		var number int
		number, err = strconv.Atoi(rest[:idx])
		if err != nil {
			err = invalid
			return
		}
		if number < 0 || number > 255 {
			err = invalid
			return
		}
		counter++

		if idx == len(rest) {
			break
		}
		rest = rest[idx+1:]
	}
	const ipSize = 4

	if counter != ipSize {
		err = invalid
		return
	}
	// check port
	number, err := strconv.Atoi(port)
	if err != nil {
		err = invalid
		return
	}

	if number < 0 || number > 65535 {
		err = invalid
		return
	}
	return
}

func main() {
	fmt.Println(parseAddress("192.168.23.48:6060"))
	fmt.Println(parseAddress("127.0.0.1:4040"))
	fmt.Println(parseAddress("192.168.256.48:6060"))
	fmt.Println(parseAddress("192.168.23.48:60b8"))
}
```

## Резюме

1. Строки неизменяемы и доступны только для чтения.
2. С помощью встроенной функции `len` можно узнать длину строки в байтах, но не в символах. 
3. Для работы с символами Unicode используйте `utf8.RuneCountInString` и `utf8.DecodeRuneInString`.
4. Существуют различные кодировки. Строки могут содержать произвольные байты. Однако, если строка была сформирована из строковых констант, то она всегда представляется в кодировке UTF-8.
5. Кодировка UTF-8 — кодировка переменной длины. 
6. Компилятор языка Go обрабатывает управляющие последовательности внутри строки. Например, `\n` — это перенос строки, `\t` — табуляция. Непосредственные переносы внутри строки с двойными кавычками недопустимы. Чтобы не обрабатывать управляющие последовательности, а также использовать непосредственные переносы, используйте обратные одинарные кавычки. 
7. Для преобразования между числами и их строковым представлением используйте пакет `strconv`.
