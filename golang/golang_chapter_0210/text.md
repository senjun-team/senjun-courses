# Глава 21. Работа с файлами

- [ ] Работа с json  
- [x] Базовые операции с файлами  
- [x] Логирование  
- [x] Задача на кастомный Split до точки с запятой
- [x] Рассказать подробнее про `os.Rename`, когда файл перемещают. Чтобы переместить файл честно, `os.Rename` не подойдет.
- [ ] Добавить введение.
- [ ] Добавить 1-2 задачи.

## Основные пакеты для работы с файлами

* `os` — базовые операции: открытие, создание, удаление, переименование, изменение прав.
* `io` — инструменты для чтения/записи.
* `bufio` — буферизированные чтение/запись для производительности и удобства. Пакет позволяет, например, читать по словам.
* `fmt` — форматированный вывод в файл.
* `encoding` — для работы с JSON, CSV, XML и другими форматами.
* `log` — классический и простой пакет для логирования. 
* `log/slog` — пакет для структурированного логирования.

## Чтение файла 

Когда файл небольшой, его читают целиком: 

```go
package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	data, err := os.ReadFile("config.json")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(data))
}
```

Пример результата: 

```json
{
  "server": {
    "host": "localhost",
    "port": 8080,
    "timeout_seconds": 30
  },
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "admin",
    "password": "secret123",
    "name": "app_db"
  },
  "logging": {
    "level": "info",
    "file": "app.log",
    "max_size_mb": 100
  },
  "features": {
    "debug_mode": true,
    "enable_cache": false
  },
  "api_keys": ["key123", "key456", "key789"]
}
```

Однако представьте себе ситуацию, что файл большой, а памяти мало. Как быть, когда файл не помещается в память целиком? В этом случае его читают по частям:

```go
package main

import (
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	// Открываем файл на чтение
	file, err := os.Open(".log")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		// Обязательно закрыть!
		err = file.Close()
		if err != nil {
			log.Fatal(err)
		}
	}()
	buf := make([]byte, 128) // 128B буфер
	for {
		// Читаем в буфер
		n, err := file.Read(buf)
		// Получим ошибку io.EOF,
		// когда дойдем до конца файла
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		if n > 0 {
			fmt.Print(string(buf[:n]))
		}
	}
}
```

Результат — содержимое файла. Пример результата: 

```
[2026-06-11 10:15:23] Server started
[2026-06-11 10:15:23] User admin logged in
[2026-06-11 10:15:23] Request processed /api/users
[2026-06-11 10:15:23] Server stopped
```

**Всегда закрывайте за собой файл!**   
В противном случае возникнет утечка памяти.

Когда нужно прочесть файл построчно, используют пакет `bufio`:

```go 
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open(".log")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		err = file.Close()
		if err != nil {
			log.Fatal(err)
		}
	}()
	scanner := bufio.NewScanner(file)
	// Читаем построчно.
	// В случае ошибки вернется false
	// В случае конца файла также 
	// вернется false. 
	// В противном случае — true.
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Println(line)
	}
	// В случае конца файла ошибка равна nil
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
```
Пример результата: 

```
[2026-06-11 10:15:23] Server started
[2026-06-11 10:15:23] User admin logged in
[2026-06-11 10:15:23] Request processed /api/users
[2026-06-11 10:15:23] Server stopped
```

По умолчанию `bufio.NewScanner` возвращает значение типа `*bufio.Scanner`, для которого определена функция `split`. Она-то и отвечает за то, что мы читаем файл построчно. В `bufio` существует несколько встроенных функций, которые задают в качестве `split`, когда хотят получить другое поведение. Делают это с помощью метода `Split`:

```go
// Встроенные функции разделения в пакете bufio:
scanner.Split(bufio.ScanLines)   // По строкам — по умолчанию
scanner.Split(bufio.ScanWords)   // По словам
scanner.Split(bufio.ScanRunes)   // По рунам
scanner.Split(bufio.ScanBytes)   // Побайтово
```

Иногда встроенных функций не хватает. В этом случае приходится написать собственную. Сигнатура для функции `split` такая: 

```go
func(data []byte, atEOF bool) (advance int, token []byte, err error)
```

Параметры:
* `data` — непрочитанные данные в буфере.
* `atEOF` — `true`, если это последняя часть данных — достигнут конец файла.

Возвращаемые значения:
* `advance` — на сколько байт продвинуться в буфере. 
* `token` — найденный токен — то, что вернёт `scanner.Text`.
* `err` — ошибка.

Рассмотрим, как считать файл по частям, разделенным символом запятой.

```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("dinner.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		err = file.Close()
		if err != nil {
			log.Fatal(err)
		}
	}()
	scanner := bufio.NewScanner(file)
	scanner.Split(ScanComma)
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Println(line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func ScanComma(data []byte, atEOF bool) (advance int,
	token []byte, err error) {
	if atEOF && len(data) == 0 {
		// Возвращаем последний токен
		return 0, nil, nil
	}
	for i := range len(data) {
		if data[i] == ',' {
			// Нашли запятую, возвращаем токен до неё
			return i + 1, data[:i], nil
		}
	}
	return len(data), data, nil
}
```

`dinner.txt`:
```
apple,banana,orange,grape 
```

Результат: 
```
apple
banana
orange
grape 
```

В файле `input.txt` находится фрагмент кода на языке C++. Необходимо вывести код частями. Части отделяются друг от друга точкой с запятой `;`. Эта точка с запятой не должна находиться в кавычках. Когда точка с запятой внутри кавычек, то она является просто частью строки. Считайте, что текст анализируемой программы всегда правильный. Также в этой программе используются только двойные кавычки, и не встречаются кавычки внутри кавычек. {.task_text}

Воспользуйтесь `bufio.NewScanner`. В качестве функции `split` возьмите `ScanInstruction`, тело которой вам необходимо реализовать.  {.task_text}

```go {.task_source #golang_chapter_0210_task_0010}
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	// ваш код здесь 
}

func ScanInstruction(data []byte, atEOF bool) (advance int,
	token []byte, err error) {
	// ваш код здесь 
}
```

Код для `main` аналогичен примеру для чтения текста по частям, разделенными запятой. Работа с последним токеном в `ScanInstruction` аналогична `ScanComma`. {.task_hint}


```go {.task_answer}
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer func() {
		err = file.Close()
		if err != nil {
			log.Fatal(err)
		}
	}()
	scanner := bufio.NewScanner(file)
	scanner.Split(ScanInstruction)
	for scanner.Scan() {
		line := scanner.Text()
		fmt.Print(line)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func ScanInstruction(data []byte, atEOF bool) (advance int,
	token []byte, err error) {
	if atEOF && len(data) == 0 {
		// Возвращаем последний токен
		return 0, nil, nil
	}
	inQuote := false // внутри кавычек
	for i := range len(data) {
		if data[i] == '"' {
			// Переключаем состояние кавычек
			inQuote = !inQuote
		} else if !inQuote && data[i] == ';' {
			// Нашли точку с запятой вне кавычек
			return i + 1, data[:i], nil
		}
	}
	return len(data), data, nil
}
```

## Запись в файл 

Бывает необходимость как писать в существующий файл, так и в новый. Рассмотрим оба варианта.  
Запись в новый файл: 

```go {.example_for_playground}
package main

import (
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Create("pc.txt")
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()
	// Запись строки
	_, err = file.WriteString("CPU AMD Ryzen 5 7600X\n")
	exitOnError(err)
	// Запись байтов
	data := []byte("Memory 16 ГБ (2x8 ГБ) DDR5-6000 МГц\n")
	_, err = file.Write(data)
	exitOnError(err)
	storage := "SSD 2 ТБ M.2 NVMe"
	os := "Ubuntu 22.04"
	// Форматированная запись
	fmt.Fprintf(file, "Storage: %s, OS: %s\n", storage, os)
}

func exitOnError(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
```
```
CPU AMD Ryzen 5 7600X
Memory 16 ГБ (2x8 ГБ) DDR5-6000 МГц
Storage: SSD 2 ТБ M.2 NVMe, OS: Ubuntu 22.04
```
Запись множества мелких записей в файл с буфером и без него:

```go {.example_for_playground}
package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"math/rand"
	"os"
	"time"
)

const measureNumber = 10000

func main() {
	// Открываем существующий файл с правами 0644 в режиме
	// os.O_WRONLY — только на запись
	file, err := os.OpenFile("sensor.txt", os.O_WRONLY, 0644)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()
	withoutBuffer(file)
	// Чистим файл
	err = file.Truncate(0)
	exitOnError(err)
	_, err = file.Seek(0, 0)
	exitOnError(err)
	withBuffer(file)
}

func withoutBuffer(file *os.File) {
	// Вызываем trackTime
	// и откладываем выполнение
	// возвращаемой ею функции
	defer trackTime("withoutBuffer")()
	systemInfo(file)
}

func withBuffer(file *os.File) {
	defer trackTime("withBuffer")()
	writer := bufio.NewWriter(file)
	systemInfo(writer)
	// Пишем буферизованный результат
	// в файл
	err := writer.Flush()
	exitOnError(err)
}

func systemInfo(writer io.Writer) {
	r := rand.New(rand.NewSource(42))
	for i := range measureNumber {
		fmt.Fprintf(writer, "%d\tsensor value:\t%.2f\n",
			i+1, r.Float64())
	}
}

// Функция trackTime замеряет время работы
// функции с именем funcName
func trackTime(funcName string) func() {
	start := time.Now()
	return func() {
		elapsed := time.Since(start)
		fmt.Printf("%s: %v\n", funcName, elapsed)
	}
}

func exitOnError(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
```
```
withoutBuffer: 70.401288ms
withBuffer: 11.412507ms
```

Вывод будет немного различаться от запуска к запуску.  
Обратите внимание, что запись в файл с буфером в несколько раз быстрее, чем без него. Это объясняется тем, что операция обращения к файлу дорогая. Гораздо выгоднее сначала накопить много мелких записей в памяти, а потом один раз запись их все в файл.


Также рассмотрим строчку: 

```go
file, err := os.OpenFile("sensor.txt", os.O_WRONLY, 0644)
```

Здесь `0644` — [права](https://ru.wikipedia.org/wiki/Chmod) для работы с файлом на Linux.  

`os.O_WRONLY` — режим открытия файла, в котором разрешена только запись. Старый текст будет удален, и запись начнется сначала.

Чтобы дописать в файл, поступают таким образом: 

```go
file, err := os.OpenFile("sensor.txt", os.O_WRONLY|os.O_APPEND, 0644)
```

Подробнее про различные режимы открытия файла написано на официальном сайте Go, на странице про пакет `os`, в разделе [Constants.](https://pkg.go.dev/os#pkg-constants) 

## Удаление файла, перемещение, переименование, изменение прав 

Чтобы удалить файл, используют функцию `os.Remove`:

```go
err := os.Remove("trash.txt")
if err != nil {
	log.Fatalf("Ошибка удаления: %v\n", err)
}
log.Println("Файл успешно удалён")
```

Чтобы переименовать файл, нужно вызвать функцию `os.Rename`:

```go
err := os.Rename("old_name.txt", "new_name.txt")
if err != nil {
	log.Fatalf("Ошибка переименования: %v\n", err)
}
log.Println("Файл успешно переименован")
```

Переместить файл в Go — это то же самое, что и переименовать его:

```go
err := os.Rename("nature.pdf", "journals/nature.pdf")
if err != nil {
	log.Fatalf("Ошибка перемещения: %v\n", err)
}
log.Println("Файл успешно перемещен")
```

Отметим, что такое перемещение файла является не совсем честным. Это работает только в рамках одной файловой системы. Если вы попытаетесь переместить файл на другой диск, то `os.Rename` вернет ошибку. 

Дело в том, что `os.Rename` меняет только метаданные о файле. Функция физически не перемещает сам файл. Когда нужно это сделать, то сначала копируют файл в новое местоположение, а затем удаляют старый:

```go
package main

import (
	"errors"
	"io"
	"log"
	"os"
)

func main() {
	exitOnError(mv("dinner.txt", "lunch.txt"))
}

func mv(src string, dst string) (err error) {
	srcFile, err := os.Open(src)
	if err != nil {
		return err
	}
	defer func() {
		e := srcFile.Close()
		if e != nil {
			// Объединяем две ошибки 
			err = errors.Join(err, e)
		}
	}()
	dstFile, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer func() {
		e := dstFile.Close()
		if e != nil {
			err = errors.Join(err, e)
		}
	}()
	_, err = io.Copy(dstFile, srcFile)
	if err != nil {
		return err
	}
	err = os.Remove(src)
	if err != nil {
		return err
	}
	return nil
}

func exitOnError(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
```

Права файла изменяют с помощью `os.Chmod`:

```go 
err := os.Chmod("journals/nature.pdf", 0644)
if err != nil {
	log.Fatalf("Ошибка изменения прав: %v\n", err)
}
log.Println("Права успешно изменены!")
```

## Классическое логирование 

Чтобы сделать логирование через пакет `log`, достаточно воспользоваться `log.Println`. Однако чаще всего нужно сделать предварительную настройку. Например, чтобы писать лог в файл и выводить сообщения в определенном формате. Функция `log.SetOutput` позволяет сделать вывод в файл. Функция `log.SetFlags` устанавливает флаги, определяющие некоторые свойства логирования. С помощью этих флагов, например, выводят дату и время в каждой строке лога. Функция `log.SetPrefix` устанавливает префикс вначале каждой записи.

Пример демонстрирует, как сделать логи и их ротацию. Ротация логов — это процесс управления логами. В результате ротации логов старые записи архивируются или удаляются. Для новых данных создаются свежие пустые файлы.

```go {.example_for_playground}
package main

import (
	"errors"
	"fmt"
	"log"
	"os"
	"time"
)

// Для демонстрации заданы небольшие значения
// В реальных системах таких значений не будет
const sizeLimit = 1 // 1B
const timeLimit = 5 * time.Second

func main() {
	// 1. Создаём лог-файл
	// Создать папку logs
	err := makeDir("logs")
	exitOnError(err)
	file, err := os.OpenFile("logs/app.log", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	exitOnError(err)
	defer file.Close()

	// Настройка лога
	log.SetOutput(file)
	log.SetFlags(log.Ldate | log.Ltime)
	log.SetPrefix("[APP] ")
	log.Println("Приложение запущено")

	// 2. Ротация при достижении лимита
	info, err := file.Stat()
	exitOnError(err)
	if info.Size() > sizeLimit { 
		// Перемещаем в архив
		err = file.Close()
		exitOnError(err)
		err = makeDir("logs/archive")
		exitOnError(err)
		err = os.Rename("logs/app.log", fmt.Sprintf("logs/archive/app_%s.log",
			time.Now().Format("2006-01-02T15-04-05")))
		exitOnError(err)
		// Создаём новый файл
		file, err = os.OpenFile("logs/app.log", os.O_CREATE|os.O_WRONLY, 0644)
		exitOnError(err)
		log.SetOutput(file)
		log.Println("Лог-файл ротирован")
	}

	// 3. Удаляем старые архивы (старше timeLimit)
	files, _ := os.ReadDir("logs/archive")
	for _, f := range files {
		info, _ := f.Info()
		if time.Since(info.ModTime()) > timeLimit {
			err = os.Remove("logs/archive/" + f.Name())
			exitOnError(err)
			log.Printf("Удалён архив: %s", f.Name())
		}
	}

	log.Println("Приложение завершено")
}

func exitOnError(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func makeDir(dir string) (err error) {
	info, err := os.Stat(dir)
	if errors.Is(err, os.ErrNotExist) ||
		!info.IsDir() {
		err = os.Mkdir(dir, 0755)
	}
	return
}
```

## Логирование через log/slog

Пакет `log/slog` позволяет делать структурированное логирование. Такой лог удобно, например, обрабатывать автоматическими средствами. Приятный бонус — возможность задать уровень логирования из коробки. 

Уровни логирования:

```
1. slog.LevelDebug
2. slog.LevelInfo
3. slog.LevelWarn
4. slog.LevelError
```
Сообщения в лог проходят только на данном уровне и на всех уровнях выше. Например, информационное сообщение, написанное в лог c уровнем `slog.LevelWarn`, не попадет в лог. Однако предупреждения и ошибки попадут в лог.

```go {.example_for_playground}
package main

import (
	"log/slog"
	"os"
)

func main() {
	opts := &slog.HandlerOptions{
		// пишем в лог только предупреждения и ошибки
		Level: slog.LevelWarn,
	}

	// пишем лог в stdout
	handler := slog.NewTextHandler(os.Stdout, opts)
	logger := slog.New(handler)
	slog.SetDefault(logger)
	slog.Info("this message is not in the log")
	slog.Error("database is not available, reconnecting...")
}
```
```
time=2026-10-12T13:15:54.871+03:00 level=ERROR msg="database is not available, reconnecting..."
```

Вместо `os.Stdout` задают файл, когда нужно записать лог именно в файл.

## Работа с json 

