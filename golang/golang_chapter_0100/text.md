# Глава 10. Строки

В Go строка `string` — это срез байтов, доступный только для чтения. Строки необязательно содержат читаемый для человека текст, однако чаще всего это так. Встроенная функция `len` возвращает количество байтов (**не символов!**) в строке. Так, программа ниже выведет на экран число 6.

```go {.example_for_playground .example_for_playground_001}
import (
	"fmt"
)

func main() {
	s := "hello!"
	fmt.Println(len(s))
}
```

Однако вот эта программа выведет на экран число 13:

```go {.example_for_playground .example_for_playground_002}
import (
	"fmt"
)

func main() {
	s := "привет!"
	fmt.Println(len(s))
}
```

