# Глава 4. Условия, pattern matching

В этой главе вы узнаете, как работать с условными выражениями, а также как выглядит сопоставление с образцом (pattern matching).

## Условие if/elif/else
Условные выражения в питоне очень удобны и не уступают функционалу популярной конструкции `switch/case` из других языков.

В данном примере мы проверяем переменные `a` и `b` на равенство с помощью оператора `==` и сравниваем с нулем остаток от деления, полученный оператором `%`:

```python {.example_for_playground}
a = 3
b = 9

if a == b:
    print("a and b are equal")
elif a % b == 0:
    print("b is a divisor of a")
elif b % a == 0:
    print("a is a divisor of b")
else:
    print("else branch")
```
```
a is a divisor of b
```

В конструкции `if/elif/else` блоки `elif` и `else` не обязательны, а `elif` можно повторять сколько угодно раз.

Сделайте код функции `analyze_string()` более читабельным, уменьшив вложенность условий: откажитесь от вложенных `if` в пользу единого блока `if/elif/else`. {.task_text}

```python {.task_source #python_chapter_0040_task_0010}
def analyze_string(s):
    print("Analyzing string...")

    if s.isdigit():
        print("All characters are digits")
    else:
        if s.islower():
            print("All characters are lower case")
        else:
            if s.isalpha():
                print("All characters are in the alphabet")
            else:
                print("There is nothing special about this string")
    
    print("Finished string analysis")


analyze_string("Hint")
```
Должно получиться условие, состоящее из одного `if`, двух `elif` и одного `else`. {.task_hint}
```python {.task_answer}
def analyze_string(s):
    print("Analyzing string...")

    if s.isdigit():
        print("All characters are digits")
    elif s.islower():
        print("All characters are lower case")
    elif s.isalpha():
        print("All characters are in the alphabet")
    else:
        print("There is nothing special about this string")
    
    print("Finished string analysis")


analyze_string("Hint")
```

## Тернарный if
Условие `if/else` можно записать более компактно, используя тернарный оператор:

```python
res = "OK" if code == 200 else "Error"
```

Этот пример трактуется так: присвоить переменной `res` значение `"OK"`, если `code` равен 200. Иначе присвоить `res` значение `"Error"`.


На строке 2 напишите тернарный оператор, присваивающий переменной `s_descr` значение `"long string"`, если строка `s` длиннее 79 символов, и `"short string"`, если это не так. Для определения длины строки используйте функцию `len()`. Аргументом `len()` является строка.  {.task_text}

```python {.task_source #python_chapter_0040_task_0020}
s = "Explicit is better than implicit."

```
Синтаксис: `<variable> = <result_true> if <condition> else <result_false>` {.task_hint}
```python {.task_answer}
s = "Explicit is better than implicit."

s_descr = "long string" if len(s) > 79 else "short string"
```

Замените `if/else` на тернарный оператор. {.task_text}

```python {.task_source #python_chapter_0040_task_0030}
for val in [8, 3, 16]:
    if val % 2 == 0:
        res = "even"
    else:
        res = "odd"

    print(val, res)
```
Синтаксис: `<variable> = <result_true> if <condition> else <result_false>` {.task_hint}
```python {.task_answer}
for val in [8, 3, 16]:
    res = "even" if val % 2 == 0 else "odd"

    print(val, res)
```

## Pattern matching: когда if/else становится мало
В питоне 3.10 появилась новая фича: [pattern matching](https://ru.wikipedia.org/wiki/%D0%A1%D0%BE%D0%BF%D0%BE%D1%81%D1%82%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5_%D1%81_%D0%BE%D0%B1%D1%80%D0%B0%D0%B7%D1%86%D0%BE%D0%BC) (сопоставление с образцом). Звучит сложно, но под капотом это всего лишь синтаксический сахар над старым-добрым `if/elif/else`. Он позволяет более гибко сравнивать и обрабатывать значения, распаковывать коллекции в отдельные переменные, управлять потоком выполнения.

Начнем с простого:

```python {.example_for_playground}
val = 8

match val:
    case 0:
        print('OK')
    case 1:
        print('Error')
    case unknown_val:
        print('Unexpected value:', unknown_val)
```
```
Unexpected value: 8
```

Этот пример проясняет два момента. Во-первых, при использовании `match/case` оператор `break` в конце `case` для прерывания прохода по всему `match` не нужен. Выход произойдет автоматом.

Во-вторых, в `case` можно **создавать** переменные для использования внутри этого блока. В данном случае мы создали и вывели в консоль переменную `unknown_val`.

Зачастую в последнем блоке `case` требуется обработать «все остальные значения, неважно какие»:

```python
case something_else:
    print("Unsupported input. Exiting program")
```

В таком случае переменную, в которую захватывается значение, принято именовать символом `_`:

```python
case _:
    print("Unsupported input. Exiting program")
```

Такое именование — распространенная договоренность для «wildcard match», то есть совпадения с чем-либо еще.

Перепишите этот `if/else` на `match/case`.  {.task_text}

```python  {.task_source #python_chapter_0040_task_0040}
def parse_option(option):
    if option == "save_to_file":
        return "Saving data to file..."
    elif option == "log_statistics":
        return "Dumping stats to logs..."
    elif option == "quit":
        return "Quitting..."
    else:
        return "Unsupported option"
```
Не забудьте обработать случай, в котором требуется вернуть `"Unsupported option"`. {.task_hint}
```python {.task_answer}
def parse_option(option):
    match option:
        case "save_to_file":
            return "Saving data to file..."
        case "log_statistics":
            return "Dumping stats to logs..."
        case "quit":
            return "Quitting..."
        case _:
            return "Unsupported option"
```

Рассмотрим еще один пример. Для его цельного восприятия важно знать, что в питоне есть тип данных [список](/courses/python/chapters/python_chapter_0110/) (динамический массив). Список из нескольких элементов выглядит как перечисленные в квадратных скобках объекты. Вот список строк: 

```python
lst = ["exit", "copy", "delete"]
```

Допустим, консольное приложение ожидает команду и дополнительные опции от пользователя. В строку `user_input` мы читаем пользовательский ввод с помощью встроенной функции `input()`. Затем через метод строки `split()` разбиваем ее по пробелам и получаем список слов. К которому и применяем `match/case`:

```python
user_input = input("Please enter command: ")

commands = user_input.split()

match commands:
    case ["exit"]:
        exit(0)
    case ["copy", path_src, path_dst]:
        print(f"Copying file from {path_src} to {path_dst}...")
    case ["delete", path]:
        print(f"Deleting file {path}...")
    case _:
        print ("Unsupported command", user_input)
```

Первый `case` сопоставляет список `commands` со списком из одного элемента `"exit"`. 

Второй `case` сопоставляет `commands` со списком из трех элементов. Его тело выполнится, если список `commands` состоит из трех элементов, первый из которых совпадает со строкой "copy". Два других элемента списка могут содержать произвольные значения. Причем переменные `path_src` (путь к исходному файлу) и `path_dst` (путь, куда копировать) создаются и заполняются значениями в момент выполнения `case`. Внутри `case` их уже можно использовать. Например, выводить в консоль функцией `print()`.

Третий `case` устроен похожим образом, а вот последний `case _:` нужен как дефолтный обработчик для не поддерживаемых команд.

В этом примере уже проявляется вся мощь сопоставления шаблонов, демонстрирующая, зачем было вводить в язык новую конструкцию. А более продвинутые варианты использования pattern matching разберем в следующих главах.

## Резюмируем
- Для условного выполнения кода есть конструкции `if/elif/else` и тернарный `if`.
- Для сопоставления с образцом (pattern matching) используется конструкция `match/case`.

