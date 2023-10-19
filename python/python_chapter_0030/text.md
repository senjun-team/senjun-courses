# Глава 3. Условия, циклы, pattern matching

В этой главе вы узнаете, как работать с циклами и условиями, расставлять комментарии и как выглядит сопоставление с образцом (pattern matching).

## Комментарии
Комментарии в питоне могут быть однострочными. Они начинаются с символа `#`:

```python
# TODO: refactor coords calculation
x = x_offset()  # get x offset
```

...И многострочными. Такие комментарии обрамляются тройными кавычками. Если многострочный комментарий следует сразу за объявлением модуля, функции, класса или метода, то он называется docstring. Большинство современных IDE умеют делать подсказки на основе docstring. 

Пример docstring для функции:

```python
def get_coords():
    """Here we calculate x, y and transform
    these coordinates to lat, lon"""
    pass
```

## Условие if/elif/else
Синтаксис условных выражений в питоне ничем принципиально не отличается от других языков:

```python
if data_is_loaded:
    process_data()
elif session_is_open():
    check_data()
elif data_is_corrupted():
    retry()
else:
    wait(timeout)
```

Блоки `elif` и `else` не обязательны, а `elif` можно повторять сколько угодно раз. Фактически `if/elif` — это замена популярной конструкции `switch/case` в других языках. 

Также в питоне реализован мощный механизм [сопоставления с образцом](https://ru.wikipedia.org/wiki/%D0%A1%D0%BE%D0%BF%D0%BE%D1%81%D1%82%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5_%D1%81_%D0%BE%D0%B1%D1%80%D0%B0%D0%B7%D1%86%D0%BE%D0%BC) (pattern matching). Но `match/case` — не просто аналог `switch/case` для сравнения переменной с набором значений, а гибкая конструкция для анализа и обработки переменных, их распаковки и управления потоком выполнения. Ее мы рассмотрим ниже.

Сделайте этот код более читабельным, уменьшив вложенность условий: откажитесь от вложенных `if` в пользу единого блока `if/elif/else`. {.task_text}

```python {.task_source #python_chapter_0030_task_0010}
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
```{.task_hint}
Должно получиться условие, состоящее из одного if, двух elif и одного else.
```

## Тернарный if
Условие `if/else` можно записать более компактно, используя тернарный опертор:

```python
res = "OK" if code == 200 else "Error"
```

Этот пример трактуется так: присвоить переменной `res` значение `"OK"`, если `code` равен 200. Иначе присвоить `res` значение `"Error"`.


На строке 2 напишите тернарный оператор, присваивающий переменной `s_descr` значение `"long string"`, если `s` длиннее 79 символов, и `"short string"`, если это не так. Для определения нкцию `len()`. Аргументом `len()` является строка.  {.task_text}

```python {.task_source #python_chapter_0030_task_0020}
s = "Explicit is better than implicit."

```
```{.task_hint}
s_descr = "long string" if len(s) > 79 else "short string"
```

Замените `if/else` на тернарный оператор:  {.task_text}

```python {.task_source #python_chapter_0030_task_0030}
for val in [8, 3, 16]:
    if val % 2 == 0:
        res = "even"
    else:
        res = "odd"

    print(val, res)
```
```{.task_hint}
res = "even" if val % 2 == 0 else "odd"
```

## pattern matching: когда if/else становится мало
В питоне 3.10 появилась новая фича: pattern matching (сопоставление с образцом). Звучит сложно, но под капотом это всего лишь синтаксический сахар над старым-добрым `if/elif/else`. Он позволяет более гибко сравнивать и обрабатывать значения. 

Начнем с простого:

```python
match val:
    case -1:
        print('Negative number')
    case 1:
        print('Positive number')
    case unknown_val:
    	print('Unexpected value:', unknown_val)
```

Этот пример проясняет два момента. Во-первых, при использовании `match/case` оператор `break` в конце `case` для прерывания прохода по всему `match` не нужен. Выход произойдет автоматом.

Во-вторых, в `case` можно захватывать переменные для использования внутри этого блока. В данном случае мы создали и вывели в консоль переменную `unknown_val`.

Перепишите этот `if/else` на `match/case`.  {.task_text}

```python  {.task_source #python_chapter_0030_task_0040}
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
```{.task_hint}
match option
```

Рассмотрим еще один пример. Для его цельного восприятия важно знать, что в питоне есть тип данных [список](/courses/python/chapters/python_chapter_0090/) (динамический массив). Список из нескольких элементов выглядит как перечисленные в квадратных скобках объекты. Вот список строк: 

```python
lst = ["exit", "copy", "delete"]
```

Допустим, консольное приложение ожидает команду и дополнительные опции от пользователя.

```python
# Читаем консольный ввод:
input = parse_input()
# Разбиваем полученную строку по пробелам
# и получаем список строк:
commands = input.split()

match commands:
    case ["exit"]:
        quit_app()
    case ["copy", path_src, path_dst]:
        copy_file(path_src, path_dst)
    case ["delete", path]:
        erase_file(path)
    case _:
        print ("Unsupported command", input)
```

Первый `case` сопоставляет список `commands` со списком из одного элемента `"exit"`. Второй `case` сопоставляет `commands` со списком из трех элементов (команда `"copy"`, путь к исходному файлу и путь, куда копировать). Причем переменные `path_src` и `path_dst` захватываются для передачи в функцию `copy_file()`. 

Третий `case` устроен похожим образом, а вот последний `case _:` нужен как дефолтный обработчик для не поддерживаемых команд. Символ `_` здесь означает «wildcard match», совпадение с чем-либо еще.

В этом примере уже проявляется вся мощь сопоставления шаблонов, демонстрирующая, зачем было вводить в язык новую конструкцию. А более продвинутые варианты использования pattern matching разберем в следующих главах.

## Циклы for и while
Цикл `while` — это всем известный классический `while` из JavaScript, C++, Java:

```python
x = 100

while x > 0:
    handle(x)
    x -= 1
```

Вариация вечного цикла:

```python
while True:
    rotate_spinner()
```

Этот цикл тоже является вечным, потому что 1 интерпретируется как «истина»:

```python
while 1:
    run()
```

Цикл `for` последовательно перебирает элементы коллекции:

```python
for resp in responses:
    parse(resp)
```

Пример прохода в цикле `for` по последовательности чисел, которую генерирует функция `range()`:

```python
for x in range(10):
    print(x * x)
```

Последовательность состоит из целых от 0 до 9, 10 в нее не попадает.

Имплементируйте тело функции. Пусть она возвращает количество слов, из которых состоит строка `s` в snake_case. Например, если на вход функции прилетает `s`, равная `"not_supported_format"`, функция вернет 3. А для `"naive_solution"` вернет 2.   {.task_text}

```python  {.task_source #python_chapter_0030_task_0050}
def get_words_count(s):
    # Your code here
```
```{.task_hint}
Можно пройтись циклом по строке и посчитать в ней количество символов '_'. На основании этого сделать вывод о количестве слов, которые '_' разделяет.
```

В питоне есть оператор `continue` для перехода на следующую итерацию цикла.
В этом цикле выведутся все согласные буквы:

```python
s = "Now is better than never."

for letter in s:
    if letter in "aeiouy":
        continue

    print(letter)
```

Также в питоне есть оператор `break` для выхода из цикла. Этот цикл завершится на первом встреченном пробеле:

```python
s = "Although never is often better than right now."

for letter in s:
    if letter == " ":
        break

    print(letter)
```

Забавная особенность питона: ключевое слово `else` может использоваться не только в связке с `if`, но и после циклов `while` и `for`! В таком случае `else` сработает, только если выход из цикла произошел **без** участия `break`. 

```python
s = "If the implementation is hard to explain, it's a bad idea."

for letter in s:
    if letter == "y":
        break

    print(letter)
else:
    print("There is no letter 'y' in string")
```

В данном примере в консоль будет выведен текст из блока `else`. Потому что из цикла `for` не было выхода по `break`.

Имплементируйте функцию `print_letters()`. На вход она принимает строку. Функция анализирует каждую букву строки:{.task_text}
- Если в строке содержится заглавная буква, то печатает ее. Для определения, что буква заглавная, используйте метод `isupper()`.
- Если строчная — пропускает.
- Если найден пробел, то все следующие за ним символы пропускаются.
- Если в строке не было ни одного пробела, функция печатает `"no spaces"`.
 {.task_text}
 
```python {.task_source #python_chapter_0030_task_0060}
def print_letters(s):
    # Your code here
```
```{.task_hint}
Для выхода из цикла используйте 'break', для перехода на новую итерацию — 'continue', а для обработки условия не выхода по 'break' — 'else'.
```

## Резюмируем
- Однострочные комментарии начинаются с символа `#`, многострочные — заключаются в тройные кавычки: `"""TODO: fix this."""`.
- Для условного выполнения кода есть конструкции `if/elif/else` и тернарный `if`.
- Для сопоставления с образцом (pattern matching) используется конструкция `match/case`.
- В питоне есть два вида циклов: `for` и `while`. 
- Циклы поддерживают операторы `break`, `continue` и `else`. `else` срабатывает, если из цикла не было выхода по  `break`.
