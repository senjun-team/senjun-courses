# Глава 24. Comprehensions, generator expressions

Питон славен своей краткостью. Comprehensions — его культовая фишка, позволяющая писать эффективный и компактный код для обработки коллекций. Термин comprehension сложно перевести на русский язык. List comprehensions иногда переводят как «генераторы списков», а set и dict comprehensions — как генераторы множеств и словарей. Эти переводы не очень распространены, поэтому мы будем использовать английские варианты.

Начинающих питонистов comprehensions способны отпугнуть из-за своего предельно лаконичного, но не всегда понятного синтаксиса. Давайте разберемся, как готовить их правильно.

## List comprehensions
Один из печально известных анти-паттернов разработки на питоне — это заполнение списка в цикле:

```python
numbers = []

for i in range(10):
    numbers.append(i * i)
```

Для такого простого действия используется аж три строки. Подобная многословность считается крайне [не идиоматичной.](https://realpython.com/lessons/zen-of-python/) Благодаря list comprehension количество строк редуцируется до одной:

```python  {.example_for_playground}
numbers = [i * i for i in range(10)]
```

Самый простой list comprehension выглядит следующим образом:
```
<list_object> = [<expression> for <member> in <iterable>]
```

Он формирется из 3-х обязательных частей, обернутых в квадратные скобки:

- expression — выражение, возвращающее новый элемент списка,
- member — объект, используемый в expression,
- iterable — итерабельный объект: список, генератор, корутина и т.д.

К преимуществам использования list comprehension можно отнести:
- лаконичный способ заполнения списков,
- скорость выполнения: производительнее, чем `for` из-за оптимизаций интерпретатора, но медленее, чем [встроенные функции высших порядков](/courses/python/chapters/python_chapter_0280/) `map()` и `filter()`.

Перепишите заполнение списка чисел на list comprehenshion с вызовом `range()`. {.task_text}

```python {.task_source #python_chapter_0240_task_0010}
dates = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
```
Вызов `range()` выглядит следующим образом: range(1, 31). {.task_hint}
```python {.task_answer}
dates = [date for date in range(1, 31)]
```

Перепишите list comprehension из предыдущей задачи так, чтобы получился список значений в шестнадцатеричной системе счисления в формате строк. {.task_text}

Для получения шестнадцатеричного строкового представления числа воспользуйтесь встроенной функцией `hex()`. {.task_text}

```python {.task_source #python_chapter_0240_task_0020}
```
Внутри list comprehension воспользуйтесь вызовом `range(1, 31)`. {.task_hint}
```python {.task_answer}
dates = [hex(date) for date in range(1, 31)]
```

С помощью list comprehension удобно фильтровать данные:
```
<list_object> = [<expression> for <member> in <iterable> if <condition>]
```

```python  {.example_for_playground}
three_divisable = [i for i in range(31) if i % 3 == 0]
print(three_divisable)
```
```
[0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30]
```

В этом случае мы отбросили все числа, которые не делятся на 3. 

А если бы мы хотели вместо них сохранить в список что-то другое? Для этого нужно переместить `if` в выражение, возвращающее элемент списка:

```
<list_object> = [<expression1> if <condition> else <expression2> for <member> in <iterable>]
```

```python  {.example_for_playground}
three_divisable = [i if i % 3 == 0 else -1 for i in range(10)]
print(three_divisable)
```
```
[0, -1, -1, 3, -1, -1, 6, -1, -1, 9]
```

С помощью list comprehension пройдитесь по диапазону от 0 до 19 включительно. Если встретили четное число, поместите в список `lst` строку `"even"`, а если нечетное — `"odd"`. {.task_text}

```python {.task_source #python_chapter_0240_task_0030}
```
Для формирования строки воспользуйтесь тернарным оператором: `"even" if x % 2 == 0 else "odd"`. {.task_hint}
```python {.task_answer}
lst = ["even" if x % 2 == 0 else "odd" for x in range(0, 20)]
```

Даны два списка `l1` и `l2`. С помощью list comprehension составьте список `common` из их общих элементов. {.task_text}

```python {.task_source #python_chapter_0240_task_0040}
l1 = [1, 2, 3, 4, 5]
l2 = [4, 5, 6, 7, 8]
```
Добавляйте в список `common` только элементы, присутствующие в `l2`. {.task_hint}
```python {.task_answer}
common = [number for number in l1 if number in l2]
```

List comprehensions могут быть вложенными!

Допустим, у нас есть список списков `numbers`, и мы хотим получить из него плоский список `squares` с квадратами четных чисел. Этого можно добиться с помощью цикла и одного list comprehension:

```python  {.example_for_playground}
numbers = [[1, 2, 3, 4, 5], [6, 7]]

squares = []
for l in numbers:
   res = [n**2 for n in l if n%2 == 0]
   squares.extend(res)

print(squares)
```
```
[4, 16, 36]
```

Альтернативный вариант — организовать вложенный list comprehension:

```python  {.example_for_playground}
numbers = [[1, 2, 3, 4, 5], [6, 7]]

squares = [n**2 for l in numbers for n in l if n%2 == 0]
print(squares)
```
```
[4, 16, 36]
```

Перепишите литерал на list comprehension с вызовом `range()`. {.task_text}

```python {.task_source #python_chapter_0240_task_0050}
matrix = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]
```
Должен получиться вложенный list comprehension для итерации от 0 до 2-х и для итерации от 1 до 3-х. {.task_hint}
```python {.task_answer}
matrix = [
    [x for x in range(1, 4)]
    for y in range(3)
]
```

## set comprehension, dict comprehension
По аналогии с `list comprehension` в питоне есть возможность лаконично создавать множества и словари:

```python  {.example_for_playground}
s = {i for i in [1, 2, 3, 1, 2, 4]}
print(s)
```
```
{1, 2, 3, 4}
```

```python  {.example_for_playground}
d = {i: i for i in range(10)}
print(d)
```
```
{0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8, 9: 9}
```

Через list comprehension сформируйте список `rps`, содержащий только значения поля `"max_rps"` словаря `apis`: `[3000, 1100, 4000]`. {.task_text}

```python {.task_source #python_chapter_0240_task_0060}
apis = [
    {
        "name": "search engine",
        "max_rps": 3000,
    },
    {
        "name": "analytics",
        "max_rps": 1100,
    },
    {
        "name": "crawler", 
        "max_rps": 4000
    }
]
```
Результирующими элементами списка `rps` должны быть значения "max_rps" словарей из списка `apis`. {.task_hint}
```python {.task_answer}
apis = [
    {
        "name": "search engine",
        "max_rps": 3000,
    },
    {
        "name": "analytics",
        "max_rps": 1100,
    },
    {
        "name": "crawler", 
        "max_rps": 4000
    }
]

rps = [d["max_rps"] for d in apis]
```

Сделайте вложенный объект `apis` плоским. То есть получите список `plain`, в котором перечислены все значения из словаря: `['search engine', 3000, 'analytics', 1100, ...]`. {.task_text}

```python {.task_source #python_chapter_0240_task_0070}
apis = [
    {
        "name": "search engine",
        "max_rps": 3000,
    },
    {
        "name": "analytics",
        "max_rps": 1100,
    },
    {
        "name": "crawler", 
        "max_rps": 3000
    }
]
```
Проитерируйтесь по значениям словаря с помощью метода `values()`. {.task_hint}
```python {.task_answer}
apis = [
    {
        "name": "search engine",
        "max_rps": 3000,
    },
    {
        "name": "analytics",
        "max_rps": 1100,
    },
    {
        "name": "crawler", 
        "max_rps": 3000
    }
]

plain = [v for d in apis for v in d.values()]
```

## Оператор моржа внутри comprehensions {#block-walrus}
[Оператор моржа](/courses/python/chapters/python_chapter_0030#block-walrus) `:=` может использоваться внутри comprehensions для того чтобы избежать повторных вычислений одних и тех же данных. 

Рассмотрим пример:

```python  {.example_for_playground}
def normalize(word):
    return word.strip().lower()

words = ["list ", " Comprehensions", ". "]
normalized = [n for w in words if len(n := normalize(w)) > 1]

print(normalized)
```
```
['list', 'comprehensions']
```

Здесь мы воспользовались оператором моржа, чтобы лишний раз не вызывать функцию нормализации строки: она нам потребовалась и для фильтрации данных, и для сохранения их в результирующий список. Если бы в языке не было синтаксиса для присваивания значения и возвращения его в едином выражении, в нашем примере пришлось бы вызывать `normalize()` дважды.

Приоритет оператора моржа даже ниже, чем у оператора сравнения. Поэтому зачастую его приходится брать в скобки: `if (n := words_count(text)) > max_len)`.

Перепишите этот list comprehension с использованием оператора моржа. Это позволит отказаться от повторного расчета куба числа и ускорит код. {.task_text}

```python {.task_source #python_chapter_0240_task_0080}
vals = [-100, 5, 19, 46, -99, 101]

cubes = [x**3 for x in vals if x**3 > 0]
```
При вызове `if` воспользуйтесь оператором моржа: `if (cube := x**3) > 0`. Созданную в этом месте переменную `cube` задействуйте при формировании списка `cubes`. {.task_hint}
```python {.task_answer}
vals = [-100, 5, 19, 46, -99, 101]

cubes = [cube for x in vals if (cube := x**3) > 0]
```

## Генераторные выражения {#generator-expressions}
При использовании comprehensions нужно помнить, что все данные в итоге попадают в список, множество или словарь. То есть занимают место в оперативной памяти. Представим, что нам нужно вычислить квадрат целого числа для первого миллиарда элементов. Если написать для этого list comprehension, есть вероятность, что компьютер попросту зависнет.

Когда размер получаемой последовательности становится слишком большим, на помощь приходит синтаксис генераторных выражений (generator expressions). От list comprehensions он отличается только наличием круглых скобок вместо квадратных. То есть вместо списка возвращается генератор:

`<generator_object> = (<expression> for <member> in <iterable>)`

В [прошлой главе](/courses/python/chapters/python_chapter_0230/) мы писали, что в теле генератора должен присутствовать оператор `yield`. Так вот, generator expression — это альтернативный способ создания генераторов. Он точно также реализует концепцию ленивых вычислений и отдает следующий элемент только при обращении к генератору. В нем нет слова `yield`, но `generator_object` все равно считается генератором.

```python  {.example_for_playground}
g = (i * i for i in range(1000000000))
print(sys.getsizeof(g))
print(sum(g))
```
```
208
333333332833333333500000000
```

Мы посчитали сумму квадратов чисел до миллиарда. При этом в памяти хранился всего один объект генератора с фиксированным размером.

## Что под капотом у comprehensions?

Мы познакомились с comprehensions для списков, множеств и словарей. А что насчет кортежей? В питоне отсутствуют tuple comprehensions: синтаксис круглых скобок уже занят генераторными выражениями. Как раз с их помощью и можно инициализировать кортеж:

```python  {.example_for_playground}
t = tuple(letter for letter in "generator expression")
print(t)
```
```
('g', 'e', 'n', 'e', 'r', 'a', 't', 'o', 'r', ' ', 'e', 'x', 'p', 'r', 'e', 's', 's', 'i', 'o', 'n')
```

В этом примере мы вызвали конструктор `tuple()` от генераторного выражения, которое является итерабельным объектом. Ничто не мешает нам поступить так же и со списком, множеством и словарем:

```python  {.example_for_playground}
data = "data"

l = list(x for x in data)
print(l)

s = set(x for x in data)
print(s)

d = dict((x, x.upper()) for x in data)
print(d)
```
```
['d', 'a', 't', 'a']
{'t', 'd', 'a'}
{'d': 'D', 'a': 'A', 't': 'T'}
```
Но стоит помнить `generator expression` - это не про скорость, а про сохранение памяти, потому что каждый вызов `next`/`yield` внутри генератора - это накладные расходы

Вы только что увидели, как выглядят comprehensions. Это не более чем синтаксический сахар, надстройка над вызовом конструктора типа от генераторного выражения, но не list comprehensions.

## Почему list comprehensions быстрее обычных циклов?
Хотя все comprehensions можно выразить через конструкторы и генераторные выражения, list comprehensions имеют важную оптимизацию в CPython, делающую их быстрее обычных циклов. Рассмотрим детали:

### 1. Специализированный байт-код для list comprehensions

List comprehensions компилируются в более эффективный байт-код, который:

1. [Использует опкод**](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Python/bytecodes.c#L1082) `LIST_APPEND` вместо вызова метода append() у питоновского списка
2. Избегает поиска опкодом `LOAD_ATTR` в каждой итерации, потому что работает с локальной переменной
3. Уменьшает количество операций вызова опкода  `CALL`, который является дорогой операцией 
4. Оптимизация работы c [переменной](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Python/bytecodes.c#L317) (`STORE_FAST`, `LOAD_FAST`) и [памятью](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Python/bytecodes.c#L276) `LOAD_FAST_AND_CLEAR`.
Это позволяет быстрее освобождать ссылки и уменьшает нагрузку на сборщик мусора.  
Такая оптимизация особенно заметна при работе с большими объёмами данных.

** *(от англ. operation code — «код операции», особые операции для Python Virtual Machine (PVM))*

Рассмотрим работу со списком в цикле for:
```python
import dis
dis.dis('''
lst = []
for x in range(10):
    lst.append(x)
''')
```

```commandline
  0           0 RESUME                   0

  2           2 BUILD_LIST               0                          # lst = []
              4 STORE_NAME               0 (lst)                    # Сохраняем в глобальной области

  3           6 PUSH_NULL
              8 LOAD_NAME                1 (range)
             10 LOAD_CONST               0 (10)
             12 CALL                     1
             20 GET_ITER
        >>   22 FOR_ITER                19 (to 64)
             26 STORE_NAME               2 (x)                      # x в глобальной области видимости

  4          28 LOAD_NAME                0 (lst)                    # Проблемная зона, т.к. lst в глобальной области
             30 LOAD_ATTR                7 (NULL|self + append)     # Поиск метода каждый раз в цикле
             50 LOAD_NAME                2 (x)                      # Загрузка x из глобальной области каждый раз в цикле
             52 CALL                     1                          # Дорогой вызов функции каждый раз в цикле
             60 POP_TOP
             62 JUMP_BACKWARD           21 (to 22)

  3     >>   64 END_FOR
             66 RETURN_CONST             1 (None)
```
А теперь рассмотрим оптимизированный байт-код у list comprehensions
```python
import dis
dis.dis('[x for x in range(10)]')
```
```commandline
  0           0 RESUME                   0

  1           2 PUSH_NULL
              4 LOAD_NAME                0 (range)
              6 LOAD_CONST               0 (10)
              8 CALL                     1
             16 GET_ITER
             18 LOAD_FAST_AND_CLEAR      0 (x)        # Ключевая оптимизация для comprehensions.
                                                      # Очищает предыдущее значение переменной x (если оно было) и помечает её как локальную. 
                                                      # Это нужно, чтобы избежать утечек переменных из внешней области видимости.
             20 SWAP                     2
             22 BUILD_LIST               0
             24 SWAP                     2
        >>   26 FOR_ITER                 4 (to 38)
             30 STORE_FAST               0 (x)         # Для работы с локальной переменной x, а не глобальной
             32 LOAD_FAST                0 (x)         # Аналогично, эксклюзивно для comprehensions
             34 LIST_APPEND              2             # Вторая ключавая оптимизация работает на уровне C, 
                                                       # которая выполняется за одну инструкцию, минуя поиск и вызов метода LOAD_ATTR 
             36 JUMP_BACKWARD            6 (to 26)
        >>   38 END_FOR
             40 SWAP                     2
             42 STORE_FAST               0 (x)         # Для работы с локальной переменной x, а не глобальной
             44 RETURN_VALUE
        >>   46 SWAP                     2
             48 POP_TOP
             50 SWAP                     2
             52 STORE_FAST               0 (x)
             54 RERAISE                  0
ExceptionTable:
  22 to 38 -> 46 [2]
```

Как видим, в list comprehensions нет вызова `LOAD_ATTR` и каждый раз не происходит вызов метода `CALL`
### 2. Оптимизация памяти
CPython предварительно определяет размер списка для list comprehensions:
- Лучше работает с итерируемыми объектами известной длины (range, кортежи, списки)
- Создает список сразу нужного размера, избегая `list_resize`
- Использует `PyList_New` для одноразовой аллокации памяти

### 3. Реализация в CPython
Ключевые компоненты в исходном коде Python:
- Компиляция ([Python/codegen.c](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Python/codegen.c#L4888)): Функция codegen_listcomp() генерирует оптимизированный байт-код

- Исполнение ([generated_cases.c.h](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Python/generated_cases.c.h#L7719)): Обработка опкода `LIST_APPEND` для list comprehensions

- Создание списка ([Objects/listobject.c](https://github.com/python/cpython/blob/1bde13b0e99592fbfce3538b27ada29ea09840a6/Objects/listobject.c#L235)): Функции `PyList_New` и `list_resize` для эффективного управления памятью.
Ключевой момент для понимания, почему включения быстрее простого цикла со списками это строчка с кодом `op->allocated = size;` где сразу указывается размер будущего списка и не нужно будет каждый раз при переполнении вызывать [формулу из 11 главы](https://senjun.ru/courses/python/chapters/python_chapter_0110/#block-lst-inner).

### Когда использовать list comprehensions?
#### Оптимально для:
- Простых преобразований данных (`[x*2 for x in iterable]`)
- Фильтрации с условиями (`[x for x in iterable if x > 0]`)
- Работы с итерируемыми объектами известной длины

#### Меньший выигрыш:
- При сложных вычислениях внутри цикла
- С потоками данных неизвестной длины
- Когда требуется побочные эффекты (лучше использовать обычные циклы для читаемости)

### Почему другие comprehensions не имеют такого прироста?
Оптимизация `LIST_APPEND` уникальна для списков, а вот для множеств и словарей:
- Нет специализированных инструкций байт-кода
- Используются обычные (питоновские) вызовы `add()` и `__setitem__()`
- Память не может быть предварительно выделена из-за хэширования

`List comprehensions` — это не просто синтаксический сахар. 
Их производительность — результат глубокой оптимизации в CPython, сочетающей эффективный байт-код и управление памятью. 


## Резюмируем
- Comprehension — конструкция языка, которая позволяет лаконично создавать и фильтровать списки, множества и словари.
- За счет повышения краткости иногда повышается сложность восприятия. Поэтому нужно быть осторожным с вложенностью в comprehensions.
- Генераторные выражения — однострочный синтаксис создания генератора без вызова `yield`.