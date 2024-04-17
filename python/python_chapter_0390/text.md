# Глава 39. Еще раз про pythonic way
> Readability counts.  
Дзен питона


Для того чтобы по-максимуму использовать преимущества языка, недостаточно знать его синтаксис. Приемы, считающиеся хорошей практикой в одном языке, назовут анти-паттернами, если они встретятся в коде на другом языке. 

В мире Java геттеры и сеттеры для полей класса — это норма, но переносить их в питон [не имеет смысла.](/courses/python/chapters/python_chapter_0160#block-fields) Невозможно представить код на go без возврата и проверки кодов ошибок функций. А для питона подобный подход слишком многословный: проще придерживаться [принципа EAFP](/courses/python/chapters/python_chapter_0190#block-eafp) и обрабатывать исключения. Подобных примеров сотни.

Иными словами, важно не просто уметь написать рабочий код, а сделать это *идиоматично.* В этой главе мы пойдем от обратного: рассмотрим популярные анти-паттерны питона. И варианты их рефакторинга.

## Работа со словарями
### Поиск элемента: if vs метод get()
Допустим, нам нужно найти в словаре `d` значение по ключу `k` и присвоить его переменной `x`. Если ключ не найден, то присвоить значение по умолчанию 1. Вот как выглядит громоздкое и неэффективное решение этой задачи:

```python
x = 1

if k in d:
    x = d[k]
```

`k in d` и `d[k]` — двойная работа по поиску элемента в словаре. А вместо трех строчек то же самое выражается одной:

```python
x = d.get(k, 1)
```

Мы воспользовались [методом словаря](/courses/python/chapters/python_chapter_0150#block-methods) `get(key, default_val=None)`.

Проведите рефакторинг поиска записи `"blog"` в словаре `clicks`. {.task_text}

```python {.task_source #python_chapter_0390_task_0010}
clicks = {"news": 15, "home": 102, "faq": 8}

count = 0

if "blog" in clicks:
    count = clicks["blog"]

print(f"{count=}")
```
Нужно вызвать метод `get()`: `clicks.get("blog", 0)`. {.task_hint}
```python {.task_answer}
clicks = {"news": 15, "home": 102, "faq": 8}

count = clicks.get("blog", 0)

print(f"{count=}")
```

### Обновление элемента: if vs класс defaultdict
Распространенная задача: изменить значение в словаре по ключу. При этом обработать ситуацию, если значение еще не было добавлено.

```python
d = {}

if k not in d:
    d[k] = []

d[k].append("some value")
```

Внутри `if` происходит повторное обращение по ключу, что не эффективно. То же самое легко выразить лаконичнее и без участия `if`. Достаточно заменить встроенный тип `dict` на `defaultdict` из [модуля](https://docs.python.org/3/library/collections.html#collections.defaultdict) `collections`:

```python
from collections import defaultdict

d = defaultdict(list)

d[k].append("some value")
```

Класс `defaultdict` наследован от `dict` и отличается от него только обработкой значений по умолчанию. Его конструктор `defaultdict(default_factory=None, /[, ...])` соответствует конструктору `dict` с добавлением аргумента `default_factory` для заполнения значений по умолчанию. 

В качестве `default_factory` может выступать `None` или вызываемый объект. В том числе лямбда или конструктор встроенных типов:

```python
from collections import defaultdict

d1 = defaultdict(lambda: 255)
d2 = defaultdict(str)
```

Откажитесь от стандартного словаря в пользу `defaultdict`. {.task_text}

```python {.task_source #python_chapter_0390_task_0020}
clicks = {}

if "news" not in clicks:
    clicks["news"] = 0

clicks["news"] += 1

print(clicks["news"])
```
Инициализация переменной `clicks`: `defaultdict(lambda : 0)`. {.task_hint}
```python {.task_answer}
from collections import defaultdict

clicks = defaultdict(lambda : 0)

clicks["news"] += 1

print(clicks["news"])
```

### Обновление элемента: if vs метод setdefault()
Вернемся к неудачному коду из предыдущего пункта главы.

```python
d = {}

if k not in d:
    d[k] = []

d[k].append("some value")
```

Как мы разобрались, его можно отрефакторить заменой `dict` на `collections.defaultdict`. Второй вариант — воспользоваться [методом словаря](/courses/python/chapters/python_chapter_0150#block-methods) `setdefault()`:

```python
d = {}

d.setdefault(k, []).append("some value")
```

`setdefault()` удобен, если значение по умолчанию в процессе работы со словарем может поменяться. Например, оно разное для разных ключей.

Проведите рефакторинг этого кода с помощью метода `setdefault()`. {.task_text}

```python {.task_source #python_chapter_0390_task_0030}
subscriptions = {}

if "user_1" not in subscriptions:
    subscriptions["user_1"] = set()

subscriptions["user_1"].add("monthly")

print(subscriptions)
```
Добавление в словарь `subscriptions` ключа "user_1", к которому привязано множество из одного элемента "monthly": `setdefault("user_1", set()).add("monthly")`. {.task_hint}
```python {.task_answer}
subscriptions = {}

subscriptions.setdefault("user_1", set()).add("monthly")

print(subscriptions)
```

### Циклы: обращение по ключу vs метод items()
Так выглядит типичный проход по словарю у новичка в мире питона:

```python
for k in d:
    print(k, d[k])
```

Этот код не оптимален: в нем есть лишнее обращение по ключу. Правильнее проитерироваться по парам ключ-значение, которые возвращает [метод](/courses/python/chapters/python_chapter_0150#block-basic-ops) `items()`:

```python
for k, v in d.items():
    print(k, v)
```

А точнее, `items()` возвращает представление [(view).](https://docs.python.org/3/library/stdtypes.html#dictionary-view-objects) Говоря простым язком, это «окно», скользящее по ключам и значениям словаря. Поэтому `items()` безопасно использовать даже для очень больших словарей. Причем представление является динамическим: если содержимое словаря обновится, то все изменения корректно отразятся представлением. При каждом обращении представление словаря отдает кортеж из двух элементов. 

Проведите рефакторинг цикла по словарю. {.task_text}

```python {.task_source #python_chapter_0390_task_0040}
numbers = {i: i * 2 for i in range(5)}

for num in numbers:
    if num % 2 == 0:
        print(f"{num} -> {numbers[num]}")
```
Проитерируйтесь по парам ключ-значение, которые возвращает `numbers.items()`. {.task_hint}
```python {.task_answer}
numbers = {i: i * 2 for i in range(5)}

for k, v in numbers.items():
    if k % 2 == 0:
        print(f"{k} -> {v}")
```

## Работа со списками
### Поиск по списку vs поиск по множеству
Проверим, содержит ли список `lst` элемент `x`:

```python
if x in lst:
    print(f"Found {x} in list!")
```

В такой проверке нет ничего плохого, но ровно до тех пор, пока длина списка удерживается в рамках разумного. Как мы [обсуждали](/courses/python/chapters/python_chapter_0110#block-lst-inner) в главе про списки, сложность поиска элемента в объекте типа `list` — O(n). И если количество элементов списка переваливает за тысячи, а то и миллионы, то поиск становится ужасно не эффективным.

В таком случае появляется веский повод отказаться от списка в пользу другой структуры данных, например множества `set` или словаря `dict`. Поиск в этих коллекциях [занимает](/courses/python/chapters/python_chapter_0140#block-complexity) O(1).

```python
s = set(lst)

if x in s:
    print(f"Found {x} in set!")
```


Напишите [декоратор](/courses/python/chapters/python_chapter_0270/) `measure_time()` для измерения времени выполнения функции. Он должен логировать, сколько секунд она выполнялась: `"2.03 seconds"`. {.task_text}

Декорируйте им функции `search_in_list()` и `search_in_set()`. Визуально сравните, сколько времени занимает их выполнение. {.task_text}

Пример измерения времени выполнения [приведен](/courses/python/chapters/python_chapter_0300#block-measure-time) в главе про процессы и потоки. {.task_text}

```python {.task_source #python_chapter_0390_task_0050}
def search_in_list(lst, vals):
    for val in vals:
        x = val in lst

def search_in_set(lst, vals):
    s = set(lst)
    for val in vals:
        x = val in s


large_lst = [n for n in range(25000)]
vals = [n for n in range(15000, 30000)]

search_in_list(large_lst, vals)

search_in_set(large_lst, vals)
```
Требуется декорировать методы `search_in_list()` и `search_in_set()` через `@measure_time`. {.task_hint}
```python {.task_answer}
import time

def measure_time(func):
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        func(*args, **kwargs)
        finish = time.perf_counter()
        print(f"{finish - start:.2f} seconds")

    return wrapper


@measure_time
def search_in_list(lst, vals):
    for val in vals:
        x = val in lst


@measure_time
def search_in_set(lst, vals):
    s = set(lst)
    for val in vals:
        x = val in s


large_lst = [n for n in range(25000)]
vals = [n for n in range(15000, 30000)]

search_in_list(large_lst, vals)

search_in_set(large_lst, vals)
```

### Циклы: len() + range() vs enumerate()
Этот анти-паттерн касается не только списков, но и строк, кортежей. Просто применительно к спискам он встречается особенно часто. И заключается он в организации цикла по коллекции через индексы:

```python
lst = ["a", "b", "c"]

for i in range(0,len(lst)):
    elem = lst[i]
    print(i, elem)
```

Связку `len()` + `range()` для получения индекса и обращения к элементам коллекции по этому индексу следует заменить на вызов [встроенной функции](/courses/python/chapters/python_chapter_0250#block-enumerate) `enumerate()`:

```python
lst = ["a", "b", "c"]

for i, elem in enumerate(lst):
    print(i, elem)
```

Проведите рефакторинг этого кода. {.task_text}

```python {.task_source #python_chapter_0390_task_0060}
langs = ["go", "rust", "ruby", "c++", "c"]

i = 0

while i < len(langs):
    print(i + 1, langs[i]) # Numeration must start from 1!
    i += 1
```
`enumerate()` принимает 2 аргумента: итерабельный объект и начальное значение счетчика (по умолчанию 0). А возвращает пару: индекс элемента и сам элемент, извлеченный из итерабельного объекта. {.task_hint}
```python {.task_answer}
langs = ["go", "rust", "ruby", "c++", "c"]

for i, lang in enumerate(langs, 1):
    print(i, lang)
```

### List comprehensions vs generator expressions
Многие обходят [list comprehension](/courses/python/chapters/python_chapter_0240/) стороной из-за необычного синтаксиса. Но стоит его освоить, и зачастую новички в мире питона бросаются в другую крайность: злоупотребление.

```python
comma_seperated_words = ','.join([word for word in words])
```

В этом примере кода, как и во многих других случаях, нет нужды создавать целый список и расходовать на него оперативную память. Достаточно применить [генераторное выражение:](/courses/python/chapters/python_chapter_0240#generator-expressions)

```python
comma_seperated_words = ','.join(word for word in words)
```

Большинство встроенных и библиотечных функций могут работать с генераторными выражениями: `all()`, `any()`, `enumerate()`, `iter()`, `itertools.cycle()`, `itertools.accumulate()` и т.д.

Проведите рефакторинг этого кода, чтобы вместо заведения списка `lst` через list comprehension сразу использовать генераторное выражение. {.task_text}

```python {.task_source #python_chapter_0390_task_0070}
lst = [i*i for i in range(10)]
s = sum(lst)

print(s)
```
Синтаксис генераторных выражений отличается от синтаксиса list comprehension только заменой квадратных скобок на круглые. {.task_hint}
```python {.task_answer}
lst = [i*i for i in range(10)]

s = sum(i*i for i in range(10))

print(s)
```

## Резюмируем
- Для получения из словаря значения по ключу (либо значения по умолчанию, если ключ не найден), используйте метод `get()`.
- Когда требуется задавать одно и то же значение по умолчанию для несуществующих ключей словаря, вместо класса `dict` используйте `collections.defaultdict`.
- Если же необходимо задавать различные умолчания для несуществующих ключей словаря, используйте метод `setdefault()`.
- Для итерации по словарю используйте метод `items()`.
- Во множестве или словаре поиск элементов осуществляется гораздо быстрее, чем в списках, строках и кортежах. Помните об этом, если перед вами встает задача частых поисков в большой коллекции.
- Используйте метод `enumerate()` для получения индексов элементов при итерации по коллекции.
- Везде, где вместо list comprehension подойдет генераторное выражение, отдавайте предпочтение генераторному выражению.
