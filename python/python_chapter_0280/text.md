# Глава 28. Функции высших порядков

В питоне приемы из функционального программирования гармонично сочетаются с привычным ООП. Они позволяют создавать выразительный,  легко модифицируемый и тестируемый код. Для этого в языке есть все необходимое: замыкания, ленивые вычисления, рекурсия, функции высших порядков. 

Функции высших порядков — это функции, которые возвращают или принимают в качестве аргументов другие функции.
О них и поговорим.

## Встроенные функции и модули functools, itertools
В этой главе мы рассмотрим две встроенные функции высшего порядка:
- `map()` применяет функцию поэлементно к одному или нескольким итерабельным объектам.
- `filter()` фильтрует элементы итерабельного объекта с помощью функции.

Модуль `functools` содержит набор функций высших порядков, в том числе для [мемоизации](https://wiki5.ru/wiki/Memoization) (кэширования результатов работы других функций) и [частичного применения.](https://ru.wikipedia.org/wiki/%D0%A7%D0%B0%D1%81%D1%82%D0%B8%D1%87%D0%BD%D0%BE%D0%B5_%D0%BF%D1%80%D0%B8%D0%BC%D0%B5%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5) Мы же рассмотрим функцию из этого модуля для обработки последовательностей:
- `reduce()` кумулятивно применяет функцию к элементам итерабельного объекта, чтобы в результате осталось только одно значение.

В комбинации с функциями высших порядков часто используются вспомогательные функции для итерирования:
- `zip()` — встроенная функция. Принимает несколько итерируемых объектов, из которых поэлементно создает кортежи. Возвращает итератор, который останавливается, когда исчерпывается один из итерируемых объектов.
- `zip_longest()` — функция из модуля `itertools`, богатого на разнообразные виды итераторов. Она схожа со встроенной функцией `zip()`, но продолжает обход до исчерпания всех итерируемых объектов. Когда объекты исчерпываются, вместо их элементов функция подставляет заданное значение (по умолчанию `None`).
- `chain()` — функция из модуля `itertools`. Позволяет бесшовно итерироваться по нескольким коллекциям.

Рассмотрим перечисленные функции подробнее.

## map() {#block-map}
Встроенная функция `map()` принимает функцию и один или несколько итерабельных объектов, применяет эту функцию к каждому их элементу:

```python
map(function, *iterables)
```

Количество принимаемых функцией `function` аргументов должно совпадать с количеством переданных в `map()` итерабельных объектов. `map()` возвращает итератор, при каждом обращении к  которому `function` применяется к элементам итерабельных объектов. Итератор истощается, как только будет завершена итерация по одному из объектов.

Применим `map()`, чтобы конвертировать числа в шестнадцатеричную систему счисления:

```python
numbers = [32, 51, 3]

for n in map(hex, numbers):
    print(n)
```
```
0x20
0x33
0x3
```

Обработаем через `map()` сразу три коллекции. Чтобы не заводить отдельную вспомогательную функцию, воспользуемся лямбдой:

```python
l1 = [-2, 3, 1]
l2 = [0, 5, 8]
l3 = [1, 2, 0]

for res in map(lambda a, b, c: a + b + c, l1, l2, l3):
    print(res)
```
```
-1
10
9
```

Создайте переменную `s` типа `set`. Сохраните в нее числа из списка `lst`, сконвертированные в строки с префиксом `"s"`. Примеры таких строк: `"s-1"`, `"s4"`. {.task_text}

```python {.task_source #python_chapter_0280_task_0010}
lst = [45, 9, -1, 0, 9, 1024, -1]

s = # your code here
print(s)
```
В конструктор `set()` требуется передать результат работы `map()`. {.task_hint}
```python {.task_answer}
s = set(map(lambda n: "s" + str(n), lst))
```

Некоторые разработчики предпочитают вместо list comprehension использовать связку `map()` и `list()`: `map()` возвращает генератор, `list()` принимает его на вход и формирует результирующий список.

Рассмотрим классический list comprehension:

```python
squares = [n*n for n in range(0, 5)]

print(squares)
```
```
[0, 1, 4, 9, 16]
```

Перепишем его с использованием `map()`:

```python
squares = list(map(lambda n : n*n, range(0, 5)))

print(squares)
```
```
[0, 1, 4, 9, 16]
```

Сохраните в словарь `discounted` товары из `products` с применением скидки в 3%. Используйте для этого `map()`. {.task_text}

```python {.task_source #python_chapter_0280_task_0020}
products = {"corn": 5.2, "noodle": 6.5, "mayonnaise": 1.0}
```
Пригодится вспомогательная функция `apply_discount(product, discount=3.0)`, которую `map()` применит к элементам `products.items()``. {.task_hint}
```python {.task_answer}
def apply_discount(product, discount=3.0):
    return product[0], product[1] * (1.0 - discount / 100.0)

products = {"corn": 5.2, "noodle": 6.5, "mayonnaise": 1.0}

discounted = dict(map(apply_discount, products.items()))
```

## filter() {#block-filter}
Встроенная функция `filter()` принимает функцию и итерабельный объект:

```python
filter(function, iterable)
```

`filter()` возвращает итератор из тех элементов `iterable`, для которых `function` вернула `True` или приводимое к нему значение. Пропускаются элементы, для которых `function` вернула `False` либо 0, `None`, пустую строку и т.д.

```python
passwords = ["*****", "**", "*******", "*"]

safe_passwords = list(filter(lambda p: len(p) >= 3, passwords))
print(safe_passwords)
```
```
['*****', '*******']
```

Сохраните в словарь `expensive` товары из `products`, которые дороже 4$. {.task_text}

```python {.task_source #python_chapter_0280_task_0030}
products = {"corn": 5.2, "noodle": 6.5, "mayonnaise": 1.0}
```
Пригодится вспомогательная функция `is_expensive(product)`, которую можно передать аргументом в `filter()`. {.task_hint}
```python {.task_answer}
def is_expensive(product):
    return product[1] > 4

expensive = dict(filter(is_expensive, products.items()))
```

Если вместо функции в `filter()` передать `None`, то в отфильтрованном объекте окажутся те значения, которые сами по себе приводимы к  `True`:

```python
temperatures = (5, 0, -1, 6)

print(tuple(filter(None, temperatures)))
```
```
(5, -1, 6)
```

Сохраните в множество `valid_keys` не пустые элементы из `keys`. {.task_text}

```python {.task_source #python_chapter_0280_task_0040}
keys = ['u', 'u', '', 'd', 'h', '', '', 'r']

```
Вызов `filter()`:  `filter(None, keys)`. {.task_hint}
```python {.task_answer}
keys = ['u', 'u', '', 'd', 'h', '', '', 'r']

valid_keys = set(filter(None, keys))
```

Перепишите этот list comprehension на связку вызовов функций высших порядков. {.task_text}

```python {.task_source #python_chapter_0280_task_0050}
velocities = {60, 65, 90, 100, 120, 20, 40}

res = [f"{v} km/h" for v in velocities if v > 60]

print(res)
```
Воспользуйтесь функциями `map()` и `filter()`, а также конструктором `list()`. {.task_hint}
```python {.task_answer}
velocities = {60, 65, 90, 100, 120, 20, 40}

res = list(map(lambda v : f"{v} km/h", filter(lambda v : v > 60, velocities)))

print(res)
```

## functools.reduce()
Функция `reduce()` из модуля `functools` производит цепочечные вычисления, многократно применяя `function` к каждому элементу коллекции и подставляя `initializer` в качестве первого параметра, а сам элемент в качестве второго. 

```python
functools.reduce(function, iterable, initializer=None)
```

`initializer` — это аккумулятор (стартовое значение), с которого начинаются расчеты. Он возвращается, если последовательность пуста. Если `initializer` не указан, а итерабельный объект состоит из единственного элемента, то `reduce()` возвращает этот элемент.

```python
from functools import reduce

def f(prev, cur):
    return prev * cur

res = reduce(f, [1, 2, 3, 4])
print(res)
```
```
24
```

Кстати, в данном примере вместо самописной функции для умножения мы могли бы воспользоваться функцией `mul()` из модуля `operator`. В нем также есть функции `add()` для сложения.

Напишите функцию `get_total_clicks()`. Она принимает на вход список словарей, содержащих 2 поля: `"clicks"` (количество кликов посетителя на странице), `"is_bot"` (маркер, является ли посетитель ботом). {.task_text}

Функция должна вернуть суммарное количество кликов, совершенных не ботами. Для пустого списка она должна вернуть 0. {.task_text}

Например, для списка `page_stats` функция вернет 4. {.task_text}

```python {.task_source #python_chapter_0280_task_0060}
def get_total_clicks(stats):
    # Your code here
    ...

page_stats = [
    {
        "is_bot": True,
        "clicks": 2,
    },
    {
        "is_bot": False,
        "clicks": 3,
    },
    {
        "is_bot": False,
        "clicks": 1,
    }
]

print(get_total_clicks(page_stats))
```
Воспользуйтесь связкой `reduce()` и `filter()`. {.task_hint}
```python {.task_answer}
from functools import reduce


def get_total_clicks(stats):
    return reduce(
        lambda a, b: a + b["clicks"],
        filter(lambda p: not p["is_bot"], stats),
        0,
    )

page_stats = [
    {
        "is_bot": True,
        "clicks": 2,
    },
    {
        "is_bot": False,
        "clicks": 3,
    },
    {
        "is_bot": False,
        "clicks": 1,
    }
]

print(get_total_clicks(page_stats))
```

## zip() {#block-zip}
Встроенная функция `zip()` нужна для итерирования параллельно по нескольким коллекциям. На каждой итерации она возвращает кортеж из элементов этих коллекций:

```python
zip(*iterables, strict=False)
```

Если значение аргумента `strict` равно `False` (то есть совпадает со значением по умолчанию), то по достижению самой короткой последовательности `zip()` завершает работу. Если же `strict` выставлен в `True`, а переданные в `zip()` коллекции имеют различное количество элементов, функция сгенерирует исключение `ValueError`.

```python
cities = ["Podolsk", "Tver"]
locations = [(55.43, 37.54), (56.85, 35.90)]

places = tuple(zip(cities, locations))
print(places)
```
```
(('Podolsk', (55.43, 37.54)), ('Tver', (56.85, 35.9)))
```

Добавим в список `cities` еще один город и «забудем» внести его координаты в список `locations`. Выставим при этом `strict` в `True`: 

```python
cities = ["Podolsk", "Tver", "Voronej"]
locations = [(55.43, 37.54), (56.85, 35.90)]

places = tuple(zip(cities, locations, strict=True))
```
```
(('Podolsk', (55.43, 37.54)), ('Tver', (56.85, 35.9)))
```
```
Traceback (most recent call last):
  File "example.py", line 4, in <module>
    places = tuple(zip(cities, locations, strict=True))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ValueError: zip() argument 2 is shorter than argument 1
```

Как видите, установка `strict=False` помогает удостовериться, что переданные в `zip()` коллекции имеют одинаковую длину.

Напишите вариативную функцию `match()`, которая принимает произвольное количество позиционных аргументов — итерабельных объектов.  {.task_text}

Пусть она вернет список кортежей элементов тех объектов, которые не являются пустыми. Если переданные в функцию объекты имеют разную длину, она должна сгенерировать исключение `ValueError`. {.task_text}

Например, `match([1, 2, 3], [], [9, 2, 0])` вернет `[(1, 9), (2, 2), (3, 0)]`. {.task_text}

```python {.task_source #python_chapter_0280_task_0070}
```
Воспользуйтесь связкой `zip()`, `filter()` и конструктора `list()`. {.task_hint}
```python {.task_answer}
def match(*args):
    return list(zip(*filter(len, args), strict=True))
```

Распакуйте в переменные `a2`, `b2` такое выражение над `paired`, чтобы успешно пройти проверку `assert`. {.task_text}

Небольшая подсказка: в этом выражении участвует вызов `zip()` и оператор `*`. {.task_text}

```python {.task_source #python_chapter_0280_task_0080}
a = ["A", "B", "C"]
b = ["D", "E", "F"]
paired = list(zip(a, b)) # [("A", "D"), ("B", "E"), ("C", "F")]

a2, b2 = # Your code here

assert a == list(a2) and b == list(b2)
```
Вызов `zip()`: `zip(*paired)`. {.task_hint}
```python {.task_answer}
a = ["A", "B", "C"]
b = ["D", "E", "F"]
paired = list(zip(a, b)) # [("A", "D"), ("B", "E"), ("C", "F")]

a2, b2 = zip(*paired)

assert a == list(a2) and b == list(b2)
```

## itertools.zip_longest()
Функция `zip_longest()` из модуля `itertools` составляет поэлементные кортежи из переданных в нее итерабельных объектов, пока все они не исчерпаются. Если одни объекты исчерпались раньше других, вместо их элементов подставляется значение `fillvalue`:

```python
itertools.zip_longest(*iterables, fillvalue=None)
```

Таким образом, `zip_longest()` отличается от встроенной функции `zip()` только обработкой ситуации, когда в нее переданы коллекции разной длины. 

```python
from itertools import zip_longest

short = range(3)
long = range(5)

pairs = list(zip_longest(short, long, fillvalue="X"))
print(pairs)
```
```
[(0, 0), (1, 1), (2, 2), ('X', 3), ('X', 4)]
```

# itertools.chain()
Функция `chain()` из модуля `itertools` возвращает итератор, который поочередно проходится по элементам переданных коллекций.

```python
itertools.chain(*iterables)
```

С помощью `chain()` можно бесшовно итерироваться по нескольким итерабельным объектам:

```python
from itertools import chain

for x in chain("ABC", "DEF"):
    print(x)
```
```
A
B
C
D
E
F
```

Напишите вариативную функцию `get_best_genres()`, которая принимает на вход произвольное количество списков, содержащих словари. В словарях есть поля: `title` (название жанра игры), `avg_rating` (средняя оценка). {.task_text}

Функция должна вернуть множество названий жанров в lower-case, для которых средняя оценка игр выше 3.5. {.task_text}

Используйте в решении `itertools.chain()`, `filter()`, `map()`. {.task_text}

```python {.task_source #python_chapter_0280_task_0090}
games_shop_stats_april = [{
    "title": "Tower defense",
    "avg_rating": 3.4
},
{
    "title": "Hack and Slash RPG",
    "avg_rating": 3.6
}]

games_shop_stats_june = [
{
    "title": "Hack and Slash RPG",
    "avg_rating": 3.7
},
{
    "title": "Rouelike",
    "avg_rating": 4.1
}]

def get_best_genres(*stats):
    # Your code here
    ...

print(get_best_genres(games_shop_stats_april, games_shop_stats_june))
```
Воспользуйтесь связкой `map()`, `filter()`, `chain()` и конструктором `set()`. {.task_hint}
```python {.task_answer}
from itertools import chain

games_shop_stats_april = [{
    "title": "Tower defense",
    "avg_rating": 3.4
},
{
    "title": "Hack and Slash RPG",
    "avg_rating": 3.6
}]

games_shop_stats_june = [
{
    "title": "Hack and Slash RPG",
    "avg_rating": 3.7
},
{
    "title": "Rouelike",
    "avg_rating": 4.1
}]

def get_best_genres(*stats):
    return set(
        map(
            lambda g: g["title"].lower(),
            filter(lambda g: g["avg_rating"] > 3.5, chain(*stats)),
        )
    )

print(get_best_genres(games_shop_stats_april, games_shop_stats_june))
```

## Резюмируем
- Встроенная функция `map()` применяет функцию к элементам коллекций.
- Встроенная функция `filter()` фильтрует элементы коллекции с помощью переданной в нее функции.
- Функция `reduce()` из модуля `functools` применяет функцию к элементам итерабельного объекта, сводя их к единственному значению.
- Встроенная функция `zip()` и функция `zip_longest()` из модуля `itertools` генерируют поэлементные кортежи из нескольких коллекций.
- Функция `chain()` из модуля `itertools` позволяет обрабатывать несколько последовательностей как одну, бесшовно итерируясь по элементам всех последовательностей.
