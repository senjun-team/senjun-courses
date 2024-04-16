# Глава 33. Аннотации типов

Аннотации типов — это возможность указывать типы при объявлении переменных, полей класса, параметров и возвращаемых значений функций... Типизация в питоне динамическая: интерпретатор получает информацию о типах не во время компиляции, а лишь во время выполнения. Так что аннотации типов — не более чем **подсказки** для разработчиков, IDE и статических анализаторов кода, таких как [mypy.](https://mypy-lang.org/) 

Несмотря на это аннотации типов все чаще становятся неотъемлемой частью код-стайла в крупных проектах. Ведь они повышают читабельность кода и помогают уберечься от досадных ошибок.

## Как работают аннотации типов
Аннотации не дают никакой гарантии, что в переменную будет записано значение указанного типа. Зато во многих случаях это обнаружит статический анализатор и сгенерирует предупреждение. Не удивительно, что во многих проектах анализ кода встроен в [CI](https://en.wikipedia.org/wiki/Continuous_integration) и запускается автоматически.

Поэтому хоть аннотации типов являются встроенным функционалом питона и для их использования не требуется никаких сторонних библиотек и утилит, рассматривать мы их будем в связке с mypy. Только обязательное использование статического анализатора позволит в полной мере раскрыть пользу от внедрения в проект статической типизации.

Связывание переменной с типом строится на простых правилах:
- Типы переменных и параметров указываются после двоеточия `:`. Например, запись `res = calc()` превращается в `res: float = calc()`.
- Типы возвращаемых значений указываются после стрелочки `->`, например `-> str`. Если функция ничего не возвращает, это прописывается явно с помощью `-> None`.
- В качестве типа объекта можно указать его базовый класс. Тогда переменной можно присвоить его наследников, но использовать для них только функционал, определенный в базовом классе. Например, нельзя вызвать метод, отсутствующий в базовом классе.

Сохраним в скрипт `example.py` простой пример использования аннотаций типов:

```python
def format(val: float) -> str:
    return f"{val=:.2f}"

print(format(2.009))
```

Проверим файл `example.py` через mypy:

```
$ mypy example.py 
Success: no issues found in 1 source file
```

А теперь вместо ожидаемого значения типа `float` передадим в функцию строку:

```python
def format(val: float) -> str:
    return f"{val=:.2f}"

print(format("value"))
```

При несовпадении типов mypy сгенерирует ошибку:

```
example.py:4: error: Argument 1 to "format" has incompatible type "str"; expected "float"  [arg-type]
Found 1 error in 1 file (checked 1 source file)
```

Как видите, mypy запускается из консоли. Его можно установить через менеджер пакетов `pip`:

```shell
python3 -m pip install mypy
```

Если при вызове mypy задать флаг `--strict`, то будут включены все возможные опциональные проверки. Они гарантируют, что если в процессе выполнения кода возможно какое-то несовпадение типов, mypy о них сообщит. При этом будьте готовы к ложным срабатываниям.

Даже если тип переменной не указан, с помощью анализа [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) во многих случаях mypy  способен его вывести самостоятельно. Так, в этом тривиальном примере аннотация считается избыточной:

```python
x: int = 1
l: list[int] = [1, 2]
```

Но если мы заводим **пустую** коллекцию, то без явного указания типа `mypy` не сможет определить, какие элементы в нее попадут:

```python
d: dict[str, float] = {}
s: set[str] = set()
```

## Аннотации для встроенных типов
Аннотация для встроенного типа — это просто имя типа:

```python
x: bool = True
val: int = 2
a: float = 5.01
s: str = "ABC"
```

В аннотации коллекций после типа коллекции в скобках указывается тип элементов:

```python
l: list[str] = ["A", "B"]
s: set[int] = {105, -9}
d: dict[int, bool] = {}
```

Добавьте аннотации типов вместо их описания в комментариях. Исправьте код, который нарушает аннотации. {.task_text}

```python {.task_source #python_chapter_0330_task_0010 .run_static_type_checker}
class Storage():
    def __init__(self, message_size_limit):
        # message_size_limit must be int
        self._message_size_limit = message_size_limit

        # mapping of message id to message
        self._storage = {}

    def save_message(self, message_id, message):
        # message_id must be int
        self._storage[message_id] = message

    def get_message(self, message_id):
        return self._storage[message_id]


s = Storage("9")
s.save_message("8", "message")
s.get_message(8)
```
Не забудьте указывать типы для возвращаемых методами значений. {.task_hint}
```python {.task_answer}
class Storage():
    def __init__(self, message_size_limit: int) -> None:
        self._message_size_limit = message_size_limit

        # mapping of message id to message
        self._storage: dict[int, str] = {}

    def save_message(self, message_id: int, message: str) -> None:
        self._storage[message_id] = message

    def get_message(self, message_id: int) -> str:
        return self._storage[message_id]


s = Storage(9)
s.save_message(8, "message")
s.get_message(8)
```

В аннотации кортежа перечисляются типы всех его элементов:

```python
def f(t: tuple[int, float, str]) -> None:
    ...
```

Однако в некоторых случаях длина кортежа неизвестна. Например, если кортеж подается на вход функции. Тогда вместо поэлементного перечисления типов используется многоточие `...`. В данном примере функция принимает кортеж произвольной длины, заполненный целыми числами:

```python
def f(t: tuple[int, ...]) -> None:
    ...
```

Добавьте аннотации типов в функцию. {.task_text}

```python {.task_source #python_chapter_0330_task_0020 .run_static_type_checker}
def word_count(lines):
      result = {}
      for line in lines:
          for word in line.split():
              result[word] = result.get(word, 0) + 1
      return result
```
Не забудьте указать тип для переменной `result`. {.task_hint}
```python {.task_answer}
def word_count(lines: list[str]) -> dict[str, int]:
      result: dict[str, int] = {}
      for line in lines:
          for word in line.split():
              result[word] = result.get(word, 0) + 1
      return result
```

Как быть, если одной переменной могут быть присвоены значения разных типов? Например, если список содержит целые числа и строки. Тогда возможные типы перечисляются через символ `|`:

```python
l: list[int | str]
```

Частный случай — переменная, которая может быть `None`:

```python
x: list[int | None]
```

## Модуль typing
Модуль `typing` содержит множество подсказок о типах, среди которых:
- `Optional`: тип переменной, которая может принимать значение `None`. Например, `Optional[int]`. Может использоваться вместо синтаксиса `T | None`.
- `Any`: произвольный тип.
- `Literal`: перечисление допустимых значений для переменной. Например, `Literal["retry", "abort"]`.
- `Protocol`: [протокол,](/courses/python/chapters/python_chapter_0170#block-protocols) то есть класс, описывающий некоторый интерфейс в традициях утиной типизации.
- `NoReturn`: способ указания результата функции, если функция никогда не возвращает управление.

`Optional` используется для переменных, которые могут становиться `None`:

```python
from typing import Optional

x: Optional[str] = "val" if some_check() else None
```

Для обозначения объектов произвольного типа предназначено определение `Any`:

```python
from typing import Any

obj: Any = some_magic()
```

Конечно, вместо `Any` можно было бы указать тип `object`, потому что он базовый вообще для всех объектов. Но, во-первых, `Any` более явно выражает намерение подчеркнуть, что тип объекта неизвестен или не важен. Во-вторых, если переменной задать тип `object`, то и работать с ней можно только как с экземпляром `object`. Иначе статические анализаторы выдадут ошибку типов.

Определение `Literal` нужно для проверки соответствия значения переменной одному из фиксированных литералов.

```python
from typing import Literal

ENDPOINTS = Literal["/search", "/suggest"]

def get_endpoint_rps(endpoint: ENDPOINTS) -> dict[ENDPOINTS, int]:
    return {endpoint: 3000}

print(get_endpoint_rps("/search"))
```

Разумеется, в качестве литералов можно перечислять не только строки:

```python
def validate_simple(data: Any) -> Literal[True]:
    return True
```

Использование `Protocol` мы [подробно рассматривали](/courses/python/chapters/python_chapter_0170#block-protocols) в главе про полиморфизм, поэтому здесь останавливаться на нем не будем.

Определение `NoReturn` указывается для возвращаемого значения, если функция никогда не возвращает управление. Например, она в вечном цикле обрабатывает соединения либо вызывает `sys.exit()`.

```python
from typing import NoReturn

def f() -> NoReturn:
    while True:
        ...
```

Добавьте аннотации типов в код. {.task_text}

```python {.task_source #python_chapter_0330_task_0030 .run_static_type_checker}
import sys

def get_tuple(arg = None):
    if arg is None:
        return 1, 2
    if arg == 0:
        return 2, 3
    return 3, 2

def get_false():
    # Always returns false
    return False

def shutdown():
    sys.exit(0)

def format(a, b, c):
    # 'a' is int
    # 'b' is bool
    # we don't know 'c' type

    return f"{a=} {b=} {c=}"
```
Вам пригодятся аннотации `Any`, `Literal`, `NoReturn`, `Optional` из модуля `typing`. {.task_hint}
```python {.task_answer}
import sys
from typing import Any, Literal, NoReturn, Optional

def get_tuple(arg: Optional[int] = None) -> tuple[int, int]:
    if arg is None:
        return 1, 2
    if arg == 0:
        return 2, 3
    return 3, 2

def get_false() -> Literal[False]:
    # Always returns false
    return False

def shutdown() -> NoReturn:
    sys.exit(0)

def format(a: int, b: bool, c: Any) -> str:
    # 'a' is int
    # 'b' is bool
    # we don't know 'c' type

    return f"{a=} {b=} {c=}"
```

## Модуль collections.abc
В модуле `collections.abc` содержатся [определения](https://docs.python.org/3/library/collections.abc.html#collections-abstract-base-classes) для обобщенной (generic) аннотации типов.

Наиболее распространенные из них:
- `Callable`: функция или другой вызываемый объект, у которого определен dunder-метод `__call__()`.
- `Mapping`: объект, хранящий пары ключ-значение, у которого есть метод `__getitem__()`.
- `MutableMapping`: изменяемый объект для хранения пар ключ-значение. У него должен быть определен метод `__setitem__()`.
- `Sequence`: последовательность элементов с доступом по индексу. Должна поддерживать методы `__len__()` и `__getitem__()`.
- `Iterable`: [итерабельный объект,](/courses/python/chapters/python_chapter_0230/) то есть любой объект, по которому можно пройтись циклом `for`.
- `Iterator`: [итератор,](/courses/python/chapters/python_chapter_0230/) то есть объект, поддерживающий протокол итератора (методы `__iter__()` и `__next__()`).

Пример аннотации для функции, которая принимает функцию и итерабельный объект и вызывает для них [встроенную функцию](/courses/python/chapters/python_chapter_0280#block-filter) `filter()`:

```python
from collections.abc import Callable, Iterable, Iterator

def filter_vals(check_data: Callable[[int], bool], data: Iterable[int]) -> Iterator[int]:
    return filter(check_data, data)

print(list(filter_vals(lambda x : x > 0, [-1, 3, -2, 8, 9])))
```

Здесь для функции мы использовали аннотацию `Callable[[int], bool]`, то есть указали, что функция принимает единственный параметр типа `int` и возвращает тип `bool`. Если сигнатура функции не важна, можно писать просто `Callable`.

Модуль `collections.abc` позволяет типизировать обобщенный код. Например, вы написали функцию, которая принимает последовательность и считает по ее элементам какую-то статистику. Функция корректно отработает и для строки, и для списка, и для кортежа. Поэтому для параметра функции подойдет тип `Sequence`, а для результирующей статистики например тип `Mapping`.

Добавьте аннотации типов в код. {.task_text}

```python {.task_source #python_chapter_0330_task_0040 .run_static_type_checker}
def format(d):
    return (f"{k}-{v}" for k, v in d.items())

def modify(d):
    for k, v in d.items():
        if v is not None and v < 0:
            print("Modifying key:", k)
            d[k] = None
```
Вам пригодятся аннотации `Mapping`, `MutableMapping` и `Iterable` из модуля `collections.abc` и `Any` из `typing`. {.task_hint}
```python {.task_answer}
from collections.abc import Mapping, MutableMapping, Iterable
from typing import Any

def format(d: Mapping[Any, Any]) -> Iterable[str]:
    return (f"{k}-{v}" for k, v in d.items())

def modify(d: MutableMapping[int, int | None]) -> None:
    for k, v in d.items():
        if v is not None and v < 0:
            print("Modifying key:", k)
            d[k] = None
```


## Игнорирование типов в mypy
Комментарий с текстом `# type: ignore` используется, чтобы подавить ошибки mypy для конкретных строк. Хорошим тоном считается после него оставить комментарий, поясняющий, почему в данном месте следует опустить проверку типов.

```python
x = some_magic()  # type: ignore  # some_magic() won't return None here because ...
```

## Резюмируем
- Аннотации типов нужны для повышения читабельности кода, для подсказок от IDE и проверки статическими анализаторами. Они никак не влияют на рантайм. Интерпретатор их пропускает. 
- Аннотация для переменной, поля или параметра функции указывается через двоеточие: `x : int`. Аннотация для возвращаемого значения — после стрелочки: `def f() -> None`.
- Модуль `typing` содержит подсказки о типах, например `Any`, `Optional`, `NoReturn`.
- Модуль `collections.abc` содержит подсказки для типов коллекций.