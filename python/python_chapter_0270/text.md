# Глава 27. Декораторы

## Что такое декоратор
Для понимания концепции декораторов необходимо твердо держать в голове факт, что функции в питоне являются объектами первого порядка. Это означает, что функции:
- можно присваивать другим объектам, переменным и функциям,
- могут быть аргументами для других функций,
- могут быть возвращены из функций.

А еще функции могут быть вложенными, то есть определенными внутри других функций.
 
```python
def outer():
    l = [1, 2, 3]

    def inner():
        return l

    return inner
```

Функция `inner()` имеет доступ к данным внешней функции `outer()` и поэтому считается не просто вложенной функцией, а замыканием (closure). Она как будто «замыкает» внутрь себя данные внешней функции для своей работы. При этом `inner()` можно вернуть наружу и использовать как обычную функцию!

Присвоим объекту `obj` результат выполнения функции `outer()`. Так как `outer()` возвращает функцию `inner()`, то `obj` становится вызываемым объектом.

```python
obj = outer()
print(obj()) # calls inner()
```
```
[1, 2, 3]
```

Вместо присваивания результата выполнения `outer()` новому объекту `obj` мы можем с помощью оператора `=` переопределить значение функции `outer()`:

```python
outer = outer()
print(outer())
```
```
[1, 2, 3]
```

Мы только что поменяли значение переменной `outer`, теперь при вызове она будет возвращать список. При этом нет возможности вернуть ее в прежнее состояние. Именно эта идея и является главной в концепции декораторов: мы переопределяем исходную функцию, оборачивая ее в какой-либо вызываемый объект. 

Что выведет этот код?  {.task_text}

В случае не обработанного исключения напишите `error`. {.task_text}

```python  {.example_for_playground}
def outer(func):
    def wrapper(arg):
        arg += 2
        return func(arg)

    return wrapper


def increment(x):
    return x + 1

increment = outer(increment)
print(increment(4))
```

```consoleoutput {.task_source #python_chapter_0270_task_0010}
```
Мы перезаписали значение объекта `increment` замыканием `wrapper`. Перед вызовом оригинальной функции `wrapper` увеличил значение аргумента 4 на 2. То есть в функцию `increment()` был передан аргумент `x=6`. Функция увеличила его на 1. {.task_hint}
```python {.task_answer}
7
```

## Простой декоратор
Переопределим функцию `f()` функцией `wrapper()` из `log_func_start()`. Такой прием позволяет выполнять некоторые действия как непосредственно **до** вызова функции, так и **после** него.

```python  {.example_for_playground}
from datetime import datetime


def log_func_start(func):
    def wrapper():
        dt = datetime.utcnow().isoformat(sep=" ", timespec="milliseconds")
        print(f"Function starts at {dt}")
        return func()

    return wrapper


def f():
    return "f() result"


f = log_func_start(f)

print(f())
```
```
Function starts at 2023-10-24 07:24:08.400
f() result
```

Чтобы при декорировании функций каждый раз не писать строку с переприсваиванием, в синтаксис был введен специальный символ `@`:

```python
@log_func_start
def f():
    return "f() result"
```

В этом примере запись с символом `@` эквивалентна присваиванию `f = log_func_start(f)`. Да, декораторы в питоне — это не более чем синтаксический сахар для подобных присваиваний.

Напишите декоратор `to_uppercase(func)`, который переведет возвращаемую функцией `func` строку в верхний регистр. {.task_text}

Декорируйте им функцию `get_random_string()`. {.task_text}

```python {.task_source #python_chapter_0270_task_0020}
import random
import string

# Your code here


def get_random_string():
    """
    Returns string consisting of 10 random ASCII letters
    """
    length = 10
    letters = string.ascii_lowercase

    return "".join(random.choice(letters) for i in range(length))


print(get_random_string())
```
Для перевода строки в верхний регистр воспользуйтесь методом строки `upper()`. {.task_hint}
```python {.task_answer}
def to_uppercase(func):
    def wrapper():
        return func().upper()

    return wrapper

@to_uppercase
def get_random_string():
    """
    Returns string consisting of 10 random ASCII letters
    """
    length = 10
    letters = string.ascii_lowercase

    return "".join(random.choice(letters) for i in range(length))


print(get_random_string())
```

Напишите декоратор `print_exec_time()`, который выводит в консоль время выполнения функции в секундах. Например, `"2.03 seconds"`. {.task_text}

Оберните декоратором `print_exec_time()` функцию `slumber()`. {.task_text}

Пример измерения времени выполнения [приведен](/courses/python/chapters/python_chapter_0300#block-measure-time) в главе про процессы и потоки. {.task_text}

```python {.task_source #python_chapter_0270_task_0030}
import random
import time

# Your code here

def slumber():
    time.sleep(random.randint(0, 3))

slumber()
```
Пример f-строки для форматированного вывода времени исполнения: `f"{finish - start:.2f} seconds"`. {.task_hint}
```python {.task_answer}
def print_exec_time(func):
    def wrapper():
        start = time.perf_counter()
        func()
        finish = time.perf_counter()
        print(f"{finish - start:.2f} seconds")

    return wrapper


@print_exec_time
def slumber():
    time.sleep(random.randint(0, 3))

slumber()
```

## Цепочки декораторов
Иногда в коде могут встретиться конструкции вида:

```python
@decorator2
@decorator1
def f():
    ...
```

Это означает, что к функции `f()` применено 2 декоратора. А применяются они снизу вверх. Такая запись является эквивалентом следующему:

```python
f = decorator2(decorator1(f))
```

Набор оборачивающих функцию декораторов называется цепочкой декораторов. Количество декораторов в цепочке не ограничено, но злоупотреблять этим не стоит.

Напишите декораторы `parentheses()` и `brackets()`, которые возвращают строку, обернутую круглыми и квадратными скобками соответственно. Задекорируйте ими функцию `f()` так, чтобы при ее вызове вернулась строка `"[(baz)]"` {.task_text}

```python {.task_source #python_chapter_0270_task_0040}
# Your code here

def f():
    return "baz"

print(f())
```
Ближе всего к функции должен быть добавлен декоратор `@parentheses`, а над ним `@brackets`. {.task_hint}
```python {.task_answer}
def parentheses(func):
    def inner():
        return f"({func()})"
    return inner

def brackets(func):
    def inner():
        return f"[{func()}]"
    return inner

@brackets
@parentheses
def f():
    return "baz"

print(f())
```

## Параметры декорируемых функций
Мы рассмотрели простой случай: декорирование функций без параметров. А что делать, если функция принимает параметры или их количество в общем случае не ограничено? Решение на поверхности: передавать в замыкание декоратора вариабельные позиционные и именованные аргументы:

```python  {.example_for_playground}
def decorator(func):
    def wrapper(*args, **kwargs):
        print("Args decorator!")
        result = func(*args, **kwargs)
        return result

    return wrapper

@decorator
def say(name, surname):
    return f"{name} {surname}"

print(say("Senior", "Junior"))
```
```
Args decorator!
Senior Junior
```

Напишите декоратор `str_checker()`, который проверяет, что все позиционные аргументы декорируемой функции являются строками. А если это не так — бросает исключение `ValueError`. {.task_text}
```python {.task_source #python_chapter_0270_task_0050}
# Your code here

def concat(*words):
    return "~".join(words)

print(concat("A", "B", "C"))

```
Для проверки, является ли аргумент строкой, примените к нему встроенную функцию `isinstance()`. Она принимает 2 аргумента: объект и тип. {.task_hint}
```python {.task_answer}
def str_checker(func):
    def wrapper(*args):
        for arg in args:
            if not isinstance(arg, str):
                raise ValueError

        return func(*args)

    return wrapper

@str_checker
def concat(*words):
    return "~".join(words)

print(concat("A", "B", "C"))
```

## Метаданные функций
Каждая функция обладает такими метаданными как `__name__`, `__doc__` и т.д. При декорировании эта информация теряется, так как объекту функции присваивается объект замыкания из декоратора:

```python  {.example_for_playground}
def decorator(func):
    def inner(*args, **kwargs):
        """decorator doc"""

        func(*args, **kwargs)

    return inner 

@decorator
def f(*args):
    """f doc"""
    ...

print(f.__name__)
print(f.__doc__)
```
```
inner
decorator doc
```

Это может стать проблемой, потому что во время отладки кода удобно удостоверяться, какая функция действительно была вызвана. Но, к счастью, решение есть и имя ему `functools.wraps`. Это специальный декоратор для декоратора, который под капотом сохраняет метаданные декорируемой функции:

```python  {.example_for_playground}
import functools

def decorator(func):
    @functools.wraps(func)
    def inner(*args, **kwargs):
        """decorator doc"""

        return func(*args, **kwargs)

    return inner 

@decorator
def f(*args):
    """f doc"""
    ...

print(f.__name__)
print(f.__doc__)
```
```
f
f doc
```

Выглядит немного мрачно: для своего декоратора нужно использовать какой-то другой декоратор. Но благодаря `functools.wraps` на корню пресекаются проблемы с отладкой, так как известно, в какой именно функции произошла ошибка.

## Фабрики декораторов
В примере выше мы использовали декоратор с параметром. Это и называется фабрикой декораторов:

```python
@some_decorator(param1, param2)
```

Декораторы возвращают замыкание, декорирующее исходную функцию. А фабрика декораторов возвращает декоратор! Это можно лучше понять на примере:

```python  {.example_for_playground}
def factory(text):
    def decorator(func):
        def inner(*args, **kwargs):
            print(text)
            func()

        return inner

    return decorator

@factory("Fabric")
def f():
    print("f")

f()
```
```
Fabric
f
```

Функция `factory()` считается фабрикой, потому что создает новый декоратор, замыкающий в себе аргумент.

Напишите фабрику декораторов `run_in_loop(n)` для запуска функции `n` раз. {.task_text}

Декорируйте ей функцию `f()`, чтобы функция выполнилась 4 раза. Сделайте так, чтобы метаданные функции не потерялись при декорировании. {.task_text}

```python {.task_source #python_chapter_0270_task_0060}
# Your code here

def f():
    """
    Test function
    """
    print("f")

f()
```
Не забудьте обернуть замыкание внутри декоратора через `@functools.wraps`. {.task_hint}
```python {.task_answer}
import functools

def run_in_loop(n):
    def decorator(func):
        @functools.wraps(func)
        def inner(*args, **kwargs):
            for _ in range(n):
                func()

        return inner

    return decorator

@run_in_loop(4)
def f():
    """
    Test function
    """
    print("f")

f()
```

## Классы-декораторы
Декорировать можно не только функции, но и методы. Более того. В качестве самого декоратора может выступать не только функция, но и класс.

```python  {.example_for_playground}
class SquareResult:
    def __init__(self, func):
        # момент декорирования
        self.func = func

    def __call__(self, *args, **kwargs):
        # момент вызова
        result = self.func(*args, **kwargs)
        return result**2

@SquareResult
def multiply(a, b):
    return a * b

print(multiply(2, 3))
```
```
36
```

Теперь функция `multiply()` является инстансом класса с типом
```
<class '__main__.SquareResult'>
```

Класс-декоратор является альтернативой для функции-декоратора. Однако он чуть более многословен, из-за чего используется не так часто и в основном для [проектирования фреймворков](https://habr.com/ru/articles/750312/). Большее распространение получили функции-декораторы. Например, они широко используются в популярных фреймворках, таких как [Flask](https://flask.palletsprojects.com/en/3.0.x/) и [FastAPI.](https://fastapi.tiangolo.com/)

## Готовые декораторы
В питоне есть встроенные декораторы, а также декораторы, реализованные в модулях стандартной библиотеки. Вот некоторые из них:
- `@classmethod` и `@staticmethod` делают метод методом класса или статическим методом. Мы [рассматривали их](/courses/python/chapters/python_chapter_0160#block-classmethod) в главе про классы и объекты.
- `@abstractmethod` помечает метод как абстрактный. Мы [разбирали его](/courses/python/chapters/python_chapter_0170#block-abstract) в главе про полиморфизм.
- `@atexit.register` исполняет переданную в декоратор функцию при завершении скрипта. Например, если вызван `sys.exit()`.
- `@typing.final` подсказывает статическому анализатору, что метод является финальным, то есть не должен быть переопределен в классах-наследниках.
- `@property` делает так, чтобы работать с методом класса как с полем, а не как с вызываемым объектом.
- `@functools.lru_cache` кэширует результаты выполнения функции.

## Резюмируем
- Декораторы помогают применять к вызываемому объекту дополнительные операции как до его вызова, так и после.
- Использование `@` — всего лишь синтаксический сахар для присвоения исходной функции результата декорирования.
- К функции можно применять цепочку декораторов. Порядок их вызова идет снизу вверх от имени функции.
- Чтобы сохранить метаданные декорируемой функции, используйте `functools.wraps`.
- Фабрика декораторов — это функция, которая принимает аргументы и возвращает декоратор.
- В стандартной библиотеке питона содержится множество декораторов, решающих типовые задачи: кэширование результатов функции, объявление метода статическим и многое другое.
