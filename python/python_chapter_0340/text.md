# Глава 34. Дескрипторы

[Дескрипторы](https://docs.python.org/3/howto/descriptor.html) позволяют реализовывать произвольную логику при обращении к атрибутам объекта для чтения, модификации и удаления. 

## Что такое дескриптор
Дескриптор — это атрибут класса, поведение при работе с которым переопределяется dunder-методами `__get__()`, `__set__()` и `__delete__()`. Эти три метода реализуют протокол дескриптора. 

Если для атрибута имплементирован хотя бы один, то атрибут становится дескриптором. Поведение при работе с ним называется связанным. **Связанное поведение** (binding behavior) означает привязку к данному объекту способа, которым он изменяется, читается или удаляется.

Дескрипторы — это именно **поля класса,** а не объекта. Рассмотрим это на примере.

```python
class MinMeasurement:
    def __get__(self, obj, objtype=None):
        return min(obj.measurements, default=0)

class Measurements:
    # Descriptor:
    min_measurement = MinMeasurement()

    def __init__(self, measurements):
        self.measurements = measurements


m1 = Measurements([2, -9, 4])
print(m1.min_measurement)

m2 = Measurements([100, 55, 50, 80])
print(m2.min_measurement)
```
```
-9
50
```

Мы завели класс `MinMeasurement`, реализующий dunder-метод `__get__()`: значит, `MinMeasurement` поддержал протокол дескриптора. Мы назначили его инстанс полем `min_measurement` класса `Measurements` (то есть сделали дескриптором). Затем создали два инстанса класса `Measurements`: `m1` и `m2`. 

При обращении к дескриптору `min_measurement` через инстансы `m1` и `m2` происходит магия: вызывается метод `__get__()`, в который передается соответствующий инстанс в качестве аргумента `obj`.

Итак, класс, который реализует протокол дескриптора, позволяет делать управляемыми атрибуты в другом классе! Сигнатуры методов, составляющих протокол дескриптора, выглядят так:
```python
__get__(self, obj, obj_type=None)

__set__(self, obj, value)

__delete__(self, obj)
```

`__get__()` возвращает произвольное значение, `__set__()` и `__delete__()` возвращают `None`. 

Все три метода принимают параметры `self` и `obj`. `self` — объект дескриптора. В примере выше это поле `min_measurement`. `obj` — инстанс класса, в который дескриптор добавлен в качестве поля. В нашем примере это `m1` и `m2`.

Параметр `obj_type` метода `__get__()` — это класс, в который добавлено поле-дескриптор. В нашем примере это класс `Measurements`.

Рассмотрим еще один пример. Дескриптор для класса, описывающего аптайм сервера.

```python
import time
from datetime import datetime, timedelta


class Uptime:
    def __init__(self):
        self._ts_start = 0

    def __get__(self, obj, obj_type=None):
        now = time.time()
        sec = timedelta(seconds=int(now - self._ts_start))
        dt = datetime(1, 1, 1) + sec

        return (
            f"{dt.day-1} days, {dt.hour} hours, {dt.minute} minutes {dt.second} seconds"
        )

    def __set__(self, obj, ts_start):
        if ts_start <= 0:
            raise ValueError("Timestamp must be > 0")

        if ts_start > time.time():
            raise ValueError("Timestamp can't be in the future")

        self._ts_start = int(ts_start)

    def __delete__(self, obj):
        del self._ts_start


class Server:
    uptime = Uptime()

    def __init__(self, name, ts_start):
        self.name = name
        self.uptime = ts_start


server = Server("Sandbox", time.time())
time.sleep(2)
print(server.uptime)
```
```
0 days, 0 hours, 0 minutes 2 seconds
```

`uptime` — дескриптор класса `Server`. В методе `__init__()`, вызываемом при создании объектов `Server`, при присваивании `uptime` аргумента `ts_start` вызывается метод `__set__()`. При записи в поле `uptime` инстанса `server` вызывается `__set__()`. В него встроена валидация: временная метка не должна быть меньше нуля или старше текущего времени.


Замените поле `name` из класса `Server` на дескриптор: {.task_text}
- Заведите класс `Name`, реализующий все три метода протокола дескриптора.
- С их помощью логируйте в консоль факт чтения и записи поля `name` объектов `Server`: выводите сообщения `"Getting name"` и `"Setting name"`.
- Запретите удаление поля `name`: генерируйте исключение типа `AttributeError`.

```python {.task_source #python_chapter_0340_task_0010}
import logging
import time

logging.basicConfig(level=logging.INFO)


class Server:
    def __init__(self, name, ts_start):
        self.name = name
        self.uptime = ts_start


logging.info("Before server instantiation...")
server = Server("Sandbox", time.time())
logging.info(server.name)
server.name = "Prod"
logging.info(server.name)

try:
    del server.name
except AttributeError as e:
    logging.exception(f"Couldn't delete readonly field: {e}")

```
В классе `Name` требуется реализовать методы: `__get__()`, `__set__()`, `__delete__()`. {.task_hint}
```python {.task_answer}
import logging
import time

logging.basicConfig(level=logging.INFO)


class Name:
    def __get__(self, obj, obj_type=None):
        name = obj._name
        logging.info("Getting name")
        return name

    def __set__(self, obj, name):
        logging.info("Setting name")
        obj._name = name

    def __delete__(self, obj):
        raise AttributeError("Can't delete attribute")


class Server:
    name = Name()

    def __init__(self, name, ts_start):
        self.name = name
        self.uptime = ts_start


logging.info("Before server instantiation...")
server = Server("Sandbox", time.time())
logging.info(server.name)
server.name = "Prod"
logging.info(server.name)

try:
    del server.name
except AttributeError as e:
    logging.exception(f"Couldn't delete readonly field: {e}")
```

## Виды дескрипторов
Дескрипторы можно разбить на два типа:
- Дескрипторы данных (data descriptors). Они реализуют хотя бы один из методов `__set__()` или `__delete__()`.
- Дескрипторы не-данных (non-data descriptors). Они реализуют только метод `__get__()`.

От типа дескриптора зависит **приоритет при разрешении имен** в инстансе класса с дескриптором:
- Если в объекте есть поле, имя которого совпадает с именем дескриптора данных, то приоритет отдается дескриптору данных. 
- Если же имя поля совпадает с именем дескриптора не-данных, то приоритет отдается полю.


Что выведет этот код?  {.task_text}

В случае не обработанного исключения напишите `error`. {.task_text}

```python
class X2:
    def __get__(self, obj, obj_type=None):
        return obj.x * 2


class Data:
    x2 = X2()

    def __init__(self, val):
        self.x2 = val


d = Data(5)
print(d.x2)
```

```consoleoutput {.task_source #python_chapter_0340_task_0020}
```
`x2` — это дескриптор не-данных, потому что он реализует только метод `__get__()`. Следовательно, при разрешении имен атрибутов объекта `d` приоритет отдается полю объекта, а не дескриптора. Умножения на 2, реализованного в методе `__get__()`, не происходит. И в консоль выводится значение обычного целочисленного поля. {.task_hint}
```python {.task_answer}
5
```


Что выведет этот код?  {.task_text}

В случае не обработанного исключения напишите `error`. {.task_text}

```python
class Length:
    def __get__(self, obj, obj_type=None):
        return len(obj.lst)

    def __set__(self, obj, value):
        obj.lst = value


class Arr:
    l = Length()

    def __init__(self, data):
        self.l = data


a = Arr([1, 2, 3])
print(a.l)
```

```consoleoutput {.task_source #python_chapter_0340_task_0030}
```
`l` — это дескриптор данных, потому что он реализует метод `__set__()`. Следовательно, при разрешении имен атрибутов объекта a приоритет отдается дескриптору, а не полю объекта. В консоль выводится результат работы `__get__()`, то есть длина списка [1, 2, 3]. {.task_hint}
```python {.task_answer}
3
```

## Использование дескрипторов
Под капотом языка дескрипторы применяются сплошь и рядом. Именно они определяют, каким образом функции трансформируются в методы. Декораторы `@classmethod`, `@staticmethod`, `@property`, `@functools.cached_property` и многие другие реализованы за счет дескрипторов. [Слоты](/courses/python/chapters/python_chapter_0350/) тоже строятся на базе дескрипторов. О них мы поговорим в следующей главе.

## Резюмируем
- Дескрипторы — это атрибуты класса, имплементирующие один из методов `__get__()`, `__set__()` и `__delete__()`. Эти методы составляют протокол дескриптора.
- Дескрипторы нужны, чтобы реализовывать специфичную логику при чтении, изменении и удалении атрибутов объекта.
- Дескрипторы делятся на два типа: дескрипторы данных и дескрипторы не-данных. От типа зависит приоритет при разрешении имен.
