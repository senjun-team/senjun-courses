# Глава 17. Полиморфизм

Полиморфизм в языках программирования — это способность выполнять одно и то же действие над объектами разных типов. Выясним, какими способами он достигается в питоне.

## Наследование и утиная типизация
В питоне [полиморфизм](https://ru.wikipedia.org/wiki/%D0%9F%D0%BE%D0%BB%D0%B8%D0%BC%D0%BE%D1%80%D1%84%D0%B8%D0%B7%D0%BC_(%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%82%D0%B8%D0%BA%D0%B0)) реализуется двумя способами:
- [Наследование.](https://ru.wikipedia.org/wiki/%D0%9D%D0%B0%D1%81%D0%BB%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)) От базового класса наследуются классы-потомки, которые реализуют у себя его методы. Получается, что сигнатура методов совпадает, и к ним может обращаться вызывающий код вне зависимости от класса объекта, с которым он работает.
- [Утиная типизация.](https://ru.wikipedia.org/wiki/%D0%A3%D1%82%D0%B8%D0%BD%D0%B0%D1%8F_%D1%82%D0%B8%D0%BF%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F) В питоне реализована [динамическая типизация:](/courses/python/chapters/python_chapter_0010#block-dynamic-typing) переменные связываются с типом не в момент объявления, а в момент присваивания значения. Благодаря этому если классы содержат одинаковые методы, вызывающий код может обрабатывать объекты этих классов единообразно.

Рассмотрим каждый из подходов.

## Наследование
При наследовании в определении класса-потомка базовый класс указывается в скобках:

```python
class Parent:
    ...

class Child(Parent):
    ...
```

Класс-потомок получает от родителя все атрибуты и способен их модифицировать. В частности, методы родителя можно переопределить либо расширить:
- При переопределении метода класс-потомок полностью заменяет код метода на свой.
- При расширении метода потомок вызывает логику из класса-родителя, но при этом добавляет в метод новую функциональность. Код метода родителя вызывается через встроенную функцию `super()`.

`super()` позволяет получить доступ к атрибутам базового класса:

```python  {.example_for_playground}
class Parent:
    def __init__(self, x):
        self.x = x
        print("Parent init")

    def f(self, val):
        print(f"Parent f({val})")
        return self.x * val

class Child(Parent):
    def __init__(self, x, y):
        super().__init__(x)
        self.y = y
        print("Child init")

    def f(self, val):
        print(f"Child f({val})")
        return self.x * self.y * val

parent = Parent(2)
res = parent.f(3)
print("Result:", res, "\n")

child = Child(2, 5)
res = child.f(3)
print("Result:", res)
```

```
Parent init
Parent f(3)
Result: 6 

Parent init
Child init
Child f(3)
Result: 30
```

Инициализатор класса-потомка мы **расширили,** через `super()` вызвав в нем `__init__()` базового класса. А метод `f()` **переопределили:** полностью заместили функциональность метода из родительского класса.

Есть базовый класс `Storage` для хранения текстовых сообщений и их поиска по id. Наследуйте от него класс `InMemoryStorage`. {.task_text}

Пусть в инициализаторе он принимает дополнительный аргумент `message_count_limit`. {.task_text}

Если при добавлении нового сообщения через `save_message()` количество хранимых сообщений может превысить `message_count_limit`, метод должен сгенерировать исключение с помощью конструкции `raise Exception("Too many messages")`. {.task_text}

Если же длина сообщения превышает `message_size_limit`, метод должен сгенерировать исключение с текстом `"Message is too long"`. Если сообщение с указанным id уже есть в хранилище, оно должно быть перезаписано. {.task_text}

Метод `get_message()` должен возвращать сообщение либо `None`, если оно не найдено. {.task_text}

Имплементируйте методы `save_message()` и `get_message()` таким образом, чтобы они **расширяли** функциональность `Storage`. Код родительского класса должен выполняться в самом начале метода класса-потомка. {.task_text}

```python {.task_source #python_chapter_0170_task_0010}
class Storage:
    def __init__(self, message_size_limit):
        self._message_size_limit = message_size_limit

    def save_message(self, message_id, message):
        print(f"Saving message {message_id}...")

    def get_message(self, message_id):
        print(f"Extracting message {message_id}...")
```
Не забудьте во всех трех методах `InMemoryStorage` вызывать методы базового класса через `super()`. {.task_hint}
```python {.task_answer}
class Storage:
    def __init__(self, message_size_limit):
        self._message_size_limit = message_size_limit

    def save_message(self, message_id, message):
        print(f"Saving message {message_id}...")

    def get_message(self, message_id):
        print(f"Extracting message {message_id}...")


class InMemoryStorage(Storage):
    def __init__(self, message_size_limit, message_count_limit):
        super().__init__(message_size_limit)
        self._message_count_limit = message_count_limit
        self._saved_messages = {}

    def save_message(self, message_id, message):
        super().save_message(message_id, message)

        if (
            len(self._saved_messages) + 1 > self._message_count_limit
            and message_id not in self._saved_messages
        ):
            raise Exception("Too many messages")
        if len(message) > self._message_size_limit:
            raise Exception("Message is too long")

        self._saved_messages[message_id] = message

    def get_message(self, message_id):
        super().get_message(message_id)
        return self._saved_messages.get(message_id)
```

Что будет, если в классе-наследнике объявить метод с таким же именем, как в родителе, но с другим набором параметров?

```python  {.example_for_playground}
class A:
    def f(self, a, b):
        print("A", a, b)

class B(A):
    def f(self, a, b, c):
        print("B", a, b, c)

b = B()
b.f(1, 2, 3)
```

```
B 1 2 3
```

Имя метода дочернего класса перекроет имя родительского метода в области видимости. Поэтому при переопределении методов не помешает лишний раз проверить соответствие параметров.

## Множественное наследование {#block-multiple-inheritance}
В языке поддерживается множественное наследование, при котором потомок получает доступ к атрибутам всех родительских классов. Родительские классы перечисляются через запятую в объявлении класса-потомка:

```python
class Child(Parent1, Parent2):
    ...
```

Каким же образом решается [проблема ромбовидного наследования?](https://ru.wikipedia.org/wiki/%D0%A0%D0%BE%D0%BC%D0%B1%D0%BE%D0%B2%D0%B8%D0%B4%D0%BD%D0%BE%D0%B5_%D0%BD%D0%B0%D1%81%D0%BB%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)

Допустим, есть базовый класс `A`. От него наследуются классы `B` и `C`. От которых наследуется `D`:

```
     A
    / \
   /   \
  /     \
 B       C
  \     /
   \   /
    \ /
     D
```

Возникает неоднозначность: если инстанс `D` вызывает метод, определенный в `A` и переопределенный в `B` и `C`, то чья реализация метода будет использована — `B` или `C`?


```python  {.example_for_playground}
class A:
    def f(self):
        print("A")

class B(A):
    def f(self):
        print("B")

class C(A):
    def f(self):
        print("C")

class D(B, C):
    ...

d = D()
d.f()
```

```
B
```

Неоднозначность вызова метода устраняется с помощью зафиксированного в языке **порядка разрешения методов** (MRO, Method Resolution Order) под названием [C3-линеаризация.](https://ru.wikipedia.org/wiki/C3-%D0%BB%D0%B8%D0%BD%D0%B5%D0%B0%D1%80%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F) 

Это довольно сложный алгоритм определения последовательности, в которой должны наследоваться методы. В общих чертах он выглядит так:
- Сначала интерпретатор ищет методы в текущем классе. В нашем случае это `D`. 
- Затем обходит классы-родители слева направо в том порядке, в котором они были перечислены в объявлении дочернего класса. В нашем случае `B` указан перед `C`.
- Если метод не найден, в этом же порядке слева направо обходятся базовые классы родителей. Затем — их родителей, и так далее.

У класса в питоне можно вызвать метод `mro()`, который возвращает список классов, от которых наследован данный. В том порядке, в котором они перебираются для разрешения методов. И вот как это выглядит для нашего класса `D`:

```python
print(D.mro())
```
```
[<class '__main__.D'>, <class '__main__.B'>, <class '__main__.C'>, <class '__main__.A'>, <class 'object'>]
```

Обратите внимание, что метод `mro()` был вызван именно от класса, а не от инстанса класса.

Как быть, если требуется вызвать метод конкретного родительского класса?

Поможет функция `super()`. В примерах выше мы ничего в нее не передавали, то есть использовали аргументы по умолчанию. 

На самом деле `super(type, type_or_obj)` возвращает объект-посредник (прокси), делегирующий вызовы по цепочке иерархии. И принимает два параметра:
- `type` — тип, с которого (не включительно) начинается поиск прокси. Например, если MRO - это `Z -> Y -> X -> object`, а значение `type = Y`, то `super()` выполнит поиск по цепочке `X -> object`.
- `type_or_obj` — тип или инстанс класса, определяющий MRO для поиска. Для инстанса будет найден прокси, для которого `isinstance(obj, type) is True`. Для типа будет найден прокси, для которого `issubclass(subtype, type) is True`.

Поэтому если в нашем примере с ромбовидным наследованием мы хотим вызвать реализацию `f()` из класса `C` вместо `B`, мы должны написать следующее:

```python
d = D()
super(B, d).f()
```

```
C
```

Это равносильно прямому обращению к методу объекта через класс с принудительным пробросом интересующего объекта вместо `self`:

```python
C.f(d)
```

```
C
```

Дана цепочка наследования `Tail -> Z -> Y -> X -> object`. Расширьте в классе `Tail` функцию `f()` таким образом, чтобы она вызывала функцию класса `X`. {.task_text}

```python {.task_source #python_chapter_0170_task_0020}
class X:
    def f(self):
        print("X")

class Y(X):
    def f(self):
        print("Y")

class Z(Y):
    def f(self):
        print("Z")

class Tail(Z):
    ...


t = Tail()
t.f()
```
В `super()` требуется передать класс `Y` и `self`. {.task_hint}
```python {.task_answer}
class X:
    def f(self):
        print("X")

class Y(X):
    def f(self):
        print("Y")

class Z(Y):
    def f(self):
        print("Z")

class Tail(Z):
    def f(self):
        super(Y, self).f()

```

## Абстрактные классы {#block-abstract-classes}
[Абстрактные классы](https://ru.wikipedia.org/wiki/%D0%90%D0%B1%D1%81%D1%82%D1%80%D0%B0%D0%BA%D1%82%D0%BD%D1%8B%D0%B9_%D0%BA%D0%BB%D0%B0%D1%81%D1%81) применяются в случаях, если для всей иерархии наследования требуется задать определенный интерфейс, то есть набор публичных атрибутов. {#block-abstract}

Для удобной работы с абстрактными классами реализован модуль `abc` (abstract base class). В частности, он содержит класс `ABC` и декоратор `@abstractmethod`. Импортируем их из модуля и используем в абстрактном классе `Cache`:

```python  {.example_for_playground}
from abc import ABC, abstractmethod

class Cache(ABC):
    @abstractmethod
    def add(self, key, value):
        pass

    @abstractmethod
    def get(self, key):
        pass

```

`Cache` наследован от `ABC` и в нем перечисляются пустые методы, которые должны определить у себя производные классы. Каждый из таких методов обернут декоратором `@abstractmethod`.

Наследуем от `Cache` специализированную имплементацию кэша — [LRU кэш.](https://ru.wikipedia.org/wiki/%D0%90%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC%D1%8B_%D0%BA%D1%8D%D1%88%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F#Least_recently_used_(%D0%92%D1%8B%D1%82%D0%B5%D1%81%D0%BD%D0%B5%D0%BD%D0%B8%D0%B5_%D0%B4%D0%B0%D0%B2%D0%BD%D0%BE_%D0%BD%D0%B5%D0%B8%D1%81%D0%BF%D0%BE%D0%BB%D1%8C%D0%B7%D1%83%D0%B5%D0%BC%D1%8B%D1%85)) Попробуем создать объект этого класса:

```python
class LRUCache(Cache):
    def __init__(self, max_items):
        self.max_items = max_items

    def add(self, key, value):
        print(f"Caching {key}...")


lru_cache = LRUCache(1000)
```

```
  File "example.py", line 21, in <module>
    lru_cache = LRUCache(1000)
                ^^^^^^^^^^^^^^
TypeError: Can't instantiate abstract class LRUCache without an implementation for abstract method 'get'
```

Что-то пошло не так: в классе `LRUCache` мы определили только один из двух методов базового класса `add()` и `get()`. Чтобы можно было создавать объекты классов, наследованных от абстрактного, в этих классах должны быть реализованы **все** его абстрактные методы.

Превратите класс `Navigation` (построения маршрутов) в абстрактный класс. Наследуйте от него 2 класса: `CarNavigation` для автомобильных маршрутов и `TransitNavigation` для маршрутов на общественном транспорте. {.task_text}

Определенные в производных классах методы должны выводить в консоль сообщение вида `Имя класса. Имя метода` Пусть они ничего не возвращают. {.task_text}

```python {.task_source #python_chapter_0170_task_0030}
class Navigation:
    def build_route(self, start, finish):
        """
        Creates route between start and finish coordinates.
        Returns route object or None in case if the route couldn't be found.
        """
        ...

    def get_maneuvers(self):
        """
        Returns list of maneuvers on the last route.
        """
        ...
```
Не забудьте импортировать модуль `abc`, наследовать `Navigation` от `ABC` и декорировать его методы через `@abstractmethod`. {.task_hint}
```python {.task_answer}
from abc import ABC, abstractmethod

class Navigation(ABC):
    @abstractmethod
    def build_route(self, start, finish):
        """
        Creates route between start and finish coordinates.
        Returns route object or None in case if the route couldn't be found.
        """
        pass
    
    @abstractmethod
    def get_maneuvers(self):
        """
        Returns list of maneuvers on the last route.
        """
        pass

class CarNavigation(Navigation):
    def build_route(self, start, finish):
        print("CarNavigation. build_route")
    
    def get_maneuvers(self):
        print("CarNavigation. get_maneuvers")

class TransitNavigation(Navigation):
    def build_route(self, start, finish):
        print("TransitNavigation. build_route")
    
    def get_maneuvers(self):
        print("TransitNavigation. get_maneuvers")
```

На практике использовать абстрактные классы в питоне не всегда удобно: например, если абстрактный класс реализован во внешнем модуле, но его требуется встроить в свою иерархию наследования или подправить интерфейс. 

Кроме того, даже core-разработчики языка [считают](https://docs.python.org/3/library/typing.html#nominal-vs-structural-subtyping) такой подход не идиоматичным. Дочерние классы приходится явно модифицировать для включения в иерархию наследования. А это — лишний код и связывание общим знанием, что все они — потомки некоего базового класса...

## Протоколы {#block-protocols}
Pythonic-way подходом к полиморфизму считается утиная типизация. Никаких перегруженных иерархий наследования, явного прописывания базовых классов. Реализация интерфейса определяется не по цепочке наследования, а просто по факту наличия набора требуемых методов. 

Но если применять утиную типизацию «как она есть» в средне-крупных проектах, то когнитивная сложность кода стремительно возрастает. Становится тяжело ориентироваться, какой класс реализует какой интерфейс, не забыта ли где-то реализация важного метода, не допущена ли ошибка... Статические анализаторы не спасут положение: у них, как и у разработчиков, просто нет для этого нужной информации.

Решение есть! В питон 3.8 были добавлены [протоколы.](https://peps.python.org/pep-0544/) Как следует из названия, они описывают некое поведение:

```python  {.example_for_playground}
from typing import Protocol

class Cache(Protocol):
    def add(self, key, value):
        pass

    def get(self, key):
        pass
```

В этом примере мы **явно** описали желаемый интерфейс для кэширования в протоколе `Cache`. А вот реализуется он **неявно,** в традициях утиной типизации. Заведем классы `LRUCache` и `MRUCache`, которые имеют все необходимые методы. Их можно использовать везде, где ожидается `Cache`:

```python
class LRUCache:
    """
    Least recently used caching policy
    """
    def add(self, key, value):
        print(f"Caching {key} to LRU...")

    def get(self, key):
        print(f"Getting from LRU cache {key}...")


class MRUCache:
    """
    Most Recently Used caching policy
    """
    def add(self, key, value):
        print(f"Caching {key} to MRU...")

    def get(self, key):
        print(f"Getting from MRU cache {key}...")


# Here we can pass objects implementing Cache protocol:
def fill_cache(cache: Cache, source):
    for k, v in source.items():
        cache.add(k, v)
```

Итак, мы вынесли общее поведение классов `LRUCache` и `MRUCache` в протокол `Cache`. И, ключевой момент, в функции `fill_cache()` воспользовались [аннотацией типов:](/courses/python/chapters/python_chapter_0330/) после имени параметра `cache` через двоеточие указали, какой у него должен быть тип. 

Аннотации типов никак не влияют на рантайм и полностью игнорируются интерпретатором. Мы познакомимся с ними [в одной из следующих глав.](/courses/python/chapters/python_chapter_0330/) А пока скажем, что они упрощают понимание кода и нужны статическим анализаторам. Например, анализатору типов [mypy.](https://mypy.readthedocs.io/en/stable/) Если тип переданного в функцию объекта не совпадает с указанным в аннотации, анализатор сообщает об ошибке. 

Что изменится, если **не наследовать** класс `Cache` от `Protocol`? 

При выполнении кода — совершенно ничего. Но при передаче аргумента типа `LRUCache` или `MRUCache` в функцию, ожидающую тип `Cache`, статические анализаторы не смогут понять, что в объекте реализован нужный протокол. `mypy` сгенерирует ошибку:

```
error: Argument 1 to "fill_cache" has incompatible type "LRUCache"; expected "Cache"  [arg-type]
```

Поэтому все преимущества протоколов раскрываются только в связке со статическими анализаторами кода.

Заведите протокол `Validator`, поддерживающий метод `is_valid(obj)`. Пусть этот протокол реализуют два класса: `TextValidator` и `ValueValidator`. {.task_text}

`TextValidator` инициализируется коллекцией символов (алфавитом). Его метод `is_valid()` должен возвращать `True` либо `False` для входной строки в зависимости от того, содержатся ли все ее символы в алфавите. {.task_text}

`ValueValidator` инициализируется двумя целыми числами: `min_val` и `max_val`. Его метод `is_valid()` должен возвращать `True` либо `False` для числа-аргумента в зависимости от того, входит ли оно в указанные границы (включая границы). {.task_text}

```python {.task_source #python_chapter_0170_task_0040}
```
Не забудьте импортировать `Protocol`. {.task_hint}
```python {.task_answer}
from typing import Protocol

class Validator(Protocol):
    def is_valid(self, obj):
        pass

class TextValidator:
    def __init__(self, alphabet):
        self._alphabet = alphabet

    def is_valid(self, obj):
        for letter in obj:
            if letter not in self._alphabet:
                return False
        return True

class ValueValidator:
    def __init__(self, min_val, max_val):
        self._min_val = min_val
        self._max_val = max_val

    def is_valid(self, obj):
        return self._min_val <= obj <= self._max_val
```

## Резюмируем
- В языке поддерживается два варианта полиморфизма: наследование и утиная типизация.
- Для доступа к атрибутам родительского класса используется встроенная функция `super()`.
- В питоне есть множественное наследование.
- Для разрешения проблемы ромбовидного наследования реализован MRO обхода иерархии классов под названием C3-линеаризация.
- Протоколы нужны для статической утиной типизации.
