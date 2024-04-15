# Глава 36. Свойства

Свойства (properties) в питоне — это инструмент для создания управляемых атрибутов (managed attributes). При доступе к управляемым атрибутам может быть выполнен дополнительный код: валидация, форматирование, логирование, разнообразные проверки. Разберем, для чего это нужно и как это организовать.

## Мотивация
Представим, что есть класс `Product`, описывающий товар: артикул (поле `number`), общее количество единиц товара и количество единиц, зарезервированных для покупки. У класса есть и различные методы, но их мы опустим.

```python
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.number = number
        self.count_total = count_total
        self.count_reserved = count_reserved
```

Этот класс используется по всему коду программы учета товаров на складе. Однажды появляется необходимость ввести проверку артикула: он должен состоять только из букв латинского алфавита и цифр.

Самое простое решение: сделать поле `number` приватным (переименовать его в `_number`). Затем добавить метод `set_number()`, который будет вызываться во всех местах, где требуется установить артикул. И `get_number()`, чтобы этот артикул читать:

```python
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.set_number(number)
        self.count_total = count_total
        self.count_reserved = count_reserved

    def set_number(self, number):
        if not number.isalnum():
            raise ValueError("Invalid number format")
        self._number = number

    def get_number(self):
        return self._number
```

Минусы такого подхода:
- Требуется менять интерфейс класса. Если класс входит в состав библиотеки, используемой многочисленными клиентами, это не всегда удобно и даже возможно.
- Придется вносить изменения во все места, где используется интересующее поле. Таких мест могут быть сотни.
- На каждое поле класса добавлять по два метода `set_field()`, `get_field()` — не идиоматично. Как мы помним, код на питоне должен быть лаконичным и простым, без лишних сущностей.

Вместо этого хочется добавить проверку для поля `number`, не меняя при этом ни интерфейс класса, ни использующий его код. Чтобы по-прежнему можно было писать `item.number = "78XZ6"`, но чтобы в момент присваивания запускалась нужная нам валидация и в случае неудачи генерировалось исключение. Свойства позволяют это сделать!

Рассмотрим другой сценарий использования свойств. В классе `Product` есть поле `count_total` (общее количество единиц товара на складе). И поле `count_reserved` (количество зарезервированных для покупки единиц). Требуется вычислить количество единиц товара, доступных для продажи. Для этого можно завести специальный метод:

```python
def count_available(self):
    return self.count_total - self.count_reserved
```

Но тогда мы получим не согласованный интерфейс класса: какие-то значения доступны как поля, а какие-то — как методы.

Свойства позволяют устранить эту проблему. Вместо синтаксиса **вызова метода** `item.count_available()` с помощью свойств можно организовать динамически вычисляемый атрибут с синтаксисом **доступа к полю:** `item.count_available`.

Рассмотрим, как же работают свойства и как с их помощью решать подобные задачи.

## Геттеры, сеттеры, делитеры
Итак, в питоне можно переопределить поведение любого атрибута при чтении, записи и удалении. Для этого создаются специальные методы, называемые геттерами (getters), сеттерами (setters) и делитерами (deleters). Их общее название — **свойства.**

### Геттеры
Для того чтобы превратить метод в геттер, метод оборачивается декоратором `@property`. После этого пользователь класса работает с методом как с обычным полем.

Заведем класс `User`, а в нем — геттер `data`.

```python
class User:
    def __init__(self, user_id, name, email):
        self._id = user_id
        self._name = name
        self._email = email

    @property
    def data(self):
        return f"User {self._id}: name={self._name}, email={self._email}"

u = User(147, "kotiko", "kotiko@gmail.com")
print(u.data)
```
```
User 147: name=kotiko, email=kotiko@gmail.com
```

При работе с объектом `u` мы обратились к `data` не как к вызываемому методу `data()`, а как к полю.

С помощью декоратора `@property` задается поведение при чтении поля. Метод, обернутый этим декоратором, называется геттером.

Добавьте в класс `Product` геттер `count_available`. Он должен возвращать количество единиц товара, доступных для продажи. {.task_text}

Создайте инстанс `scanner` с инвентарным номером `"scanner_3492"`, общим количеством единиц 19 и количеством зарезервированных единиц 3. {.task_text}

Выведите в консоль значение `count_available` для `scanner`. {.task_text}

```python {.task_source #python_chapter_0360_task_0040}
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.number = number # Инвентарный номер товара
        self.count_total = count_total # Общее количество единиц товара на складе
        self.count_reserved = count_reserved # Количество зарезервированных единиц

```
Нужно добавить в класс метод `count_available()`, который возвращает разность полей `count_total` и `count_reserved`. Этот метод следует обернуть декоратором `@property`, чтобы он превратился в геттер и к нему можно было обращаться как к полю. {.task_hint}
```python {.task_answer}
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.number = number
        self.count_total = count_total
        self.count_reserved = count_reserved

    @property
    def count_available(self):
        return self.count_total - self.count_reserved

scanner = Product("scanner_3492", 19, 3)
print(scanner.count_available)
```

### Сеттеры
Для создания сеттера, то есть для переопределения поведения при записи поля, используется декоратор `@field_name.setter`. Здесь `field_name` должен совпадать с названием метода, обернутого `@property`. 

Например, если в классе объявлен геттер `query`, сеттер для него должен быть задекорирован через `@query.setter`:

```python
class Query:
    def __init__(self, q):
        self.query = q

    @property
    def query(self):
        return self._q.lower()

    @query.setter
    def query(self, new_val):
        self._q = new_val.strip()

q = Query("   погода Саратов сегодня  ")
print(q.query)
```
```
погода саратов сегодня
```

Обратите внимание, что в инициализаторе класса `Query` мы обратились именно к управляемому атрибуту `query`, а не к приватному полю `_q`. То есть неявным образом вызвали сеттер. Если бы мы этого не сделали, в поле `_q` попала бы строка без удаленных методом `strip()` пробелов.

Что случится, если забыть добавить сеттер для свойства, у которого есть геттер, и попытаться присвоить полю значение?

```python
class Query:
    def __init__(self, q):
        self.query = q

    @property
    def query(self):
        return self._q.lower()

q = Query("Погода Тюмень")
```
```
Traceback (most recent call last):
  File "example.py", line 9, in <module>
    q = Query("Погода Тюмень")
        ^^^^^^^^^^^^^^^^^^^^^^
  File "example.py", line 3, in __init__
    self.query = q
    ^^^^^^^^^^
AttributeError: property 'query' of 'Query' object has no setter
```

Сообщение об ошибке свидетельствует о том, что внутри метода `__init__()` была осуществлена попытка записи `query`, у которого отсутствует сеттер.

Вывод: чтобы иметь возможность присваивать свойству значение, для этого свойства должен существовать сеттер.

В классе `Product` вместо поля `number` заведите геттер и сеттер с именем `number`. Чтобы не было конфликта имен, само поле сделайте приватным (переименуйте в `_number`).  {.task_text}

Геттер `number` должен выводить в консоль строку `"Getting value"` и затем возвращать значение поля. {.task_text}

Сеттер `number` должен выводить в консоль `"Setting value VAL"` (здесь вместо VAL должно быть выведено реальное присваиваемое значение) и затем проверять, что присваиваемый номер состоит только из букв латинского алфавита и цифр. Если это не так, сеттер должен бросать исключение `ValueError`. {.task_text}

Добейтесь того, чтобы сеттер срабатывал даже в методе-инициализаторе. {.task_text}

Проанализируйте консольный вывод: удостоверьтесь, что сеттер объекта `headphones` вызвался дважды. {.task_text}

```python {.task_source #python_chapter_0360_task_0050}
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.number = number
        self.count_total = count_total
        self.count_reserved = count_reserved

headphones = Product("headphones", 5, 0)
x = headphones.number
headphones.number = "headphones001"
```
Переименуйте поле `number` в `_number`. Создайте метод `number(self)`, декорированный `@property`. Он будет нашим геттером. Создайте метод `number(self, val)`, декорированный `@number.setter`. Он будет нашим сеттером. Внутри него требуется логировать `val`, а затем проверять с помощью метода строки `isalnum()`. В инициализаторе `__init__()` нужно оставить присваивание `self.number`: тогда в этом месте будет вызван сеттер вместо обращения к полю `_number` напрямую. {.task_hint}
```python {.task_answer}
class Product:
    def __init__(self, number, count_total, count_reserved):
        self.number = number
        self.count_total = count_total
        self.count_reserved = count_reserved

    @property
    def number(self):
        print("Getting value")
        return self._number

    @number.setter
    def number(self, val):
        print(f"Setting value {val}")
        if not val.isalnum():
            raise ValueError("Invalid number format")
        self._number = val

headphones = Product("headphones", 5, 0)
x = headphones.number
headphones.number = "headphones001"
```

### Делитеры
Для заведения делитера, то есть для переопределения поведения при удалении поля, используется декоратор `@field_name.deleter`. Например, `@query.deleter`. Конечно, контроль над процессом удаления поля в реальной жизни требуется довольно редко. Поэтому в большинстве случаев делитеры не используются. Они нужны, если требуется запретить удаление поля. Тогда в делитере генерируется соответствующее исключение. Или если при удалении поля требуется провести некую очистку связанных ресурсов.

Напишем класс `Coordinate` для хранения координат точки. В сеттерах `@lat.setter` и `@lon.setter` организуем проверку, что в качестве 
широты (latitude) и долготы (longitude) переданы адекватные значения. С помощью делитеров вместо удаления полей будем их сбрасывать в нулевую координату.

```python
class Coordinate:
    def __init__(self, lat, lon):
        self.lat = lat
        self.lon = lon

    @property
    def lat(self):
        return self._lat

    @lat.setter
    def lat(self, val):
        if val > 90 or val < -90:
            raise ValueError("Invalid latitude")
        self._lat = val

    @lat.deleter
    def lat(self):
        self._lat = 0.0

    @property
    def lon(self):
        return self._lon

    @lon.setter
    def lon(self, val):
        if val > 180 or val < -180:
            raise ValueError("Invalid longitude")
        self._lon = val

    @lon.deleter
    def lon(self):
        self._lon = 0.0

pos = Coordinate(34.0, -1004.5)
```
```
Traceback (most recent call last):
  File "example.py", line 26, in <module>
    pos = Coordinate(34.0, -1004.5)
          ^^^^^^^^^^^^^^^^^^^^^^^^^
  File "example.py", line 4, in __init__
    self.lon = lon
    ^^^^^^^^
  File "example.py", line 23, in lon
    raise ValueError("Invalid longitude")
ValueError: Invalid longitude
```

При выполнении кода произошла ошибка: проверка на некорректное значение долготы внутри `@lon.setter` сработала даже при присваивании полю внутри инициализатора `__init__()`.

Имплементируйте класс `Color`, который принимает в инициализаторе 3 значения: `r`, `g`, `b`. {.task_text}

Добавьте в класс свойство `hex`, которое бы возвращало шестнадцатеричное представление цвета. Можете воспользоваться вспомогательной функцией `rgb_to_hex()`. {.task_text}

Запретите удалять свойство `hex`: при попытке удаления должно генерироваться исключение `AttributeError` с текстом `"Hex attribute can not be deleted"`. {.task_text}

```python {.task_source #python_chapter_0360_task_0010}
def rgb_to_hex(r, g, b):
    return f"#{r:02x}{g:02x}{b:02x}"

class Color:
    # Your code

c = Color(169, 3, 252)
print(c.hex)

try:
    del c.hex
except AttributeError as e:
    print(f"Deleting hex: {e}")
```
Метод `hex(self)` требуется декорировать через `@hex.deleter` и в теле метода кидать исключение. {.task_hint}
```python {.task_answer}
def rgb_to_hex(r, g, b):
    return f"#{r:02x}{g:02x}{b:02x}"

class Color:
    def __init__(self, r, g, b):
        self._r, self._g, self._b = r, g, b

    @property
    def hex(self):
        return rgb_to_hex(self._r, self._g, self._b)
    
    @hex.deleter
    def hex(self):
        raise AttributeError("Hex attribute can not be deleted")

c = Color(169, 3, 252)
print(c.hex)

try:
    del c.hex
except AttributeError as e:
    print(f"Deleting hex: {e}")
```

## Основные сценарии работы со свойствами
Мы разобрались, что `@property`, `@field_name.setter` и `@field_name.deleter` позволяют реализовать произвольную логику при обращении к полю: валидацию, логирование, форматирование и т.д. Перечислим наиболее популярные сценарии использования свойств.

Реализация **read-only полей.** В классе заводится поле, имя которого начинается с подчеркивания `_`: это подсказка, что поле приватное и пользователю класса напрямую к нему обращаться не следует. Для чтения этого поля создается метод, декорируемый `@property`. Он и превращается в read-only поле. Также возможна реализация write-only полей. Тогда метод, декорируемый `@property`, бросает исключение `AttributeError`. Оно сообщает, что поле доступно только для записи.

Рефакторинг поведения класса **без изменения его интерфейса.** Например, если в классе есть публичное поле, и внезапно появилась необходимость организовать проверку при записи поля, не меняя интерфейс класса. В таком случае поле делается приватным. А вместо него вводится свойство со всеми необходимыми проверками.

**Ленивое вычисление** атрибутов. В декорируемом через `@property` методе осуществляются вычисления, результат которых сохраняется в приватное поле. При обращении к полю происходит проверка: нужно ли запускать вычисления или можно отдать запомненный ранее результат.

Реализация динамически **вычисляемых полей.** То есть полей, значения которых вычисляются заново при каждом обращении.

Реализуйте класс `Circle`, в инициализатор которого передаются координаты `x`,`y` и радиус. {.task_text}

Напишите свойство `r` для радиуса с проверкой, он не может быть отрицательным. В таком случае генерируйте `ValueError`. {.task_text}

Добавьте в класс свойства `area` и `circumference` для расчета площади круга и длины окружности. {.task_text}

```python {.task_source #python_chapter_0360_task_0020}
from math import pi

class Circle:
    # Your code here

c = Circle(5, 2.01, 6)
print(c.area)
print(c.circumference)
```
`area()` и `circumference()` — методы, декорированные `@property`. {.task_hint}
```python {.task_answer}
from math import pi

class Circle:
    def __init__(self, x, y, r):
        self.r = r
        self.x, self.y = x, y

    @property
    def r(self):
        return self._r

    @r.setter
    def r(self, value):
        if value < 0:
            raise ValueError("Negative radius")
        self._r = value
    
    @property
    def area(self):
        return pi * self.r * self.r

    @property
    def circumference(self):
        return 2 * pi * self.r
    

c = Circle(5, 2.01, 6)
print(c.area)
print(c.circumference)
```

## Свойства и наследование
В классе-потомке разрешается переопределять свойства класса-родителя. Действует правило: если в потомке для свойства не переопределен ни геттер, ни сеттер, ни делитер, то используется их определение из класса-родителя. Если же в потомке переопределен хотя бы один метод из трех, то для использования остальных их также требуется переопределить. Иначе при попытке их использования будет брошено исключение `AttributeError`.

Если запустить этот код, будет сгенерировано исключение `"AttributeError: property 'field' of 'Child' object has no setter"`. Исправьте это. {.task_text}

```python {.task_source #python_chapter_0360_task_0030}
class Parent:
    @property
    def field(self):
        print("Parent field")

    @field.setter
    def field(self, val):
        print("Parent setter")


class Child(Parent):
    @property
    def field(self):
        print("Child field")

c = Child()
c.field
c.field = 1
```
В классе `Child` требуется реализовать сеттер `field`, декорированный `@field.setter`. {.task_hint}
```python {.task_answer}
class Parent:
    @property
    def field(self):
        print("Parent field")

    @field.setter
    def field(self, val):
        print("Parent setter")


class Child(Parent):
    @property
    def field(self):
        print("Child field")

    @field.setter
    def field(self, val):
        print("Child setter")

c = Child()
c.field
c.field = 1
```

## Использование свойств без декораторов
Декораторы — наиболее лаконичный и удобный способ добавления в класс свойств. Есть и альтернатива: функция `property()`. Она принимает на вход функции, которыми требуется обернуть управляемый атрибут, а также docstring (строку документации).

```python
property(fget=None, fset=None, fdel=None, doc=None)
```

Функция `property()` возвращает сам управляемый атрибут.

Перепишем класс `Circle` из задачи выше на использование `property()`.

```python
from math import pi

class Circle:
    def __init__(self, x, y, r):
        self.r = r
        self.x, self.y = x, y

    def _get_r(self):
        return self._r

    def _set_r(self, value):
        if value < 0:
            raise ValueError("Negative radius")
        self._r = value
    
    def _get_area(self):
        return pi * self.r * self.r

    def _get_circumference(self):
        return 2 * pi * self.r
    
    r = property(
        fget=_get_r,
        fset=_set_r,
        doc="Circle radius"
    )

    area = property(fget=_get_area)

    circumference = property(fget=_get_circumference)
    

c = Circle(5, 2.01, 6)
print(c.area)
print(c.circumference)
```

Относительно кода с декораторами код с функцией `property()` выглядит более многословным. 

## Как устроены свойства
Как мы [обсуждали](/courses/python/chapters/python_chapter_0270/) в главе про декораторы, декоратор — всего лишь синтаксический сахар для оборачивания функции в замыкание. Это справедливо и для декораторов свойств. Под капотом они превращаются в вызов функции `property()`. Например, эти два блока кода эквивалентны:

```python
@property
def r(self):
    return self._r
```

```python
def r(self):
    return self._r

r = property(r)
```

Если заглядывать еще глубже в реализацию, то свойства строятся на [дескрипторах.](/courses/python/chapters/python_chapter_0340/) Ведь управляемый атрибут, возвращаемый `property()`, является дескриптором.

## Резюмируем
- Управляемый атрибут — это атрибут, для чтения, изменения или удаления которого реализована дополнительная логика.
- Свойства — механизм, позволяющий создавать управляемые атрибуты: геттеры, сеттеры и делитеры.
- Свойства можно создавать с помощью декораторов `@property`, `@field_name.setter` и `@field_name.deleter`. Либо с помощью функции `property()`.
- При наследовании действует правило: если в классе-потомке не переопределен ни один из методов свойства родителя, то подхватывается реализация свойства из родителя. Если же хотя бы один из методов (геттер, сеттер, делитер) переопределен, то для использования в классе-потомке других методов свойства их тоже требуется переопределить.
