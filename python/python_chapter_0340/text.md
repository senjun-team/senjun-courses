# Глава 34. Свойства

Свойства (properties) в питоне — это инструмент для создания управляемых атрибутов (managed attributes). То есть таких, при доступе к которым может быть выполнен дополнительный код: валидация, форматирование, дополнительные проверки...

Можно переопределить поведение атрибута при чтении, записи и удалении. Для этого создаются специальные методы, называемые геттерами (getters), сеттерами (setters) и делитерами (deleters). Их общее название — **свойства.**

## Геттеры, сеттеры, делитеры
Для того, чтобы превратить метод в **геттер,** метод оборачивается декоратором `@property`. После этого пользователь класса работает с методом как с обычным полем. 

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

При работе с объектом `u` мы обратились к `data` не как к вызываемому объекту `data()`, а как к полю. Можно сказать, что `data` — это динамически вычисляемое поле.

Итак, с помощью декоратора `@property` задается поведение при чтении поля. Метод, обернутый этим декоратором, называется геттером.

Для создания **сеттера,** то есть для переопределения поведения при записи поля, используется декоратор `@field_name.setter`. Здесь `field_name` должен совпадать с названием метода, задекорированного `@property`. Например, если в классе объявлен геттер `data`, сеттер для него должен быть задекорирован через `@data.setter`.

Для заведения **делитера,** то есть для переопределения поведения при удалении поля, используется декоратор `@field_name.deleter`. Например, `@data.deleter`. Конечно, контроль над процессом удаления поля в реальной жизни требуется довольно редко. Поэтому в большинстве случаев делитеры не используются. Они нужны, если требуется запретить удаление поля. Тогда в делитере генерируется соответствующее исключение. Или если при удалении поля требуется провести некую очистку связанных ресурсов.

Напишем класс `Coordinate` для хранения координат точки. В сеттерах `@lat.setter` и `@lon.setter` организуем проверку, что в качестве 
широты (latitude) и долготы (longitude) переданы адекватные значения.

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

	@property
	def lon(self):
		return self._lon

	@lon.setter
	def lon(self, val):
		if val > 180 or val < -180:
			raise ValueError("Invalid longitude")
		self._lon = val

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

```python {.task_source #python_chapter_0340_task_0010}
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
```{.task_hint}
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

```python {.task_source #python_chapter_0340_task_0020}
from math import pi

class Circle:
	# Your code here

c = Circle(5, 2.01, 6)
print(c.area)
print(c.circumference)
```
```{.task_hint}
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
        return pi * self.r * self. r

    @property
    def circumference(self):
        return 2 * pi * self.r
    

c = Circle(5, 2.01, 6)
print(c.area)
print(c.circumference)
```

## Свойства и наследование
В классе-потомке можно переопределять свойства класса-родителя. Но если переопределено одно свойство, необходимо переопределить и остальные, которые планируется использовать в классе-потомке. Иначе при попытке их использования будет брошено исключение `AttributeError`.

Если запустить этот код, будет сгенерировано исключение `"AttributeError: property 'field' of 'Child' object has no setter"`. Исправьте это. {.task_text}

```python {.task_source #python_chapter_0340_task_0030}
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
```{.task_hint}
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
Декораторы — наиболее лаконичный и удобный способ добавления в класс свойств. Есть и альтернатива: функция `property()`. Она принимает на вход функции, которыми требуется обернуть управляемый атрибут, а также строку документации.

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
		return pi * self.r * self. r

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
Как мы [обсуждали](/courses/python/chapters/python_chapter_0250/) в главе про декораторы, декоратор — всего лишь синтаксический сахар для оборачивания функции в замыкание. Это справедливо и для декораторов свойств. Под капотом они превращаются в вызов функции `property()`. Например, эти два блока кода эквивалентны:

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

Если заглядывать еще глубже в реализацию, то свойства строятся на [дескрипторах.](/courses/python/chapters/python_chapter_0320/) Ведь управляемый атрибут, возвращаемый `property()`, является дескриптором.

## Резюмируем
- Управляемый атрибут — это атрибут, для чтения, изменения или удаления которого реализована дополнительная логика.
- Свойства — механизм, позволяющий создавать управляемые атрибуты: геттеры, сеттеры и делитеры.
- Свойства можно создавать с помощью декораторов `@property`, `@field_name.setter` и `@field_name.deleter`. Либо с помощью функции `property()`.
- При переопределении в классе-потомке хотя бы одного свойства родителя необходимо переопределить и остальные, которые планируется использовать в потомке.
