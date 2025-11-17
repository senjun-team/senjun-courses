# Глава 37. Метаклассы
> Метаклассы — потаенная магия питона, о которой 99% разработчиков не должны даже задумываться.  
Тим Питерс, core-контрибьютор питона и автор сортировки Timsort

Как сказал Тим Питерс, если вы задумываетесь, нужно ли вам использовать метаклассы, то нет, не нужно. И это правило железно работает... За исключением того, что о метаклассах нет-нет да спрашивают на собеседованиях. А в дебрях сложных проектов, с которыми приходится сталкиваться, проскальзывает таинственное слово `metaclass`.

Метакласс — мощный инструмент метапрограммирования. Метакласс — это фабрика классов. С ее помощью классы создаются и кастомизируются прямо в рантайме. Метаклассами пронизана подкапотная работа с классами. Если хотите знать больше, то эта глава для вас.


## Что такое метакласс
Как мы [выяснили](/courses/python/chapters/python_chapter_0180/) в главе про модель данных, все в питоне — это объект. Класс — это объект с типом `type`.

```python  {.example_for_playground}
class Dummy:
    ...

d = Dummy()
print(type(d))
print(type(Dummy))
print(type(type))
```
```
<class '__main__.Dummy'>
<class 'type'>
<class 'type'>
```

Из примера видно, что тип объекта `d` — его класс `Dummy`. Тип класса `Dummy` — `type`. А тип `type` — это `type`. То есть `type` сконструирован так, чтобы являться классом для себя самого. Мы получили зацикленную цепочку из классов.

Вообще `type` относится к классу также, как класс — к своему объекту. Класс нужен, чтобы создавать объекты, а `type` — классы. А все потому, что `type` является метаклассом.

**Метакласс** — сущность, которая создает классы. И это не уникальная для питона концепция. Метаклассы поддерживаются в Ruby, Objective-C, Perl и других языках.

`type` — это **встроенный метакласс.** Он используется по умолчанию для создания классов. Чтобы понять, как это работает, вспомним, как выглядит работа с классами в питоне.

По месту объявления класса интерпретатор заводит переменную с именем класса. С которой можно работать, как и с любой другой переменной. Но эта переменная (класс) сама может создавать переменные (инстансы). Поэтому она называется классом. 

Класс можно присваивать переменной:

```python  {.example_for_playground}
class C:
    ...

x = C
print(type(x))
print(id(x))
print(id(C))
```
```
<class 'type'>
94038844921552
94038844921552
```

Можно добавлять и удалять атрибуты класса.

Что выведет этот код?  {.task_text}

```python  {.example_for_playground}
class C:
    ...

C.field = 8

print("field" in C.__dict__)
```

```consoleoutput {.task_source #python_chapter_0370_task_0010}
```
Мы завели пустой класс `C`. Затем добавили в него поле `field` со значением 8. В консоль мы вывели результат проверки, содержится ли поле `field` в словаре с атрибутами. {.task_hint}
```python {.task_answer}
True
```

Класс можно передавать в функцию и возвращать из функции:

```python  {.example_for_playground}
def factory(title):
    if title == "pancake":
        class Pancake:
            ...

        return Pancake

    if title == "brownie":
        class Brownie:
            ...

        return Brownie

    raise ValueError("Unexpected title")

x = factory("brownie")
print(type(x))
print(x.__name__)
```
```
<class 'type'>
Brownie
```

В этом примере мы создаем различные классы прямо внутри функции `factory()` и возвращаем их наружу. Выглядит это странновато. Такой код не очень гибкий, в нем легко запутаться.

Добиться того же самого результата, то есть динамического создания классов, можно с помощью встроенной функции `type()`.

## Встроенный метакласс type и динамическое создание классов
В прошлых главах мы неоднократно пользовались функцией `type()`. Она принимает объект и возвращает его тип.

```python
obj_type = type(obj)
```

В таком варианте использования `type()` фактически возвращает значение dunder-поля `__class__`. Например:

```python  {.example_for_playground}
val = 104
print(type(val))
print(val.__class__)
```
```
<class 'int'>
<class 'int'>
```

У `type()` есть и другое применение — динамическое создание классов! В таком случае функция принимает 3 аргумента: имя класса, кортеж его родителей и словарь с атрибутами класса. А возвращает сам класс:

```python
class_obj = type(class_name, parents, attrs)
```

`class_name` превращается в dunder-поле `__name__` нового класса, список родительских классов `parents` становится полем `__bases__`, а словарь `attrs` — не что иное, как `__dict__`.

Пусть вас не смущает, что в зависимости от количества передаваемых аргументов `type()` ведет себя совершенно по-разному: для единственного аргумента возвращает его тип, а для трех аргументов создает объект-класс. Так сложилось по историческим причинам.

Заменим классическое объявление пустого класса на его создание через `type()`.

При заведении класса через `class` объект класса создается автоматически.

```python
class Dummy:
    ...
```

А при заведении класса через `type()` ответственность за создание объекта лежит на разработчике:

```python
Dummy = type("Dummy", (), {})
```

Обратите внимание: здесь имя класса, переданное строкой в `type()`, совпадает с именем переменной, в которую присваивается созданные класс. В принципе они могут и различаться, но зачем усложнять!

Что выведет этот код?  {.task_text}

```python
Dummy = type("Dummy", (), {})
print(len(Dummy.__bases__))
```

```consoleoutput {.task_source #python_chapter_0370_task_0020}
```
Если у класса явно не указан родитель, класс наследуется от `object`. Поэтому кортеж `__bases__` состоит из единственного элемента: `<class 'object'>`. {.task_hint}
```python {.task_answer}
1
```

Атрибутами класса могут быть, разумеется, и поля, и методы. И вот как свободная функция превращается в метод:

```python  {.example_for_playground}
def show_summary(self):
    print(f"object type: {type(self)}")
    print(f"class parents: {type(self).__bases__}")


SimpleClass = type("SimpleClass", (), {"summary": show_summary})
obj = SimpleClass()
obj.summary()
```
```
object type: <class '__main__.SimpleClass'>
class parents: (<class 'object'>,)
```

В этом примере мы завели функцию `show_summary()`. С помощью вызова `type()` создали класс `SimpleClass`, в атрибут `summary` которого превращена функция. Затем мы инстанцировали `obj` от класса `SimpleClass` и вызвали у него данный метод. 

Проведите рефакторинг кода: замените объявления `Parent` и `Child` на создание классов через `type()`. {.task_text}

```python {.task_source #python_chapter_0370_task_0030}
class Parent:
    def __init__(self):
        self.x = 5

    def f(self, val):
        print(f"Parent f({val})")
        return self.x * val

class Child(Parent):
    def __init__(self):
        self.y = 6

    def f(self, val):
        print(f"Child f({val})")
        return self.x * self.y * val


parent = Parent()
res = parent.f(3)
print(f"Result: {res}\n")

child = Child()
res = child.f(3)
print(f"Result: {res}\n")
```
Создание класса `Parent` после заведения функции `f_parent()`: `Parent = type("Parent", (), {"x": 5, "f": f_parent})`. {.task_hint}
```python {.task_answer}
def f_parent(self, val):
    print(f"Parent f({val})")
    return self.x * val

def f_child(self, val):
    print(f"Child f({val})")
    return self.x * self.y * val

Parent = type("Parent", (), {"x": 5, "f": f_parent})

Child = type("Child", (Parent,), {"y": 6, "f": f_child})

parent = Parent()
res = parent.f(3)
print(f"Result: {res}\n")

child = Child()
res = child.f(3)
print(f"Result: {res}\n")
```

Каждый раз, когда в коде мы пишем `class`, срабатывает подкапотная магия питона. Она превращает блок `class` в вызов `type()`. Тело класса исполняется в свежесозданном пространстве имен; имя класса связывается с результатом вызова `type()`.

Метакласс `type` по умолчанию используется при создании классов. Но можно написать и свой собственный метакласс.

## Кастомные метаклассы
Рассмотрим основные шаги, исполняемые при инстанцировании объекта класса:

```python  {.example_for_playground}
class Dummy:
    ...

d = Dummy()
```

Для того чтобы создать инстанс класса, у класса нужно вызвать оператор `()`: он вернет новый объект. Вызов `()` определяется с помощью dunder-метода `__call__()`. Который в свою очередь вызывает конструктор `__new__()` и инициализатор `__init__()`. Если эти методы не объявлены в самом классе, интерпретатор ищет их в родительских классах. 

Метод `__new__()` всегда вызывается перед `__init__()`. Он создает объект и возвращает его. А `__init__()` — инициализирует, донастривает переданный в него уже существующий объект. В случае, если создаваемый объект — класс, то для его настройки правильнее использовать `__new__()`.

Если поведение при создании объекта класса вдруг захотелось переопределить, этого можно добиться, присвоив классу новый метод:

```python  {.example_for_playground}
class Dummy:
    ...

def new(cls):
    obj = object.__new__(cls)
    print(f"Creating object {obj}...")
    return obj

Dummy.__new__ = new

d = Dummy()
```
```
Creating object <__main__.Dummy object at 0x7f3fbbf8bad0>...
```

В данном примере мы переписали поведение класса `Dummy` при создании своих инстансов. Но как быть, если мы хотим переопределить поведение при создании **классов?** Как мы уже знаем, `type` — метакласс, по умолчанию создающий классы в питоне. Но вот так просто переопределить его методы  `__new__()` и `__init__()` не получится. Любая попытка перезаписи атрибутов `type` завершится исключением:

```
TypeError: can't set attributes of built-in/extension type 'type'
```

Для переопределения действий по созданию класса и нужны кастомные метаклассы.

Для начала определим новый метакласс. Чтобы класс превратился в метакласс, он должен быть наследован от `type` или его потомка. 

Затем в определении класса, поведение при создании которого хочется переопределить, укажем кастомный метакласс после ключевого слова `metaclass`.

И вот как это выглядит на примере:

```python  {.example_for_playground}
class Meta(type):
    def __new__(cls, name, bases, attrs):
        obj = super().__new__(cls, name, bases, attrs)
        print(f"Creating class {obj}...")
        return obj

class Dummy(metaclass=Meta):
    ...
```
```
Creating class <class '__main__.Dummy'>...
```

Метод `__new__()` метакласса принимает класс, его имя, кортеж родителей и словарь атрибутов.

Реализуйте метакласс `UpperAttrMeta`, который бы переводил все атрибуты класса, не являющиеся dunder-атрибутами, в верхний регистр. {.task_text}

Примените этот метакласс к `SimpleClass`. {.task_text}

```python {.task_source #python_chapter_0370_task_0040}
# Your code here

class SimpleClass:
    attr1 = "val1"
    attr2 = "val2"

print(hasattr(SimpleClass, "attr1"))
print(hasattr(SimpleClass, "ATTR1"))
```
Метод `__new__()` класса `UpperAttrMeta` принимает аргументы: `cls`, `name`, `bases`, `attrs`. Внутри метода требуется пройтись по ключам и значениям словаря `attrs` и добавить в новый словарь их в модифицированном виде. Затем вызвать `type.__new__()`, передав в него этот новый словарь. {.task_hint}
```python {.task_answer}
class UpperAttrMeta(type):
    def __new__(cls, name, bases, attrs):
        uppercase_attrs = {
            name if name.startswith("__") else name.upper(): value
            for name, value in attrs.items()
        }

        return type.__new__(cls, name, bases, uppercase_attrs)


class SimpleClass(metaclass=UpperAttrMeta):
    attr1 = "val1"
    attr2 = "val2"


print(hasattr(SimpleClass, "attr1"))
print(hasattr(SimpleClass, "ATTR1"))
```

Метаклассы выполняют довольно простую работу: перехватывают создание класса; модифицируют класс; возвращают уже модифицированный. Вот собственно и все, что нужно знать о метаклассах.

## Пару советов о использовании метаклассов
- Избегайте изменения сигнатур методов через метаклассы, т.к. это ломает IDE и type hints
- Тестируйте метаклассы отдельно - ошибки в них сложно отлаживать
- Документируйте метаклассы подробно, так же как и с декораторами - `магия` внутри них не всегда очевидна

## Применение метаклассов: Система плагинов

Вот компактный, но реальный пример использования метаклассов для создания системы автоматической регистрации плагинов. Этот паттерн было бы `не удобно` реализовать без метаклассов, ручная регистрация каждого плагина привела бы к дублированию кода и ошибкам, когда разработчик забывает зарегистрировать новый плагин.

```python
class PluginRegistry(type):
    plugins = {}

    def __new__(cls, name, bases, attrs):
        new_cls = super().__new__(cls, name, bases, attrs)
        if name != "BasePlugin":
            cls.plugins[name] = new_cls
        return new_cls


class BasePlugin(metaclass=PluginRegistry):
    """Базовый класс для всех плагинов"""
    def execute(self):
        raise NotImplementedError()


class EmailPlugin(BasePlugin):
    def execute(self):
        print("Отправка email уведомления")


class SMSService(BasePlugin):
    def execute(self):
        print("Отправка SMS сообщения")


def run_plugin(plugin_name):
    plugin_class = PluginRegistry.plugins.get(plugin_name)
    if not plugin_class:
        raise ValueError(f"Плагин '{plugin_name}' не найден")
    return plugin_class().execute()

if __name__ == "__main__":
    print("Доступные плагины:", list(PluginRegistry.plugins.keys()))
    print(run_plugin("EmailPlugin"))
    print(run_plugin("SMSService"))
```
```
Доступные плагины: ['EmailPlugin', 'SMSService']
Отправка email уведомления
Отправка SMS сообщения
```

### Почему именно метаклассы?

- **Автоматизация:** Плагины регистрируются при определении класса, без дополнительных вызовов
- **Надежность:** Разработчик не может "забыть" зарегистрировать плагин
- **Инкапсуляция:** Логика регистрации скрыта от пользователя API
- **Альтернативы работает не на столко хорошо:** Декораторы @register можно забыть добавить, ручная регистрация — дублирование кода


## Метаклассы в популярных фреймворках

И примеры, и задачи в этой главе выглядят притянутыми за уши. Чтобы добиться аналогичного результата, не стоит прибегать к тяжелой артиллерии в виде метаклассов. Сгодятся и декораторы классов. Метаклассы существуют отнюдь не для тривиального изменения поведения классов. Они оказываются действительно незаменимы для решения более сложных задач. Например, когда речь заходит о разработке API, таких как Django ORM.

### Django
Django — это веб-фреймворк. Django ORM (Object Relational Mapping) — это API, который позволяет взаимодействовать с базой данных, используя код на питоне вместо SQL-запросов. Он описывает записи в SQL-таблицах обычными классами:

```python
class Course(models.Model):
    title = models.CharField(max_length=200, unique=True)
    chapters_count = models.IntegerField()

course = Course(title="python", chapters_count=37)
print(course.chapters_count)
```

В данном случае мы обратились к полю `chapters_count`, которое в бд имеет тип `IntegerField`. Но в коде мы работаем с ним как с обычным `int`, хотя значение `chapters_count` извлекается из таблицы. Это возможно благодаря определенному для `models.Model` метаклассу, который позволяет в стиле питона работать с сущностями из бд и избегать сложных запросов.

### Pydantic Models
Pydantic — библиотека для валидации данных и сериализации, которая автоматически проверяет типы и значения данных при создании объектов на основе аннотаций типов в классах. Получить все зарегистрированные модели можно через `__subclasses__()`:

```python
from pydantic import BaseModel, field_validator

class UserPydantic(BaseModel): # BaseModel - метакласс
    name: str
    age: int

class CursePydantic(BaseModel):
    id: int
    title: str

    @field_validator("title", mode="before")
    def validate_title(cls, value: str):
        return value.upper()

all_models = BaseModel.__subclasses__()
print("Все Pydantic модели:", [cls.__name__ for cls in all_models])
user = UserPydantic(name="John", age=18)
curse = CursePydantic(id=1, title="python")
print(user)
print(curse)
```
```
Все Pydantic модели: ['UserPydantic', 'CursePydantic']
John
id=1 title='PYTHON'
```

В Pydantic v2 метакласс автоматически собирает поля из аннотаций типов и генерирует валидаторы для типов, так же вручную можно добавить более сложную логику валидирования.

Реализацию можно найти в файлах пайдентика:
- Реализация [метакласса](https://github.com/pydantic/pydantic/blob/f42171c760d43b9522fde513ae6e209790f7fefb/pydantic/_internal/_model_construction.py#L82)
- Реализация [базового класса](https://github.com/pydantic/pydantic/blob/f42171c760d43b9522fde513ae6e209790f7fefb/pydantic/v1/main.py#L316)

### SQLAlchemy Declarative
SQLAlchemy — мощная библиотека Python для работы с реляционными базами данных (ORM), позволяющая описывать таблицы в виде классов Python и выполнять операции с базой данных с помощью этих объектов.
Она использует метакласс для автоматической регистрации моделей и создания маппинга с таблицами базы данных:

```python
from sqlalchemy import Column, Integer, String
from sqlalchemy.orm import declarative_base

Base = declarative_base() # Конструктор метакласса

# Этот класс нужен для человеко-читаемого представления моделей
class BaseModel(Base):
    __abstract__ = True
    def __repr__(self):
        attrs = []
        for column in self.__table__.columns:
            value = getattr(self, column.name)
            attrs.append(f"{column.name}={value!r}")
        return f"<{self.__class__.__name__}({', '.join(attrs)})>"

class UserSQLAlchemy(BaseModel):
    __tablename__ = 'UserSQLAlchemy'
    id = Column(Integer, primary_key=True)
    name = Column(String(50))


class CurseSQLAlchemy(BaseModel):
    __tablename__ = 'CurseSQLAlchemy'
    id = Column(Integer, primary_key=True)
    title = Column(String(100))

print("Все SQLAlchemy таблицы:", list(Base.metadata.tables.keys()))
user = UserSQLAlchemy(id=1, name="John")
curse = CurseSQLAlchemy(id=1, title="Python")
print(user)
print(curse)
```
```
Все SQLAlchemy таблицы: ['UserSQLAlchemy', 'CurseSQLAlchemy']
<UserSQLAlchemy(id=1, name='John')>
<CurseSQLAlchemy(id=1, title='Python')>
```

### Реализацию для SQLAlchemy можно посмотреть здесь:
- Реализация [метакласса](https://github.com/sqlalchemy/sqlalchemy/blob/8383e3f48c900fa248f026218fed0cea5ad0e6a5/lib/sqlalchemy/orm/decl_api.py#L170)
- Реализация [конструктора базового класса](https://github.com/sqlalchemy/sqlalchemy/blob/8383e3f48c900fa248f026218fed0cea5ad0e6a5/lib/sqlalchemy/orm/decl_api.py#L1016)

## Когда избегать метаклассов

### Альтернативы метаклассам:
- **Декораторы классов**: для модификации существующих классов
- **Наследование и миксины**: для повторного использования логики
- **Фабричные функции**: для динамического создания классов
- **__init_subclass__** (Python 3.6+): для кастомизации при наследовании

### Красные флаги для метаклассов:
- Если задача решается простым наследованием
- Если вы хотите просто добавить методы к классу
- Если вы новичок в Python и еще не освоили дескрипторы
- Если код должен быть легко читаемым командой


## Современная альтернатива: `__init_subclass__`

Начиная с [Python 3.6](https://peps.python.org/pep-0487/) появился более легковесный и читаемый способ кастомизации наследования - метод `__init_subclass__`. Он позволяет переопределить поведение при создании (но только после создания) подклассов без необходимости писать полноценные метаклассы

### Как это работает
```python
class Base:
    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        print(f"Creating subclass {cls.__name__} from {cls.__base__.__name__}")
        # Можно модифицировать класс здесь
        if not hasattr(cls, "default_value"):
            cls.default_value = 0

class Child(Base):
    pass

class AnotherChild(Base):
    default_value = 42

print(Child.default_value)
print(AnotherChild.default_value)
```
```
Creating subclass Child from Base
Creating subclass AnotherChild from Base
0
42
```

## Сравнение с метаклассами
__init_subclass__ — это не замена метаклассам, а упрощенный инструмент для конкретных задач. На самом деле, __init_subclass__ работает внутри механизма метаклассов, а не вместо них.
Когда вы определяете __init_subclass__ в базовом классе, интерпретатор Python автоматически:

- Обнаруживает наличие метода __init_subclass__ при создании подкласса
- Автоматически оборачивает его в classmethod (даже без декоратора @classmethod)
- Вызывает его в правильный момент — после создания класса, но до его финальной инициализации

```python
class Base:
    def __init_subclass__(cls, **kwargs):
        """Этот метод автоматически становится classmethod!"""
        print(f"Initializing subclass {cls.__name__}")
        super().__init_subclass__(**kwargs)  # Важно вызывать super(), хотя у нас нет явного родительского класса

class Child(Base):
    pass
```
```
Initializing subclass Child
```

### Почему это не замена метаклассам?
Под капотом __init_subclass__ использует стандартный механизм метаклассов. По сути, это синтаксический сахар для частого паттерна использования метаклассов. Вот что происходит при создании класса:

```python
# Когда вы пишете:
class Child(Base):
    pass

# Интерпретатор выполняет примерно такой код:
Child = type("Child", (Base,), {})
# Затем автоматически вызывает:
for base in Child.__mro__:
    if hasattr(base, "__init_subclass__"):
        base.__init_subclass__.__func__(Child)  # Автоматический вызов
```

### Практическая разница
Метаклассы дают вам полный контроль над процессом создания класса:

```python
class Meta(type):
    def __new__(cls, name, bases, attrs):
        # Полный контроль до создания класса
        attrs["added_by_meta"] = "magic"
        return super().__new__(cls, name, bases, attrs)

class MyClass(metaclass=Meta):
    pass

print(MyClass.added_by_meta)  # "magic"
```
__init_subclass__ дает вам ограниченный контроль после создания класса:
```python
class Base:
    def __init_subclass__(cls, **kwargs):
        # Контроль после создания класса
        if not hasattr(cls, "default_value"):
            cls.default_value = 0

class MyClass(Base):
    pass

print(MyClass.default_value)  # 0
```

## Когда использовать __init_subclass__ вместо метаклассов:

### Используйте __init_subclass__, когда:
- Нужно просто добавить/проверить атрибуты у подклассов
- Требуется базовая валидация при наследовании
- Важна читаемость кода для команды
- Нужен доступ к классу после его создания, но до того, как класс станет полностью готовым к использованию

### Используйте метаклассы, когда:
- Нужно полностью контролировать процесс создания класса
- Требуется изменить наследование или пространство имен
- Нужно перехватить создание самого класса (не только подклассов)
- Необходима сложная логика регистрации/настройки классов

### Пример: регистрация плагинов через __init_subclass__
```python
class BasePlugin:
    plugins = {}
    
    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        if cls.__name__ != "BasePlugin":
            BasePlugin.plugins[cls.__name__] = cls
    
    def execute(self):
        raise NotImplementedError()

class EmailPlugin(BasePlugin):
    def execute(self):
        return "Email sent"

class SMSService(BasePlugin):
    def execute(self):
        return "SMS sent"

print("Registered plugins:", list(BasePlugin.plugins.keys()))
```
```
Registered plugins: ['EmailPlugin', 'SMSService']
```

Код стал проще и понятнее, а логика регистрации инкапсулирована в базовом классе.

## Проверка наследования: `issubclass()` и метаклассы

Встроенная функция `issubclass()` позволяет проверить, является ли один класс подклассом другого. Это особенно полезно при работе с метаклассами и динамическим созданием классов

### Базовое использование
```python
class Animal:
    pass

class Dog(Animal):
    pass

class Cat(Animal):
    pass

print(issubclass(Dog, Animal))
print(issubclass(Cat, Animal))
print(issubclass(Animal, Dog))
print(issubclass(int, object))
```
```
True
True
False
True # (т.к.все наследуется от object)
```

### Практические рекомендации:
- **Для регистрации плагинов:** `__init_subclass__` предпочтительнее метаклассов (проще и читаемее)
- **Для валидации:** Декораторы классов или `__init_subclass__`
- **Для проверки наследования:** Всегда используйте `issubclass()` и `__subclasses__()`
- **Для сложных API (как Django/SQLAlchemy ORM):** Метаклассы по-прежнему незаменимы

## Аналоги MetaClass/issubclass()

| Задача | Метакласс | Декоратор класса | `__init_subclass__` | `issubclass`/`__subclasses__` |
|--------|-----------|------------------|---------------------|-------------------------------|
| Регистрация классов | Отлично | Хорошо | Отлично | Для получения списка |
| Валидация атрибутов | Отлично | Можно | Хорошо | Не подходит |
| Изменение наследования | Единственный вариант | Нельзя | Нельзя | Не подходит |
| Модификация пространства имен | Полный контроль | После создания | После создания | Не подходит |
| Проверка иерархии классов | Косвенно | Нельзя | Нельзя | Идеально |


## Резюмируем
- Класс — это объект, который создает другие объекты. А метакласс — это объект, который создает классы.
- Метакласс — это фабрика для создания классов в рантайме.
- `type` — это встроенный метакласс, порождающий в питоне все классы.
- От `type` можно наследоваться и таким образом определить кастомный метакласс.
- Для указания, какой метакласс использовать при создании класса, есть ключевое слово `metaclass`.
- Для простой реализации API достаточно использовать __init_subclass__.
- Чаще всего метаклассы используются для реализации более сложных API, таких как Django/SQLAlchemy ORM.
