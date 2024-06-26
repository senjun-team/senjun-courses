# Глава 16. Классы и объекты

> Объектно-ориентированный заменитель спагетти-кода — это лазанья-код (слишком много слоев абстракции).  
Роберто Вальтман

Рассмотрим основные возможности питона для работы с классами и объектами, полями и методами.

## Объявление и инстанцирование класса
Классы создаются с помощью ключевого слова `class`. Именовать их принято в стиле "UpperCamelCase". 

Так выглядит создание класса-пустышки `Dummy` и инстанцирование от него объекта `obj`:

```python  {.example_for_playground}
class Dummy:
    pass

obj = Dummy()
```

Рассмотрим класс `SimpleExample`, содержащий атрибуты. **Атрибуты** — общее название для полей и методов.

```python  {.example_for_playground}
class SimpleExample:
    """
    Demonstrates the basic capabilites of classes.
    """

    def __init__(self, a, b):
        self._a = a
        self.b = b

    def update_a(self, new_a):
        self._a = new_a
        return self._a

    def print_b(self):
        print(f"b value: {self.b}")
```

Тело класса начинается с поясняющего комментария docstring. Как и в случае с docstring внутри функций, он не обязателен.

Через dunder-метод `__init__()` объявлен **инициализатор** для установки атрибутов.

[Dunder-атрибуты](/courses/python/chapters/python_chapter_0180#block-dunder-attributes) (сокращение от «double underscore») — это специальные поля и методы класса с зарезервированными именами. Их имена обрамляются двойными подчеркиваниями. Напрямую dunder-методы почти никогда не вызываются. Они используются неявно внутри встроенных функций и операторов. Так, `__init__()` вызывается в момент инстанцирования объекта: {#block-dunder-attributes}

```python
x = SimpleExample(1, 2)
```

За счет dunder-атрибутов в питоне достигается **полиморфизм** работы встроенных функций и операторов с различными классами. Более подробно dunder-атрибуты мы обсудим в следующей главе. 

Если же говорить конкретно про `__init__()`, то в нем инициализируются поля объекта. Не путайте его с **конструктором** `__new__()`, создающим и возвращающим новый инстанс класса. Метод `__init__()` устанавливает поля в уже созданном объекте.

В `__init__()` можно передавать произвольные аргументы. Но первым аргументом идет `self`. Это общепринятое имя для текущего объекта класса. Оно не зафиксировано на уровне синтаксиса и вместо него сработало бы любое другое. Все же считается плохой практикой именовать текущий объект иначе.

В таких языках как C# и C++ текущий объект автоматически доступен внутри методов и не нуждается в передаче аргументом. Однако в питоне его требуется указывать явно в сигнатуре метода.

Мы передали в `__init__()` два аргумента `a` и `b` и присвоили их полям `_a` и `b`. 

Подчеркивание `_` в начале имени означает, что атрибут не является публичным и его не стоит использовать за пределами методов класса. Это тоже всего лишь соглашение, а не синтаксическое правило. 

В питоне, в отличие от Java, отсутствуют модификаторы доступа к атрибутам, которые контролируют их видимость снаружи класса. По факту все атрибуты публичные, то есть доступны пользователям класса и классам-наследникам. И только договоренность по именованию через префикс `_` останавливает разработчика от доступа к таким атрибутам за пределами класса. {#block-fields}

Так выглядит инстанцирование класса `SimpleExample`, обращение к публичному полю и вызов методов:

```python
obj = SimpleExample(4, 1)

print(obj.update_a(8))

obj.print_b()
obj.b = 10
obj.print_b()
```

```
8
b value: 1
b value: 10
```

Реализуйте класс `ResponseStats`, который считает статистику HTTP кодов ответа некоего сервиса. Помимо инициализатора у него должно быть три метода: {.task_text}

`add_response()`, который принимает HTTP код. {.task_text}

`response_count()`. Он принимает HTTP код и возвращает для него накопленное количество ответов. {.task_text}

`http_err_pct()`. Метод без аргументов. Возвращает процент HTTP кодов с ошибками от общего количества ответов. Код с ошибкой — это HTTP статус от 400 и выше. {.task_text}

```python {.task_source #python_chapter_0160_task_0010}
```
Внутри класса можно завести поле типа словарь, ключи которого — HTTP статусы, а значения - их количество. {.task_hint}
```python {.task_answer}
class ResponseStats:
    def __init__(self):
        self._resp = {}

    def add_response(self, code):
        if code not in self._resp:
            self._resp[code] = 0
        self._resp[code] += 1

    def response_count(self, code):
        return self._resp.get(code, 0)

    def http_err_pct(self):
        errors = 0
        total = 0
        for code, count in self._resp.items():
            if code >= 400:
                errors += count
            total += count

        if total == 0:
            return 0
            
        return errors / total * 100.0
```

## Атрибуты объекта и атрибуты класса
Мы обсудили, каким образом инстанс класса получает доступ к своему состоянию: в инициализатор и другие методы передается текущий объект `self`. К нему и привязываются атрибуты. Это **атрибуты объекта:** в различных инстансах класса их значения отличаются.

Также существуют и **атрибуты класса,** относящиеся к классу целиком и разделяемые всеми его объектами. Они определяются в самом теле класса, а не в теле его методов.

Рассмотрим класс `Message` для хранения текста некоторого сообщения в привязке к идентификатору. Заведем в нем атрибут класса `_id`. Его удобно использовать для автоинкремента идентификаторов объектов `Message`:

```python  {.example_for_playground}
class Message:
    _id = 0

    def __init__(self, data):
        self._msg_id = Message._id
        self._data = data
        Message._id += 1

    def print(self):
        print(f"{self._data}\t id {self._msg_id} / {self._id}")

    @classmethod
    def next_free_id(self):
        return Message._id


m1 = Message("MSG 1")
m2 = Message("MSG 2")

m1.print()
m2.print()

print(Message.next_free_id())
```
```
MSG 1     id 0 / 2
MSG 2     id 1 / 2
2
```

Метод `next_free_id()` читает значение атрибута класса. Обратите внимание, что он помечен через `@classmethod`. О том, что это такое, вы узнаете очень скоро. В коде класса `Message` вместо обращения `Message._id` мы могли бы применить более гибкий вариант `type(self)._id`. Встроенная функция `type()` принимает объект и возвращает его тип. В этом варианте не участвует хардкод имени класса. Такой код менее хрупкий, его проще рефакторить. 

В этом примере мы обращаемся к атрибуту класса двумя способами:
- В методах `__init__()` и `next_free_id()` через имя класса `Message._id`. 
- В методе `print()` через объект `self._id`.

В чем разница?

**Читать** атрибуты класса можно и через `self`, и через имя класса. Но **модифицировать** атрибуты класса получится только через имя класса. Иначе при попытке присвоения атрибута класса через объект будет создан одноименный атрибут объекта. По правилам разрешения имен он перекроет атрибут из области видимости класса.

В примере с классом `Message` нам ничего не мешало назвать атрибут объекта так же, как атрибут класса. Атрибут объекта привязан к `self`, и `self._id` перекрыл бы видимость `Message._id`. А если бы мы опустили имя класса и `self` и написали бы просто `_id`, то создалась бы обычная локальная переменная с областью видимости, не выходящей за пределы метода.

Проведите рефакторинг кода из примера: {.task_text}
- Переименуйте атрибут объекта `_msg_id` в `_id` так, чтобы логика работы класса не сломалась.
- При обращении к атрибуту класса замените хардкод имени класса на автоматическое его определение.

```python {.task_source #python_chapter_0160_task_0020}
class Message:
    _id = 0

    def __init__(self, data):
        self._msg_id = Message._id
        self._data = data
        Message._id += 1

    def print(self):
        print(f"{self._data}\t id {self._msg_id} / {self._id}")

    @classmethod
    def next_free_id(self):
        return Message._id
```
Присваивание атрибута в инициализаторе: `self._id = type(self)._id` {.task_hint}
```python {.task_answer}
class Message:
    _id = 0

    def __init__(self, data):
        self._id = type(self)._id
        self._data = data
        type(self)._id += 1

    def print(self):
        print(f"{self._data}\t id {self._id} / {type(self)._id}")

    @classmethod
    def next_free_id(self):
        return type(self)._id
```

## Методы объекта, методы класса и статические методы
Методы могут принадлежать объекту, классу, а также быть статическими.

В **методы объекта** первым аргументом передается `self`. Через него открывается доступ к атрибутам, значения которых уникальны для каждого объекта. Также методам объекта доступны атрибуты класса. Метод объекта может изменить значение атрибута класса, и его подхватят все инстансы. 

Разумеется, методы объекта могут быть вызваны только через объект. 

**Методы класса** не видят атрибуты объектов. Зато им доступны атрибуты класса, значения которых распространяются на все инстансы. В методы класса первым аргументом вместо `self` передается `cls`. Он должен ссылаться на сам класс, а не на его объект. С этой целью методы класса оборачиваются в декоратор `@classmethod`. Он указывает интерпретатору, что вместо экземпляра класса первым аргументом в метод требуется подставить сам класс. {#block-classmethod}

О том, что такое декораторы и как они устроены, мы поговорим в следующих главах. Если кратко, декоратор — это обертка над функцией, позволяющая изменить ее поведение, не трогая код. Чтобы обернуть функцию в декоратор, нужно указать символ `@` и имя декоратора над объявлением функции.

Методы класса вызываются через имя класса либо через инстанс:

```python  {.example_for_playground}
class C:
    field = "class field"

    @classmethod
    def update_field(cls, new_val):
        cls.field = new_val


print(C.field)

C.update_field("new value for class field")
print(C.field)

c = C()
print(c.field)
```
```
class field
new value for class field
new value for class field
```

В качестве примера метода класса можно привести `__new__()` — конструктор объектов класса.

**Статические методы** определяются с помощью декоратора `@staticmethod`; в них не передаются аргументы `self` или `cls`. Они не имеют доступа к атрибутам объекта или атрибутам класса. Их легко вынести за пределы класса и сделать свободными функциями. Статические методы полезны для инкапсуляции вспомогательных функций в область видимости класса, если их не планируется использовать за пределами класса.

Статические методы можно вызывать и через имя класса, и через объект.

Проведите рефакторинг класса `TestClass`: {.task_text}
- Исправьте ошибки в объявлении инициализатора.
- Сделайте `instance_method()` методом объекта, `class_method()` методом класса, а `static_method()` статическим методом.
- Почините консольный вывод поля класса и поля объекта.

```python {.task_source #python_chapter_0160_task_0030}
class TestClass:
    class_field = "This is class field"

    def __init__():
        print("Constructor")
        instance_field = "This is instance field"

    def instance_method():
        print(f"Instance method. {instance_field}. {class_field}")

    def class_method():
        print(f"Class method. {class_field}")

    def static_method():
        print("Static method")


tc = TestClass()

tc.instance_method()
tc.class_method()
tc.static_method()

TestClass.class_method()
TestClass.static_method()
```
В `__init__()` и `instance_method()` нужно передать `self` и работать с полем инсанса через него. К `class_method()` и `static_method()` нужно добавить декораторы `@classmethod` и `@staticmethod`. В `class_method()` при этом передать `cls`. {.task_hint}
```python {.task_answer}
class TestClass:
    class_field = "This is class field"

    def __init__(self):
        print("Constructor")
        self.instance_field = "This is instance field"

    def instance_method(self):
        print(f"Instance method. {self.instance_field}. {(type(self)).class_field}")

    @classmethod
    def class_method(cls):
        print(f"Class method. {cls.class_field}")

    @staticmethod
    def static_method():
        print("Static method")


tc = TestClass()

tc.instance_method()
tc.class_method()
tc.static_method()

TestClass.class_method()
TestClass.static_method()
```

Распространенная ошибка при работе со статическими методами заключается в том, что при их объявлении пропускается декоратор `@staticmethod`. Казалось бы, это бесполезная строка кода, ведь в метод и так не передается ни `self`, ни `cls`. Однако именно `@staticmethod` указывает интерпретатору, что метод вызывается через класс, а не объект, и в него не требуется неявно передавать дополнительный первый аргумент. 

Если завести статический метод без декоратора и вызвать через объект класса, произойдет исключение:

```python  {.example_for_playground}
class TestClass:
    def static_method():
        print("Static method")

tc = TestClass()
tc.static_method()
```
```
Traceback (most recent call last):
  File "example.py", line 6, in <module>
    tc.static_method()
TypeError: TestClass.static_method() takes 0 positional arguments but 1 was given
```

Поэтому для корректного объявления статических методов не забывайте оборачивать их в декоратор `@staticmethod`.

## Определение класса как исполняемое выражение
Итак, мы уже знаем, что в теле класса можно заводить переменные. Такие переменные считаются полями класса. Инстанцирование этих переменных происходит в момент объявления класса. Именно объявления класса, а не создания объекта класса:
```python  {.example_for_playground}
class SideEffect:
    def __init__(self):
        print("Side effect")

print("Before class definition")

class Test:
    test_class_field = SideEffect()

print("After class definition")
```
```
Before class definition
Side effect
After class definition
```

Как видно из примера, код с сайд-эффектом отработал в момент определения класса. Напрашивается вопрос: можно ли выполнить прямо в теле класса что-то более сложное, чем конструирование поля?

В документации по этому поводу [оставлена](https://docs.python.org/3.5/reference/compound_stmts.html#class-definitions) подсказка: определение класса является исполняемым выражением.

Фактически это означает, что тело класса может целиком и полностью состоять из кода, ограниченного только полетом фантазии: импорта модулей, вложенных циклов, походов в бд...

```python  {.example_for_playground}
def send_sms():
    print("Sending sms...")

class BadBadClass:
    for i in range(3):
        send_sms()

print("After class definition")
```
```
Sending sms...
Sending sms...
Sending sms...
After class definition
```

Зачем мы вообще подняли эту тему? Об этой особенности классов нужно знать. Всячески уберегать себя от ее использования, но быть готовым столкнуться с чем-то подобным в чужом коде.

## Резюмируем
- Для создания класса используется ключевое слово `class`.
- Инициализатор класса объявляется с помощью dunder-метода `__init__()`. Он нужен для установки атрибутов объекта.
- В питоне отсутствует разграничение атрибутов на публичные и защищенные. Вместо этого действует соглашение: приватные атрибуты именовать с префиксом `_`.
- Атрибуты могут принадлежать объекту либо классу.
- Методы могут принадлежать объекту, классу, либо быть статическими.
- Определение класса является выполняемым выражением.
