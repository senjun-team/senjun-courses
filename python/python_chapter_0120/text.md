# Глава 12. Кортежи
> Питон — это эксперимент по определению степени свободы разработчика. Слишком много свободы, и никто не сможет читать чужой код; слишком мало, и это убьет выразительность.  
Гвидо ван Россум

Кортеж (`tuple`) — это неизменяемый список. Обсудим нюансы работы с кортежами и особенности их реализации в CPython.

## Создание кортежей и их отличия от списков
Создается кортеж двумя способами.

Через круглые скобки:

```python
langs = ("haskell", "erlang", "scala")
```

С помощью конструктора, в который передается итерабельный объект (в данном примере — список):

```python
lst = [1, 2, 3]
tpl = tuple(lst)
```

Варианты объявления пустого кортежа:

```python  {.example_for_playground}
t1 = ()
t2 = tuple()

print(type(t1), type(t2))
```

Консольный вывод подтверждает, что объектам `t1` и `t2` интерпретатором корректно присвоен тип `tuple`:

```
<class 'tuple'> <class 'tuple'>
```

Нюансы начинаются, когда нам требуется завести кортеж, состоящий из одного элемента. Как считаете, что выведет этот код?

```python  {.example_for_playground}
tpl = (128)
print(type(tpl))
```

Как ни странно, интерпретатор принимает `tpl` за целое число:

```
<class 'int'>
```

Дело в том, что в питоне круглые скобки `()` зарезервированы не только для конструирования кортежа, но и для расстановки приоритета в математических операциях. Например:

```python
avg = (5.6 + 8.1) / 2.0
```

Выход есть! Чтобы завести кортеж из единственного элемента, достаточно поставить после элемента запятую:

```python  {.example_for_playground}
tpl = (128,)
print(type(tpl))
print(tpl)
```

Эта запятая заставляет интерпретатор правильно определить тип `tuple`. Обратите внимание, что запятая присутствует даже при консольном выводе кортежа, состоящего из одного элемента:

```
<class 'tuple'>
(128,)
```

## Отличия кортежей от списков
К кортежу применимо подмножество не модифицирующих [операций над списком.](/courses/python/chapters/python_chapter_0110#block-operators) Соответственно у типа `tuple` присутствуют те же не модифицирующие методы, что и у `list`. Кроме того, из кортежа можно получить срез. 

Выведите в консоль срез кортежа `tpl` от индекса 2 до индекса 8 включительно с шагом 2. {.task_text}

```python {.task_source #python_chapter_0120_task_0010}
tpl = (-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
```
Синтаксис для среза до конечного индекса не включительно: `[начальный индекс: конечный индекс: шаг]`. {.task_hint}
```python {.task_answer}
tpl = (-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
print(tpl[2:9:2])
```

Что выведет этот код? В случае ошибки напишите `error`. {.task_text}

```python  {.example_for_playground}
words = ("upper", "level", "demand")
for w in words: 
    if w == w[::-1]: 
        print(w)
```

```consoleoutput {.task_source #python_chapter_0120_task_0040}
```
Создается кортеж `words` из 3-х строк. В цикле `for` перебираются элементы кортежа, то есть строки "upper", "level", "demand". В консоль выводятся только те строки, которые являются палиндромами (то есть одинаково читаются слева направо и справа налево). В кортеже содержится только одна такая строка: "level". Она и будет выведена в консоль. {.task_hint}
```python {.task_answer}
level
```

## Неизменяемость кортежа и ее взлом
Как мы уже [разбирали,](/courses/python/chapters/python_chapter_0090#block-compare) что оператор `==` сравнивает содержимое объектов. А ключевое слово `is` проверяет, являются ли два объекта одним и тем же объектом в памяти. Как считаете, что выведет этот код?

```python  {.example_for_playground}
l1 = [1, 2]
l2 = [1, 2]

t1 = (1, 2)
t2 = (1, 2) 

print("l1 eq l2", l1 == l2)
print("l1 is l2", l1 is l2)

print("t1 eq t2", t1 == t2)
print("t1 is t2", t1 is t2)
```

Оператор `==` вернет `True` и для списка, и для кортежа. А вот `is` вернет `True` только для кортежа: значит, у `t1` и `t2` одинаковый адрес в памяти.

```
l1 eq l2 True
l1 is l2 False
t1 eq t2 True
t1 is t2 True
```

Причина в неизменяемости кортежа. Интерпретатор видит, что содержимое кортежей совпадает, а раз оно не может быть модифицировано, то нет смысла дважды выделять память под одни и те же данные.

Являтся ли неизменяемость кортежа нерушимой инвариантой или ее можно случайно «сломать»? Удаление и добавление элементов кортежи не поддерживают. Попытка изменения элемента по индексу ведет к исключению: {#block-tuple-immutability}

```python  {.example_for_playground}
t = (5, 10, 20)
t[1] = 100
```

```
TypeError: 'tuple' object does not support item assignment
```

...Однако если один из элементов кортежа относится к изменяемым типам, то его, внимание, можно модифицировать! Например, если элемент — это список, интерпретатор не воспрепятствует добавлению в него элемента.

Проведите опыт: заведите два кортежа `t1` и `t2` с одинаковым содержимым `(1, [], 2)`. Выведите в консоль на двух разных строках проверку на поэлементное равенство и на совпадение объектов в памяти. {.task_text}

Добавьте в список, содержащийся в кортеже `t1`, элемент со значением `"A"`. Повторите консольный вывод двух проверок на равенство. {.task_text}

```python {.task_source #python_chapter_0120_task_0020}
```
Для поэлементного сравнения кортежей воспользуйтесь оператором `==`. Для проверки, являются ли кортежи одним и тем же объектом в памяти, воспользуйтесь оператором `is`. {.task_hint}
```python {.task_answer}
t1 = (1, [], 2)
t2 = (1, [], 2)

print(t1 == t2)
print(t1 is t2)

t1[1].append("A")

print(t1 == t2)
print(t1 is t2)
```

О чем говорит консольный вывод задачи? 

Во-первых, он подтверждает цитату из заголовка главы: питон — это эксперимент по определению степени свободы разработчика ;) 

Мы убедились, что содержимое кортежа все-таки можно изменить (на длине кортежа это конечно не отразится). Главное, чтобы тип элемента кортежа этому способствовал. 

Во-вторых, если в двух казалось бы одинаковых кортежах содержатся элементы изменяемых типов, то интерпретатор уже не будет хранить эти кортежи по общему адресу (`is` для них вернет `False`). 

## Использование оператора моржа для заполнения коллекций {#block-walrus}
На примере кортежей рассмотрим использование [оператора моржа](/courses/python/chapters/python_chapter_0030#block-walrus) `:=` для заполнения коллекций.

```python  {.example_for_playground}
query = "Как варить макароны "

stats = (q := query.strip().lower(), len(q), q.split())
print(stats)
```
```
('как варить макароны', 19, ['как', 'варить', 'макароны'])
```

В первый элемент кортежа и переменную `q` попала строка `query` с удаленными начальными и конечными пробелами, приведенная к нижнему регистру. Второй элемент кортежа — длина полученной строки. И, наконец, третий элемент — список, полученный из слов строки.

Без оператора моржа этот же код выглядел бы чуть длиннее:

```python  {.example_for_playground}
q = query.strip().lower()
stats = (q, len(q), q.split())
```

Как видите, оператор моржа удобно использовать в случаях, когда коллекцию требуется заполнить некими производными от начального значения.

Имплементируйте функцию-однострочник `get_square_root()`, которая принимает единственный аргумент — число. Функция должна вернуть кортеж из двух элементов: квадратный корень числа; строковое представление квадратного корня числа с точностью 2 знака после запятой. Для заполнения кортежа примените оператор моржа. {.task_text}

Например, для аргумента 10 функция должна вернуть кортеж `(3.16227766, '3.16')`. {.task_text}

Для превращения числа с плавающей точкой в строку, содержащую два знака после запятой, воспользуйтесь [f-строкой](/courses/python/chapters/python_chapter_0100/#block-formatting) вида `f"{val:.nf}"`. Здесь `val` — имя переменной, а `n` — количество знаков после запятой. {.task_text}

```python {.task_source #python_chapter_0120_task_0030}
import math

def get_square_root(x):
    # Your code here

print(get_square_root(10))
```
Для расчета квадратного корня воспользуйтесь функцией `sqrt()` из модуля `math`. {.task_hint}
```python {.task_answer}
import math

def get_square_root(x):
    return ( r := math.sqrt(x), f"{r:.2f}")

print(get_square_root(10))
```

## Функция enumerate()

В питоне есть встроенная функция `enumerate()`, которая призвана упростить написание циклов. Она используется, когда при итерации по коллекции требуется работать с индексами элементов. Например, мы хотим пронумеровать все элементы списка. Сделаем это с помощью счетчика `i`:

```python  {.example_for_playground}
i = 1

for x in ["A", "B", "C"]:
    print(i, x)
    i += 1
```
```
1 A
2 B
3 C
```

Функция `enumerate()` позволяет упростить код:

```python  {.example_for_playground}
for i, x in enumerate(["A", "B", "C"], start=1):
    print(i, x)
```
```
1 A
2 B
3 C
```

Удобно. Но какое отношение функция `enumerate()` имеет к кортежам? Все просто: она на каждой итерации цикла генерирует **кортеж** из двух значений — индекса итерации и элемента итерируемого объекта. Если аргумент `start` не указан явно, то индексы начинаются с нуля. Более подробно мы рассмотрим `enumerate()` [позже.](/courses/python/chapters/python_chapter_0250#block-enumerate)


## Резюмируем
- Тип `tuple` (кортеж) — это неизменяемый список, хотя у иммутабельности есть исключение: если в кортеже хранится элемент изменяемого типа (например, список), то этот элемент можно модифицировать.
- Для создания кортежа используются круглые скобки `()` либо конструктор `tuple()`.
- При создании кортежа из одного элемента не забывайте после элемента указывать запятую: `t = (8,)`.
- Оператор моржа может использоваться для удобного заполнения коллекций, в том числе кортежей.