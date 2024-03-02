# Глава 32. Модуль asyncio: асинхронный запуск задач

При асинхронном подходе функция может начать выполняться не в момент вызова, а позднее. При этом появляется возможность не блокировать вызывающий поток, а продолжить выполнять некую работу и периодически проверять завершенность асинхронного вызова. Также легко задать, какой код должен быть выполнен по завершении асинхронной операции.

Все это и даже больше реализует встроенный модуль **asyncio**.


## Корутины и синтаксис async/await
Для выполнения асинхронного кода в питоне используются **корутины**. Чтобы превратить функцию в корутину, необходимо в ее сигнатуре прописать ключевое слово `async`.

```python
async def f():
    pass


print(type(f()))
```
```
<class 'coroutine'>
```

В данном примере функция `f()` возвращает объект корутины. Ее тело называют **асинхронным контекстом.** Только внутри асинхронного контекста разрешено использовать ключевое слово `await`. С его помощью запускается корутина и осуществляется ожидание завершения ее работы. Часто одни корутины вызываются из других. `await` сигнализирует, что при выполнении следующего за ним выражения возможно переключение с текущей корутины на основной поток выполнения или на другую корутину.

Ключевое слово `await` применимо к любым объектам, у которых определен dunder-метод `__await__()`. Такие объекты называют **awaitable**. К ним относятся:
- корутины,
- объекты типа `asyncio.Future`,
- объекты типа `asyncio.Task`,
- объекты пользовательских типов с определенным dunder-методом `__await__()`.

Корутины похожи на генераторы, которые принимают значения. Но есть и отличия:
- тип корутин — это `coroutine`, тип генераторов — `function`.
- корутины являются awaitable-объектами, а генераторы — нет.

У корутин и генераторов отличаются и типичные кейсы использования:
- Корутины незаменимы в случаях, когда дает выигрыш кооперативная многозадачность. Как правило, это IO-bound задачи. 
- Генераторы отлично подходят для ленивых вычислений.

Рассмотрим пример вызова одной корутины из другой.

```python
async def coro_100():
    print(100)


async def coro_42():
    await coro_100()
    print(42)


def main():
    try:
        c = coro_42()
        c.send(None)
    except StopIteration:
        pass
    print("main ended")


main()
```
```
100
42
main ended
```

Внутри функции `main()` происходит создание объекта корутины `coro_42()`. Чтобы корутина начала свое выполнение, ее необходимо запустить с помощью метода `send()`. Когда она завершится, будет выброшено исключение `StopIteration` (по аналогии с итератором).

Запуск самой первой корутины получается слишком многословным:
- нужно инициировать запуск самостоятельно,
- нужно обрабатывать завершение с помощью блока `try/except`.

С помощью библиотеки `asyncio` перепишем этот код более компактно.

```python
import asyncio


async def coro_100():
    print(100)


async def coro_42():
    await coro_100()
    print(42)


async def main():
    await coro_42()
    print("main ended")


asyncio.run(main())
```

`asyncio.run()` принимает корутину `main()`и запускает ее внутри своего цикла событий. `await` внутри корутины `main()` скрывает запуск `coro_42()` и перехват исключения по окончании.

Цикл событий внутри `asyncio` обеспечивает кооперативную многозадачность между корутинами. Каждая корутина может уступить выполнение другой корутине. А цикл событий решает, какую из корутин следующей ставить на выполнение.

При работе с `asyncio` не требуется напрямую взаимодействовать с циклом событий за исключением непосредственно запуска через `asyncio.run()`. Однако модуль имплементирует низкоуровневый API, который все-таки позволяет это делать. В современных версиях языка его использование не является предпочтительным способом работы с `asyncio`. Работу с асинхронностью при помощи низкоуровневого API `asyncio` можно встретить в проектах, написанных на питоне версии ниже, чем `3.7`.

## Задачи
При работе с `asyncio` корутины часто оборачиваются в «задачи» **asyncio.Task** (их еще называют просто «таски»). Благодаря им можно:
- получать результат из корутин с возвращаемым значением,
- отменять запланированные, но еще не запущенные корутины,
- добавлять коллбек по завершении корутины,
- запускать несколько корутин и дожидаться выполнения одной или всех,
- передавать данные между корутинами.

Задача создается из объекта корутины. В отличие от корутин, ее выполнение начинается почти сразу после создания. «Почти», потому что цикл событий должен сначала запуститься.

Пример запуска задачи:

```python
import asyncio


async def coroutine_task():
    print("task started")
    await asyncio.sleep(1)


async def main():
    print("main coroutine")

    task = asyncio.create_task(coroutine_task())
    await task


asyncio.run(main())
print("main ended")
```
```
main coroutine
task started
main ended
```

Функция `asyncio.create_task()` принимает корутину и возвращает созданный из нее объект задачи. После создания задачи мы дожидаемся ее завершения с помощью `await task`. Внутри `coroutine_task()` имитируем IO-bound вычисления через `asyncio.sleep()`. 

Асинхронная функция `asyncio.sleep()` отличается от обычного вызова `time.sleep()`. Она не блокирует поток выполнения, а отдает управление циклу событий, чтобы тот выбрал другую корутину для работы. Это оптимизация, чтобы процессор зря не простаивал. А если нет корутин для исполнения, тогда будет функция перейдет в обычное ожидание.

Вызов `asyncio.sleep(0)` — указание планировщику из `asyncio` выбрать другую корутину для исполнения. Заметьте, что `asyncio.sleep()` вызывается вместе с `await`,  потому что сама эта функция является корутиной.

Запустим две задачи. Одна из них должна завершиться почти моментально, другая имитирует более длительные вычисления.

```python
import asyncio


async def coro():
    print("Hi, coro!")
    await asyncio.sleep(0)
    print("Bye, coro!")


async def long_calculation():
    print("Long calculation")
    await asyncio.sleep(1)
    print("Long calculation done")


async def main():
    _ = asyncio.create_task(coro())
    t2 = asyncio.create_task(long_calculation())

    await t2


asyncio.run(main())
```
```
Hi, coro!
Long calculation
Bye, coro!
Long calculation done
```


Напишите программу, которая конкурентно выполняет две задачи: {.task_text}
- Раз в секунду выводит на экран сообщение `"tick"`.
- Проверяет наличие файла senjun.py в текущей директории и при его появлении выводит на экран `"exists!"`. Программа должна завершиться, как только интересующий файл появится в директории. Наличие файла можно определить с помощью вызова `os.path.exists()`. {.task_text}

```python {.task_source #python_chapter_0320_task_0010}
import asyncio
from threading import Timer

# emulate asynchronous file getting
def create_file(name):
    with open(name, "w") as f:
        pass


t = Timer(2.0, create_file, ["senjun.py"])
t.start()


async def check_existing(path):
    # your code


async def tick():
    # your code


async def main():
    # your code


asyncio.run(main())
```
Запустите две задачи: одну для вывода в бесконечном цикле строки `"tick"`. И вторую задачу для проверки в вечном цикле существования файла. {.task_hint}

```python {.task_answer}
import asyncio
import os.path
from threading import Timer

def create_file(name):
    with open(name, "w") as f:
        pass


t = Timer(2.0, create_file, ["senjun.py"])
t.start()


async def check_existing(path):
    while not os.path.exists(path):
        await asyncio.sleep(0)

    print("exists!")


async def tick():
    while True:
        print("tick")
        await asyncio.sleep(1)


async def main():
    asyncio.create_task(tick())

    ready = asyncio.create_task(check_existing("senjun.py"))
    await ready


asyncio.run(main())
```

## Ожидание выполнения группы задач
Часто возникает ситуация, когда нужно запустить много задач и дождаться выполнения их всех. Сделать это можно «в лоб», пройдясь циклом по списку задач:

```python
import asyncio


async def coro(number):
    print(f"> task {number} executing")
    await asyncio.sleep(0.5)


async def main():
    tasks = [asyncio.create_task(coro(i)) for i in range(5)]
    for task in tasks:
        await task


asyncio.run(main())
```
```
> task 0 executing
> task 1 executing
> task 2 executing
> task 3 executing
> task 4 executing
```

Чтобы не писать цикл для ожидания каждой задачи, можно использовать встроенную функцию `asyncio.gather()`. На вход она принимает итерабельный объект с задачами, а возвращает `Future` для группы задач. Это существенно упрощает код.

```python
tasks = [asyncio.create_task(coro(i)) for i in range(5)]
await asyncio.gather(*tasks)
```

Исправьте код так, чтобы в нем не было блокирующих вызовов. Код должен выполниться примерно за 3 секунды. {.task_text}

```python {.task_source #python_chapter_0320_task_0020}
import asyncio
import time


async def sleep():
    time.sleep(1)


async def sum(name, numbers):
    total = 0
    for number in numbers:
        print(f"Task {name}: Computing {total}+{number}")
        await sleep()
        total += number
    print(f"Task {name}: Sum = {total}")


async def main():
    tasks = [
        asyncio.create_task(sum("A", [1, 2])),
        asyncio.create_task(sum("B", [1, 2, 3])),
    ]
    await asyncio.gather(*tasks)


asyncio.run(main())

```
Замените блокирующий вызов `time.sleep(1)` на `asyncio.sleep(1)`. {.task_hint}
```python {.task_answer}
import asyncio
import time


start = time.time()


async def sleep():
    await asyncio.sleep(1)


async def sum(name, numbers):
    total = 0
    for number in numbers:
        print(f"Task {name}: Computing {total}+{number}")
        await sleep()
        total += number
    print(f"Task {name}: Sum = {total}")


async def main():
    tasks = [
        asyncio.create_task(sum("A", [1, 2])),
        asyncio.create_task(sum("B", [1, 2, 3])),
    ]
    await asyncio.gather(*tasks)


asyncio.run(main())
```

## Ожидание выполнения первой задачи из множества
Иногда возникает потребность дождаться выполнения только самой первой из всех запущенных задач. Для этого используется вызов `asyncio.wait()`. Эта функция принимает список задач и указание, когда нужно завершиться. По умолчанию происходит ожидание всех переданных задач. Возвращает она кортеж из двух чисел: количество завершенных и незавершенных задач.

```python
import asyncio
import time


async def task_coro(value):
    await asyncio.sleep(value)
    print(f"> task '{value}' done")


async def main():
    start = time.time()
    print("main starting")
    tasks = [asyncio.create_task(task_coro(i)) for i in range(5)]
    done, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_COMPLETED)
    print(f"main done for {time.time() - start:.4f} seconds")


asyncio.run(main())
```
```
main starting
> task '0' done
main done for 0.0002 seconds
```

В данном примере блокировка на ожидании первой завершенной задачи прекратилось. Однако остальные задачи продолжили свое выполнение.


## Асинхронный контекстный менеджер
Асинхронным контекстным менеджером называется объект, у которого определены dunder-методы `__aenter__()` и `__aexit__()`. Напомним, что у обычного контекстного менеджера должны существовать методы `__enter__()`, `__exit__()`. Главное отличие асинхронного контекстного менеджера в том, что при инициализации и завершении он может передать управление циклу событий, чтобы не блокироваться на длительной операции.

```python
import asyncio


class AsyncContextManager:
    async def __aenter__(self):
        print("read big file...")
        await asyncio.sleep(2)

    async def __aexit__(self, exc_type, exc, tb):
        print("async manager exited")
        await asyncio.sleep(0.5)


async def tick():
    while True:
        print("tick")
        await asyncio.sleep(0.5)


async def main():
    asyncio.create_task(tick())
    async with AsyncContextManager() as m:
        print("inside async with")


asyncio.run(main())

``` 
```
read big file...
tick
tick
tick
tick
inside async with
async manager exited
tick
```

## Асинхронный итератор
Если у объекта есть dunder-методы `__aiter__()`, `__anext__()`, то он называется асинхронным итератором. Такие объекты могут использоваться в контексте `async for` по аналогии с `async with`.

```python
import asyncio


class AsyncIterator:
    def __init__(self):
        self.counter = 0

    def __aiter__(self):
        return self

    async def __anext__(self):
        if self.counter >= 2:
            raise StopAsyncIteration

        self.counter += 1
        await asyncio.sleep(1)

        return self.counter


async def coro():
    while True:
        print("Tick")
        await asyncio.sleep(0.5)


async def main():
    _ = asyncio.create_task(coro())
    async for item in AsyncIterator():
        print(item)


asyncio.run(main())
```
```
Tick
Tick
1
Tick
Tich
2
```

## Резюмируем
- Ключевое слово `async` в сигнатуре функции превращает функцию в корутину.
- Для вызова корутины и ожидания ее выполнения используется ключевое слово `await`. 
- Ключевое слово `await` может быть применено к любому awaitable-объекту.
- Модуль `asyncio` позволяет оборачивать корутины в высокоуровневые задачи и работать уже с ними.
- Цикл событий `asyncio` запускается с помощью вызова `asyncio.run()`.
- `asyncio.gather()` используется для ожидания завершения нескольких задач, а `asyncio.wait()` — для ожидания выполнения только самой первой задачи.
- Асинхронный вариант контекстного менеджера создается с помощью `async with`.
- Асинхронный итератор используется в цикле `async for`.
