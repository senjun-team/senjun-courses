# Глава 30. Потоки, процессы

Для высокоуровневого выполнения задач на пуле процессов или потоков предназначен модуль `concurrent.futures`. Низкоуровневые примитивы для работы с процессами и потоками реализованы в модулях `multiprocessing` и `threading`. Об этих трех модулях мы и поговорим.

## Процессы
Как мы [выяснили](/courses/python/chapters/python_chapter_0290#block-cpu-bound) в главе про GIL, CPU-bound задачи лучше распараллеливать на процессы, а IO-bound — на потоки. 

Рассмотрим два способа работы с процессами:
- Класс `ProcessPoolExecutor` из модуля `concurrent.futures` — самый простой способ распределить задачи по процессам.
- Модуль `multiprocessing` — низкоуровневый интерфейс для более тонкого управления процессами.

### Модуль concurrent.futures
Модуль `concurrent.futures` предоставляет простой способ асинхронного запуска задач буквально парой строк кода. 

Задачи могут выполняться как в отдельных процессах с помощью класса `ProcessPoolExecutor`, так и в потоках с помощью `ThreadPoolExecutor`. У этих классов одинаковый интерфейс, определенный в абстрактном классе `Executor` и состоящий из 3-х методов: `submit()`, `map()`, `shutdown()`.

```python
submit(fn, /, *args, **kwargs)
```

`submit()` отправляет функцию с аргументами `fn(*args, **kwargs)` на выполнение в пул процессов и возвращает объект `Future`. Сущность [future](https://en.wikipedia.org/wiki/Futures_and_promises) встречается во многих языках программирования и представляет собой объект, ждущий результатов выполнения задачи.

Рассмотрим пример запуска задач на пуле процессов. Так выглядит выполнение задачи `calc_and_sleep()` в пуле, состоящем из единственного процесса. {#block-measure-time}

```python  {.example_for_playground}
import time
from concurrent.futures import ProcessPoolExecutor


def calc_and_sleep(a, b):
    time.sleep(1)
    return pow(a, b)


start = time.perf_counter()

with ProcessPoolExecutor(max_workers=1) as executor:
    future = executor.submit(calc_and_sleep, 128, 256)
    print(future.result())

finish = time.perf_counter()
print(f"Finished in {finish - start:.2f} seconds")
```
```
279095111627852376407822673918065072905887935345660252615989519488029661278604994789701101367875859521849524793382568057369148405837577299984720398976429790087982805274893437406788716103454867635208144157749912668657006085226160261808841484862703257771979713923863820038729637520989894984676774385364934677289947762340313157123529922421738738162392233756507666339799675257002539356619747080176786496732679854783185583233878234270370065954615221443190595445898747930123678952192875629172092437548194134594886873249778512829119416327938768896
Finished in 1.01 seconds
```

Для получения результата выполнения функции `calc_and_sleep()` мы вызвали метод `result()` объекта типа `Future`. С помощью `perf_counter()` было замерено время выполнения кода. 

```python
map(fn, *iterables, timeout=None, chunksize=1)
```

`map()` применяет функцию к каждому элементу итерабельного объекта. По принципу работы она похожа на [встроенную функцию](/courses/python/chapters/python_chapter_0280#block-map) `map()`, только исполняет задачи асинхронно в пуле процессов. `map()` возвращает итератор на результаты применения функции. Если указан параметр `timeout` (в секундах), а функция не успела выполниться за это время, итератор бросает исключение `TimeoutError`.

Параметр `chunksize` определяет размер чанков, на которые распределяется итерабельный объект по процессам из пула. Для больших коллекций имеет смысл увеличить значение `chunksize` и таким образом ускорить обработку.

```python
shutdown(wait=True, *, cancel_futures=False)
```
`shutdown()` завершает пул процессов. Этот метод не нужно вызывать, если `ProcessPoolExecutor` создается через контекстный менеджер `with`.  

Если аргумент `wait` равен `True`, метод блокируется до тех пор, пока все задачи не завершатся. 

Если `cancel_futures` равен `True`, то все запланированные, но не начавшие исполнятся в пуле задачи будут отменены.

## Методы запуска процессов: fork, spawn, forkserver

При создании дочернего процесса в Python используется один из трёх методов запуска. Выбор метода влияет на производительность и поведение программы.

### fork (только Unix/Linux)

Метод `fork` создаёт дочерний процесс путём копирования состояния родительского процесса. Все переменные, импорты и открытые файлы наследуются автоматически.

**Преимущества:** быстрый запуск (просто копируется память процесса), функции и данные доступны в дочернем процессе без дополнительной сериализации, не требует `if __name__ == "__main__":` защиты.

**Недостатки:** работает только на Unix-платформах (Linux, macOS), наследует всё состояние родителя, включая потенциально некорректные данные (например, состояние потоков), может вызывать проблемы с асинхронным кодом и некоторыми C-расширениями.

```python
from multiprocessing import get_context

ctx = get_context('fork')
with ProcessPoolExecutor(mp_context=ctx) as executor:
    # ...
```

Проблемы с C-расширениями возникают из-за того, что после `fork` в дочернем процессе остаётся только копия памяти родительского процесса, но внутренние состояния библиотек могут оказаться некорректными:

- [**NumPy**](https://numpy.org/devdocs/building/blas_lapack.html#default-behavior-for-blas-and-lapack-selection) — использует многопоточные вычисления через [BLAS](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms)/[OpenMP](https://en.wikipedia.org/wiki/OpenMP). После `fork` внутренние блокировки могут остаться в заблокированном состоянии, что приведёт к зависанию дочернего процесса.
- [**Pillow (PIL)**](https://python-pillow.github.io/) — хранит состояние инициализации и кэши изображений. После `fork` может работать некорректно, особенно если изображения открывались до форка.
- [**psycopg2**](https://www.psycopg.org/docs/) — пул соединений с базой данных после `fork` наследуется, но сами соединения становятся невалидными. Это приводит к ошибкам при попытке выполнить запрос.
- [**OpenCV**](https://en.wikipedia.org/wiki/OpenCV) — как и NumPy, использует многопоточность для ускорения вычислений. После `fork` возможны дедлоки и некорректная работа внутренних очередей задач.

Общее правило: если библиотека инициализировала потоки, соединения или другие ресурсы до `fork`, использовать их в дочернем процессе небезопасно, будет лучше в таких случаях использовать `spawn`.

### spawn (все платформы)

Метод `spawn` запускает чистый интерпретатор Python и импортирует только необходимые объекты из главного модуля.

**Преимущества:** работает на всех платформах (Windows, macOS, Linux), чистое состояние дочернего процесса (нет унаследованных проблем), безопасен для асинхронного кода.

**Недостатки:** медленнее `fork` (требуется запуск нового интерпретатора), все передаваемые объекты должны быть сериализуемы через pickle, требует `if __name__ == "__main__":` для защиты от рекурсивного запуска, функции должны быть определены на верхнем уровне модуля (не внутри других функций).

```python
from multiprocessing import get_context

ctx = get_context('spawn')
if __name__ == "__main__":
    with ProcessPoolExecutor(mp_context=ctx) as executor:
        # ...
```

### forkserver (Unix/Linux, по умолчанию в Python 3.14+)

Метод `forkserver` создаёт отдельный процесс-сервер, который порождает дочерние процессы по запросу.

**Преимущества:** безопаснее `fork` (сервер не наследует состояние вашего кода), быстрее `spawn` после первоначального запуска сервера, решает проблемы `fork` с потоками и асинхронным кодом.

**Недостатки:** работает только на Unix-платформах, функции должны быть импортируемы из модуля (как и с `spawn`), требует `if __name__ == "__main__":` в большинстве случаев.

```python
from multiprocessing import get_context

ctx = get_context('forkserver')
with ProcessPoolExecutor(mp_context=ctx) as executor:
    # ...
```

### Изменения в Python 3.14

**В Python 3.14 метод запуска по умолчанию изменён с `fork` на `forkserver`** на Unix-платформах (кроме macOS). На Windows и macOS по-прежнему используется `spawn`.

| Платформа | Python 3.13 и раньше | Python 3.14+ |
|-----------|---------------------|--------------|
| Linux | `fork` | `forkserver` |
| macOS | `spawn` | `spawn` |
| Windows | `spawn` | `spawn` |

Подробнее про методы запуска потоков вы можете ознакомится на странице официальной документации [Python.](https://docs.python.org/3/library/multiprocessing.html?spm=a2ty_o01.29997173.0.0.74c65171ca6ZQy#contexts-and-start-methods:~:text=the%20__main__%20module.-,Contexts%20and%20start%20methods,-%C2%B6)

### Почему это важно

Если ваш код использует `ProcessPoolExecutor` без явного указания контекста, он будет работать по-разному в разных версиях Python:

```python
# Может не работать в Python 3.14+ на Linux
with ProcessPoolExecutor() as executor:
    for result in executor.map(func, data):
        print(result)
```

Для совместимости с Python 3.14+ и кроссплатформенной работы используйте:

```python
from multiprocessing import get_context

# Пытаемся использовать fork (быстрее, работает с exec/inject)
try:
    ctx = get_context('fork')
except ValueError:
    # На Windows fork недоступен, используем spawn
    ctx = get_context('spawn')

with ProcessPoolExecutor(mp_context=ctx) as executor:
    for result in executor.map(func, data):
        print(result)
```

### Рекомендации

1. **Для локальной разработки на Windows/macOS** всегда используйте `if __name__ == "__main__":` и определяйте функции на верхнем уровне модуля.
2. **Для Linux-серверов с Python 3.14+** явно указывайте `mp_context=get_context('fork')`, если код выполняется через `exec()` или инджектится в файлы.
3. **Для кроссплатформенных проектов** используйте динамическое определение контекста:

```python
try:
    ctx = get_context('fork')
except ValueError:
    ctx = get_context('spawn')

if __name__ == "__main__":
    with ProcessPoolExecutor(mp_context=ctx) as executor:
        # ...
```


Дан массив чисел `nums` и функция `is_prime()`, определяющая, является ли число простым. {.task_text}

Через `ProcessPoolExecutor` нужно распараллелить применение `is_prime()` к элементам `nums`. Для каждого из чисел в том порядке, в котором они идут в `nums`, вывести в консоль строку вида `"112272535095293 is prime: True"`. {.task_text}

Для итерации по числам и результатам метода `map()` объекта `ProcessPoolExecutor` используйте [встроенную функцию](/courses/python/chapters/python_chapter_0280#block-zip) `zip()`. {.task_text}

```python {.task_source #python_chapter_0300_task_0010}
import math

nums = [
    112272535095293,
    112582705942171,
    102272535065492,
    115280095190773,
    115797848077099,
    1099726899285419]

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    sqrt_n = int(math.floor(math.sqrt(n)))
    for i in range(3, sqrt_n + 1, 2):
        if n % i == 0:
            return False
    return True

# Your code here
```
Для получения пар чисел и результатов примененной к ним функции `is_prime()` можно воспользоваться функцией `zip()`. {.task_hint}
```python {.task_answer}
import math

from concurrent.futures import ProcessPoolExecutor
from multiprocessing import get_context

nums = [
    112272535095293,
    112582705942171,
    102272535065492,
    115280095190773,
    115797848077099,
    1099726899285419]

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    sqrt_n = int(math.floor(math.sqrt(n)))
    for i in range(3, sqrt_n + 1, 2):
        if n % i == 0:
            return False
    return True

try:
    ctx = get_context('fork')
except ValueError:
    ctx = get_context('spawn')

if __name__ == "__main__":
    with ProcessPoolExecutor(mp_context=ctx) as executor:
        for number, prime in zip(nums, executor.map(is_prime, nums)):
            print(f"{number} is prime: {prime}")
```

На самом деле под капотом `ProcessPoolExecutor` работает с абстракциями над POSIX и Windows процессами из модуля `multiprocessing`.

### Модуль multiprocessing
Модуль `multiprocessing` содержит [все необходимое](https://docs.python.org/3/library/multiprocessing.html) для управления процессами, создания пулов, синхронизации и передачи данных между процессами.

Прежде чем переходить к примерам, обговорим, что в скриптах, порождающих новые процессы через `multiprocessing`, **должен присутствовать** блок `if __name__ == "__main__"`. Для чего в принципе нужен этот блок, мы [разбирали](/courses/python/chapters/python_chapter_0200#block-if-main) в главе про модули. 

Применительно к `multiprocessing` корректный импорт `__main__` модуля нужен, чтобы при старте нового экземпляра интерпретатора не возникало побочных эффектов, таких как порождение лишних процессов. Точка входа `if __name__ == "__main__"` позволяет этого избежать.

Итак, работа с пулом процессов в модуле `multiprocessing` выглядит следующим образом:

```python  {.example_for_playground}
from multiprocessing import Pool

if __name__ == '__main__':
    with Pool(3) as p:
        print(p.map(abs, [-2, 8, -3, 0]))
```
```
[2, 8, 3, 0]
```

Запуск и ожидание завершения отдельного процесса:

```python  {.example_for_playground}
from multiprocessing import Process

if __name__ == '__main__':
    p = Process(target=print, args=("text",))
    p.start()
    p.join()
```
```
text
```

Передача данных между двумя процессами через очередь, которую безопасно использовать из разных потоков и процессов:

```python  {.example_for_playground}
from multiprocessing import Process, Queue

def f(q, x):
    q.put([x*2, x*3])

if __name__ == '__main__':
    q = Queue()
    p = Process(target=f, args=(q, 3))
    p.start()
    print(q.get())
    p.join()
```
```
[6, 9]
```

Передача данных между процессами через пайп (дуплексный канал, читать и писать в который могут оба процесса):

```python  {.example_for_playground}
from multiprocessing import Process, Pipe

def f(conn):
    while True:
        x = conn.recv()
        if x is None:
            return

        conn.send(x*2)

if __name__ == '__main__':
    conn_parent, conn_child = Pipe()
    p = Process(target=f, args=(conn_child, ))
    p.start()

    for val in [3, 4, 5]:
        conn_parent.send(val)
        print(conn_parent.recv())
    
    conn_parent.send(None)
    p.join()
```
```
6
8
10
```

Примитив синхронизации `Lock` нужен для блокирования какого-либо ресурса, чтобы в любой момент времени с ним работал только один процесс. Пример использования `Lock` для блокирования консольного вывода:

```python  {.example_for_playground}
import random
import time
from multiprocessing import Process, Lock

def f(lock, i):
    time.sleep(random.randint(0, 6))

    lock.acquire()

    try:
        print("Process #", i)
    finally:
        lock.release()

if __name__ == '__main__':
    lock = Lock()

    for i in range(1, 6):
        Process(target=f, args=(lock, i)).start()
```
```
Process # 2
Process # 4
Process # 5
Process # 3
Process # 1
```

Вместо вызова методов `acquire()` и `release()` удобнее использовать блокировку через контекстный менеджер `with`.

Функции `f()` и `g()` захватывают блокировку, чтобы под ней выводить текст в консоль. Но в коде допущена ошибка, которая приводит к [взаимной блокировке](https://ru.wikipedia.org/wiki/%D0%92%D0%B7%D0%B0%D0%B8%D0%BC%D0%BD%D0%B0%D1%8F_%D0%B1%D0%BB%D0%BE%D0%BA%D0%B8%D1%80%D0%BE%D0%B2%D0%BA%D0%B0) (deadlock). Нужно ее исправить. {.task_text}

```python {.task_source #python_chapter_0300_task_0020}
from multiprocessing import Process, Lock
 
def f(lock):
    with lock:
        print("f() acquired lock")
 
def g(lock):
    with lock:
        print("g() acquired lock")
        f(lock)
 
if __name__ == '__main__':
    lock = Lock()
    p = Process(target=g, args=(lock, ))
    p.start()
    p.join()
```
Взаимная блокировка происходит из-за того, что `f()` не может дождаться освобождения лока, захваченного вызвавшей ее функцией `g()`. {.task_hint}
```python {.task_answer}
from multiprocessing import Process, Lock
 
def f(lock):
    with lock:
        print("f() acquired lock")

def g(lock):
    with lock:
        print("g() acquired lock")
    f(lock)

if __name__ == '__main__':
    lock = Lock()
    p = Process(target=g, args=(lock, ))
    p.start()
    p.join()
```

Для обмена данными между процессами можно использовать разделяемую память (shared memory). Поверх нее в модуле `multiprocessing` реализованы классы `Value` и `Array`. Останавливаться на них мы не будем, потому что в промышленной разработке их почти не используют. Авторы библиотеки `multiprocessing` по этому поводу [дают рекомендации:](https://docs.python.org/3/library/multiprocessing.html#multiprocessing-programming)
- Для обмена данными между процессами предпочтительно использовать пайпы и очереди.
- Более низкоуровневых примитивов лучше избегать. 

Более того, существует негласное правило. Если какая-то задача требует сложной логики синхронизации между процессами, долгих CPU-bound вычислений и обмена между процессами большими объемов данных, то использовать питон для нее попросту не идиоматично. 

## Потоки
Классы для работы с потоками почти во всем схожи с соответствующими классами для процессов:
- Класс `ThreadPoolExecutor` из модуля `concurrent.futures` предназначен для асинхронного запуска задач на пуле потоков.
- Модуль `threading` имеет более низкоуровневый интерфейс аналогично модулю `multiprocessing` для процессов.

`ThreadPoolExecutor`, как и `ProcessPoolExecutor`, наследован от абстрактного класса `Executor` и имплементирует его методы: `submit()`, `map()`, `shutdown()`. Не будем останавливаться на них повторно.

В модуле `threading` среди прочего определен класс `Thread` с интерфейсом, схожим с интерфейсом класса `multiprocessing.Process`, а также примитив `Lock` с интерфейсом, эквивалентным `multiprocessing.Lock`.

В данном коде допущена ошибка, которая привела к взаимной блокировке. Нужно ее исправить, чтобы в консоль вывелись все сообщения из функции `f()`, запущенной на двух потоках. {.task_text}

```python {.task_source #python_chapter_0300_task_0030}
from time import sleep
from threading import Thread, Lock


def f(n, lock1, lock2):
    print(f"Thread {n} acquiring lock 1...")

    with lock1:
        sleep(1)
        print(f"Thread {n} acquiring lock 2...")
        
        with lock2:
            print(f"Thread {n} acquired 2 locks!")


lock_a = Lock()
lock_b = Lock()

t1 = Thread(target=f, args=(1, lock_a, lock_b))
t2 = Thread(target=f, args=(2, lock_b, lock_a))

t1.start()
t2.start()

t1.join()
t2.join()

```
В конструктор `Thread()` для объекта `t2` блокировки передаются в неправильном порядке. {.task_hint}
```python {.task_answer}
from time import sleep
from threading import Thread, Lock


def f(n, lock1, lock2):
    print(f"Thread {n} acquiring lock 1...")

    with lock1:
        sleep(1)
        print(f"Thread {n} acquiring lock 2...")
        
        with lock2:
            print(f"Thread {n} acquired 2 locks!")


lock_a = Lock()
lock_b = Lock()

t1 = Thread(target=f, args=(1, lock_a, lock_b))
t2 = Thread(target=f, args=(2, lock_a, lock_b))

t1.start()
t2.start()

t1.join()
t2.join()
```

Кроме того, модуль `threading` содержит:
- Класс `RLock` для реентерабельных блокировок.
- Условные переменные `Condition` для ожидания наступления какого-то события и уведомления о его наступлении.
- Семафоры `Semaphore` для захвата и освобождения блокировки со счетчиком.
- События `Event` для ожидания сигнала о наступлении события и триггера этого сигнала.
- Таймеры `Timer` для отложенного выполнения кода.
- Барьерные секции `Barrier` для синхронизации выполнения блоков кода.

Как правило, эти примитивы используются в библиотеках для реализации потоко-безопасных структур данных: защищенных очередей, брокеров сообщений и т.д. В промышленной разработке чаще всего в связке с потоками используются только блокировки и защищенные очереди. Если требуется организовать совместную работу с данными, в игру вступают внешние по отношению к проекту сущности. Например, [Redis,](https://redis.io/) [Celery,](https://docs.celeryq.dev/en/stable/) [Memcached.](https://memcached.org/)

В данном коде присутствует ошибка, приводящая к [состоянию гонки](https://ru.wikipedia.org/wiki/%D0%A1%D0%BE%D1%81%D1%82%D0%BE%D1%8F%D0%BD%D0%B8%D0%B5_%D0%B3%D0%BE%D0%BD%D0%BA%D0%B8) (race condition). Исправьте ее. {.task_text}

```python {.task_source #python_chapter_0300_task_0040}
from threading import Thread
from time import sleep

counter = 0


def increment(val):
    global counter

    local_counter = counter
    local_counter += val

    sleep(1)

    counter = local_counter
    print(f"{counter=}")


t1 = Thread(target=increment, args=(1,))
t2 = Thread(target=increment, args=(2,))

t1.start()
t2.start()

t1.join()
t2.join()
```
Чтобы устранить состояние гонки, защитите данные с помощью блокировки. {.task_hint}
```python {.task_answer}
from threading import Thread, Lock
from time import sleep

counter = 0


def increment(val, lock):
    with lock:
        global counter

        local_counter = counter
        local_counter += val

        sleep(1)

        counter = local_counter
        print(f"{counter=}")


lock = Lock()
t1 = Thread(target=increment, args=(1, lock, ))
t2 = Thread(target=increment, args=(2, lock, ))

t1.start()
t2.start()

t1.join()
t2.join()
```

## Резюмируем
- Модуль `concurrent.futures` предназначен для высокоуровневого распределения задач по пулам процессов и потоков.
- Модуль `multiprocessing` предоставляет более низкоуровневый интерфейс для работы с процессами.
- В модуле `threading` есть все необходимое для низкоуровневой работы с потоками.
