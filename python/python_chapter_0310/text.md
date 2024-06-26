# Глава 31. Многозадачность, конкурентность и асинхронность
> Concurrency is Not Parallelism.  
Роб Пайк, один из авторов языка Go, кодировки UTF-8, операционных систем Plan 9 и Inferno.

Термины «многозадачность», «конкурентность» и «асинхронность» не являются специфичными для языка программирования. Это общие концепции computer science, в которых тем не менее путаются даже опытные разработчики. Мы детально разберем, что стоит за этими терминами. И начнем мы с реализации многозадачности в операционных системах.

## Многозадачность
Многозадачность — это возможность операционной системы выполнять несколько задач одновременно. Под задачей понимается процесс с программой. Многозадачность может достигаться за счет различных, но не взаимоисключающих подходов:
- Параллелизм. В единицу времени в ОС выполняется более одной задачи. 
- Переключение между задачами в одном потоке. В единицу времени выполняется только одна задача.

Можно одновременно слушать музыку, двигать курсором мышки и листать картинки. Иллюзия одновременного выполнения достигается благодаря планировщику задач. Именно он ставит задачи на выполнение. 

**Планировщик задач** — это абстракция в ядре операционной системы, которая управляет задачами. Он решает, что будет выполняться, а чье выполнение можно прервать. Смену одной задачи на другую еще называют переключением контекста.

### Вытесняющая многозадачность
Вытесняющая многозадачность (preemptive multitasking) — это вид многозадачности, при котором планировщик задач принудительно приостанавливает выполнение текущей задачи и отдает управление другой задаче, готовой для исполнения. Приостановка текущей задачи называется вытеснением. Существуют разные механизмы вытеснения:
- таймер,
- приоритет,
- прерывания.

При этом всем задачам достанется время на выполнение. Вытесняющая многозадачность используется во многих операционных системах, в том числе Linux, MacOs, Windows.

### Кооперативная многозадачность
При кооперативной многозадачности операционная система не инициирует передачу управления между задачами. Задачи самостоятельно отдают управление планировщику. А он решает, какая задача будет выполняться следующей.

Такой вид многозадачности использовался в версиях Windows 3.x, pre-X MacOS.

Этот подход не прижился в планировании задач для операционной системы. Зато нашел эффективное применение в языках программирования.

### Параллелизм
Параллелизм — это одновременное выполнение нескольких задач. Это форма многозадачности, при которой в единицу времени может выполняться более одной задачи. Реализуется на аппаратном уровне благодаря многоядерным процессорам. Максимальное количество задач для одновременного выполнения зависит от архитектуры процессора.

## Конкурентность
Конкурентность — это композиция независимо выполняющихся задач. Из названия термина понятно, что задачи за что-то конкурируют. Например, за право выполняться. Конкуренция начинается, когда ресурса перестает хватать всем, кто пытается получить к нему доступ. Ресурсом может выступать процессорное время, потребляемая память, устройства ввода-вывода. 

В питоне есть несколько механизмов для конкурентного выполнения кода:
- Выполнение кода на разных потоках. В прошлой главе мы [рассматривали](/courses/python/chapters/python_chapter_0300/) модуль `threading` для многопоточности.
- Запуск кода на разных процессах. В прошлой главе мы [обсуждали](/courses/python/chapters/python_chapter_0300/) модуль `multiprocessing` для распараллеливания кода по процессам.
- Кооперативная многозадачность с помощью генераторов и корутин.
- Фьючи из модуля `concurrent.futures`.

При конкурентном выполнении задачам необязательно знать друг о друге. В общем случае они друг от друга не зависят, кроме случая кооперативной 
многозадачности.

В питоне в качестве планировщика задач могут выступать:
- Класс `ThreadPoolExecutor`.
- Класс `ProcessPoolExecutor`.
- Планировщик внутри стандартной библиотеки. 

Планировщик внутри стандартной библиотеки вызывается неявно: когда мы пишем `yield`, планировщик в интерпретаторе определяет, какой корутине отдать выполнение. Работает он по принципу кооперативной многозадачности. Поэтому очень важно не запускать тяжелых блокирующих операций внутри корутин. Блокировка хотя бы одной задачи заблокирует передачу управления планировщику для выбора следующей задачи.

## Синхронность
При синхронном выполнении код исполняется линейно. Все шаги прослеживаются по коду. Синхронные вызовы функций еще называют блокирующими, потому что выполнение вызывающего кода не продолжится, пока функция не вернет управление. А пока этого не произошло, поток находится в режиме ожидания.

Однако тратить процессорное время на ожидание неэффективно. Когда это становится критично, от синхронного выполнения можно отказаться в пользу асинхронности.

## Асинхронность
В питоне асинхронный код чаще всего встречается при решении IO-bound задач. Наибольшую производительность можно получить именно на этом. Вместо блокирующего ожидания управление передается другой задаче, готовой выполниться.

Асинхронную  функцию можно запустить и сразу же получить управление обратно. Но нельзя точно сказать, когда асинхронная функция завершит или начнет свою работу. Можно лишь дождаться ее завершения, прервать ожидание или передать функции код для выполнения после завершения (коллбэк).

Рассмотрим пример запуска задач внутри `ThreadPoolExecutor` и ожидания их завершения.

```python  {.example_for_playground}
import concurrent.futures
import time


def worker1():
    print("worker1 begins")
    time.sleep(2)
    print("worker1 done")


def worker2():
    print("worker2 begins")
    time.sleep(1)
    print("worker2 done")


with concurrent.futures.ThreadPoolExecutor() as executor:
    f1 = executor.submit(worker1)
    f2 = executor.submit(worker2)
    print("asynchronous tasks started")
    concurrent.futures.wait([f1, f2])
```
```
worker1 begins
worker2 begins
asynchronous tasks started
worker2 done
worker1 done
```

Любой асинхронный код является конкурентным. Нарушается последовательное исполнение программы, а распределение задач становится прерогативой планировщика. В случае с корутинами задачи крутятся внутри цикла событий (event loop). 

## Цикл событий (event loop)
Циклом событий во многих языках программирования называют планировщик задач для корутин из пространства пользователя. 

Как работает цикл событий? Сначала задачи регистрируются внутри цикла событий. Затем начинается их выполнение. Цикл событий — это бесконечный цикл, который периодически проверяет готовность задач. Когда задача завершается или добровольно отдает управление, происходит уведомление цикла событий. А он продолжает работу, пока все задачи не завершатся. 

![Event loop](https://raw.githubusercontent.com/senjun-team/senjun-courses/main/illustrations/python/event-loop.webp) {.illustration}

Написание цикла событий — нетривиальная задача. С минимальной реализацией цикла событий можно ознакомиться вот в [этом примере.](https://github.com/senjun-team/senjun-courses/blob/main/examples/python/server_client.py) Но на практике вряд ли кому-то придется разрабатывать свой. В промышленной разработке, как правило, просто следует пользоваться стандартным циклом событий, который стоит за вызовами `async/await`. Об этом подробнее в следующей главе. 

## Резюмируем
- Многозадачность бывает вытесняющей и кооперативной. 
- Для управления исполнением асинхронных задач нужен планировщик.
- Планировщик внутри стандартной библиотеки питона используется неявно для управления работой корутин. Называется он циклом событий. 
- Цикл событий использует кооперативную многозадачность.
- Задачи внутри цикла событий должны самостоятельно отдавать управление.
- В питоне выгоднее всего применять асинхронность для IO-bound задач.
