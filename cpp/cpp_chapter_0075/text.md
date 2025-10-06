# Глава 7.5. Адаптеры

Последовательные контейнеры реализуют плоские структуры данных, в которых нет иерархии или вложенности. Поверх плоских структур можно эффективно имплементировать несколько других структур данных. Это и есть адаптеры — обертки над последовательными контейнерами.


![Адаптеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_adapters.jpg) {.illustration}


## Очереди и стек

Класс [std::queue](https://en.cppreference.com/w/cpp/container/queue) — это структура данных [очередь](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)), в которой элементы добавляются с одного конца, а удаляются с другого.

```c++ {.example_for_playground .example_for_playground_023}
std::queue<int> orders;

orders.push(152);
orders.push(201);
orders.push(8);

orders.pop();

std::println("{} {}", orders.front(), orders.back());
```
```
201 8
```

Очередь может быть организована поверх нескольких последовательных контейнеров. По умолчанию она использует `std::deque`, но с помощью аргумента шаблона дек можно заменить на список:

```c++
std::queue<int, std::list<int>> orders;
```

Класс [std::priority_queue](https://en.cppreference.com/w/cpp/container/priority_queue) — это [очередь с приоритетами](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_%D1%81_%D0%BF%D1%80%D0%B8%D0%BE%D1%80%D0%B8%D1%82%D0%B5%D1%82%D0%BE%D0%BC_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)). Она поддерживает две операции: {#block-priority-queue}
- Добавить элемент.
- Извлечь элемент с максимальным приоритетом.

Приоритет элемента определяется оператором `<`, но это можно переопределить.

Очередь с приоритетами реализуется через [кучу (heap)](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) поверх массива.

Заведем очередь с приоритетами из задач на исполнение. Задача — это пара из приоритета типа `int` и названия типа `std::string`. Опертор `<` вначале сравнивает поля `first` двух объектов типа `std::pair`, затем — поля `second`. Для простой демонстрации работы контейнера нас это устраивает. 

```c++ {.example_for_playground}
import std;

void execute_task(std::pair<int, std::string> task)
{
    // https://en.cppreference.com/w/cpp/utility/format/spec
    std::println("[{:>5}] {}", task.first, task.second);
}

void process_tasks(std::vector<std::pair<int, std::string>> tasks)
{
    // Как и у всех контейнеров и адаптеров, у priority_queue
    // есть перегрузка конструктора, принимающая итераторы на
    // диапазон элементов нужного типа:
    std::priority_queue<std::pair<int, std::string>> queue = {
                                                    tasks.begin(),
                                                    tasks.end()
                                                    };
    while(queue.size() > 0)
    {
        execute_task(queue.top());
        queue.pop();
    }
}

int main()
{
    process_tasks({
        {-1, "free unused resources"},
        {100, "preprocess data 2"},
        {20, "merge results"},
        {40, "process data 2"},
        {-1000, "shutdown"},
        {-2, "free unused resources"},
        {41, "process data 1"},
        {1000, "initialize system"},
        {100, "preprocess data 1"},
    });
}
```
```
[ 1000] initialize system
[  100] preprocess data 2
[  100] preprocess data 1
[   41] process data 1
[   40] process data 2
[   20] merge results
[   -1] free unused resources
[   -2] free unused resources
[-1000] shutdown
```

Класс [std::stack](https://en.cppreference.com/w/cpp/container/stack) реализует [структуру данных стек](https://ru.wikipedia.org/wiki/%D0%A1%D1%82%D0%B5%D0%BA), в которой элементы добавляются и удаляются только с одного конца.

## flat-версии ассоциативных контейнеров {#block-flat}

Упорядоченные ассоциативные контейнеры оптимизированы для смешанных операций вставки, поиска и удаления. Очередность операций произвольна: вслед за добавлением элемента может идти еще одно добавление, удаление или поиск. Так работает кеширование.

Однако распространен и другой сценарий:
1. Вначале контейнер заполняется элементами.
2. Затем по ним осуществляется поиск.

Например, на старте сервиса подгружаются данные, а затем по ним требуется только поиск. Чем ближе друг к другу данные расположены в памяти, тем быстрее по ним поиск, потому что кеш процессора задействуется эффективнее. Класс `std::vector` — самый дружелюбный к кешу динамически расширяемый контейнер. Под него выделяется единая область памяти. На втором месте — `std::deque`, хранящийся в памяти непрерывными блоками.

Поверх этих двух классов и реализованы `flat`-версии контейнеров `map` и `set`: [std::flat_map](https://en.cppreference.com/w/cpp/container/flat_map), [std::flat_set](https://en.cppreference.com/w/cpp/container/flat_set), [std::flat_multimap](https://en.cppreference.com/w/cpp/container/flat_multimap), [std::flat_multiset](https://en.cppreference.com/w/cpp/container/flat_multiset).

Данные в них хранятся отсортированными, как и в упорядоченных ассоциативных контейнерах. Новые элементы вставляются на требуемую позицию, сдвигая существующие. При удалении оставшиеся элементы тоже сдвигаются. Поэтому вставка и удаление работают за линейное время `O(N)`. Как и в упорядоченных ассоциативных контейнерах, для поиска элементов используется бинарный поиск. Он имеет логарифмическую сложность `O(log(N))`. Но фактически за счет локальности данных поиск в flat-версиях работает быстрее, чем в обычных ассоциативных контейнерах.

Интерфейсы flat-версий контейнеров не отличаются от интерфейсов ассоциативных контейнеров.

## Алгоритмическая сложность работы с адаптерами


![Алгоритмическая сложность работы с адаптерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/container_adapters_algo_complexity.jpg) {.illustration}


Требуется добавлять и удалять элементы с обоих концов контейнера. Других операций не планируется. Какой контейнер или адаптер подойдет для этого лучше всего? {.task_text}

Напишите полное название вместе с неймспейсом. {.task_text}

Если вам интересен практический пример похожего сценария, то откройте подсказку. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0075_task_0070}
```
[Work stealing](https://en.wikipedia.org/wiki/Work_stealing) — это известная стратегия балансировки параллельных вычислений. Она описывает, как распределять потоки выполнения по ядрам процессора. Стратегия получила свое название из-за того, что простаивающие ядра перехватывают часть работы у загруженных. Каждому ядру соотносится дек потоков. Потоки могут удаляться с обоих концов дека. Стратегия work stealing реализована в асинхронном рантайме [Tokio](https://tokio.rs/) для Rust, фреймворке [Fork/Join](https://docs.oracle.com/javase/tutorial/essential/concurrency/forkjoin.html) для Java и библиотеке [Task Parallel Library](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/task-parallel-library-tpl) для .NET. {.task_hint}
```cpp {.task_answer}
std::deque
```

Требуется хранить список задач для исполнения в тред-пуле. Когда поток заканчивает задачу, он переходит к следующей с максимальным приоритетом. Задачи попадают в этот список с уже указанным приоритетом. Какой контейнер или адаптер подойдет лучше всего? {.task_text}

Напишите полное название вместе с неймспейсом. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0075_task_0080}
```
Лучше всего подойдет очередь с приоритетами. {.task_hint}
```cpp {.task_answer}
std::priority_queue
```

----------

## Резюме

В качестве резюме этой главы отлично подходит таблица контейнеров, адаптеров и их алгоритмической сложности. Держите ее под рукой, если будете готовиться к собеседованиям.


![Алгоритмическая сложность работы с контейнерами и адаптерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_algo_complexity_senjun_ru.jpg) {.illustration}