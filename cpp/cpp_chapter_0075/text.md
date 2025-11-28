# Глава 7.5. Адаптеры

Последовательные контейнеры реализуют плоские структуры данных, в которых нет иерархии или вложенности. Поверх плоских структур можно эффективно имплементировать несколько других структур данных. Это и есть адаптеры — обертки над последовательными контейнерами. У каждого адаптера есть последовательный контейнер, используемый по умолчанию. Но можно выбрать и другой, чтобы добиться желаемой алгоритмической сложности операций.


![Адаптеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_adapters.jpg) {.illustration}

## Класс queue

Класс [std::queue](https://en.cppreference.com/w/cpp/container/queue) реализует [АТД «Очередь»](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)), в котором элементы добавляются с одного конца, а удаляются с другого.

```cpp {.example_for_playground .example_for_playground_023}
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


![Очередь](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/improve-chapters-cmake/illustrations/cpp/queue.jpg) {.illustration}


Очередь может быть организована поверх нескольких последовательных контейнеров. По умолчанию она использует `std::deque`, но через аргумент шаблона дек можно заменить на список:

```cpp
std::queue<int, std::list<int>> orders;
```

## Класс priority_queue

Класс [std::priority_queue](https://en.cppreference.com/w/cpp/container/priority_queue) — это реализация [АТД «Очередь с приоритетами»](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_%D1%81_%D0%BF%D1%80%D0%B8%D0%BE%D1%80%D0%B8%D1%82%D0%B5%D1%82%D0%BE%D0%BC_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)). Этот АТД поддерживает две операции: {#block-priority-queue}
- Добавить элемент.
- Извлечь элемент с максимальным приоритетом.

Приоритет элемента определяется оператором `<`, но это можно переопределить.

Класс `std::priority_queue` реализуется через [кучу (heap)](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) поверх массива.

```cpp
std::priority_queue<int> heap;
```


![Очередь с приоритетами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/improve-chapters-cmake/illustrations/cpp/priority_queue.jpg) {.illustration}



Заведем очередь с приоритетами из задач на исполнение. Задача — это пара из приоритета типа `int` и названия типа `std::string`. Оператор `<` вначале сравнивает поля `first` двух объектов типа `std::pair`, затем — поля `second`. Для простой демонстрации работы контейнера нас это устраивает. 

```cpp {.example_for_playground}
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

## Класс stack

Класс [std::stack](https://en.cppreference.com/w/cpp/container/stack) реализует [АТД «Стек».](https://ru.wikipedia.org/wiki/%D0%A1%D1%82%D0%B5%D0%BA) Этот АТД организован по принципу LIFO (Last-In, First-Out): элементы добавляются и удаляются только с одного конца. Этот конец называется вершиной стека. Последний добавленный элемент будет первым элементом для взятия с вершины.

Основные методы контейнера:
- `push()` — добавление элемента на вершину стека.
- `pop()` — удаление элемента с вершины.
- `top()` — доступ к значению элемента на вершине.
- `size()` — получение длины стека.
- `empty()` — проверка, пуст ли стек.


![Стек](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/improve-chapters-cmake/illustrations/cpp/stack.jpg) {.illustration}


Работу стека отлично иллюстрирует класс для хранения истории посещения страниц в браузере:

```cpp
import std;

class BrowserHistory
{
public:
    BrowserHistory()
    {
        std::println("Starting with empty stack of pages");
    }

    void visit_page(std::string page)
    {
        pages.push(page);
        std::println("Visiting page {}. Stack size: {}", 
                     page, pages.size());
    }

    void go_back()
    {
        if (!pages.empty())
            pages.pop();
        std::println("Going back from current page. Stack size: {}",
                      pages.size());
    }

    std::string current_page()
    {
        if (pages.empty())
            return "";
        
        std::println("Current page: {}", pages.top());
        return pages.top();
    }

private:
    std::stack<std::string> pages;
};

int main()
{
    BrowserHistory hist;
    hist.visit_page("github.com");
    hist.visit_page("duckduckgo.com");
    hist.current_page();
    hist.go_back();
    hist.current_page();
}
```
```
Starting with empty stack of pages
Visiting page github.com. Stack size: 1
Visiting page duckduckgo.com. Stack size: 2
Current page: duckduckgo.com
Going back from current page. Stack size: 1
Current page: github.com
```

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

```consoleoutput {.task_source #cpp_chapter_0075_task_0070}
```
Подойдет последовательный контейнер, который хранит данные в наборе массивов фиксированной длины. {.task_hint}
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
