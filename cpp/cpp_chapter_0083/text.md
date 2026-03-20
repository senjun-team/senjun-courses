# Майлстоун. Вы познакомились с STL

Поздравляем, вы добрались до второго майлстоуна! Он поможет вам закрепить пройденный материал и проверить, насколько хорошо вы разобрались с итераторами, контейнерами и алгоритмами. Поехали!

## Разогрев

Является ли строка `std::string` контейнером? `Y/N`. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0083_task_0010}
```
Строку называют [псевдо-контейнером](https://en.cppreference.com/w/cpp/container) из-за ограничений на типы данных, которые она позволяет хранить. **Темы, затронутые в задаче:** [какие бывают контейнеры](/courses/cpp/chapters/cpp_chapter_0071/). {.task_hint}
```cpp {.task_answer}
n
```

Какая алгоритмическая сложность у вставки, удаления и поиска элемента в упорядоченном ассоциативном контейнере? Введите `O(1)`, если константная; `O(N)`, если линейная; `O(log(N))`, если логарифмическая. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0083_task_0020}
```
Элементы таких контейнеров всегда отсортированы. **Темы, затронутые в задаче:** [классификация контейнеров,](/courses/cpp/chapters/cpp_chapter_0071/#block-classification) [упорядоченные ассоциативные контейнеры](/courses/cpp/chapters/cpp_chapter_0073/) {.task_hint}
```cpp {.task_answer}
O(log(N))
```

На каких строках кода допущены ошибки? Перечислите их через пробел. Интересующие строки отмечены пронумерованными комментариями. {.task_text}

```cpp {.example_for_playground}
std::deque<std::size_t> session_ids = {57, 89, 22};

const auto it = session_ids.begin();
++it;     // 1
*it = 0;  // 2

auto cit = session_ids.cbegin();
++cit;    // 3
*cit = 0; // 4
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0030}
```
Итератор, объявленный с квалификатором `const` — это константа. Как и любую константу, такую переменную нельзя менять. Зато можно менять значение объекта, на который он указывает. Метод контейнера `cbegin()` возвращает итератор типа `const_iterator`. И с ним ситуация обратная. **Темы, затронутые в задаче:** [константные итераторы,](/courses/cpp/chapters/cpp_chapter_0062/#block-const) [ключевое слово auto](/courses/cpp/chapters/cpp_chapter_0062/#block-auto). {.task_hint}
```cpp {.task_answer}
1 4
```

## Джун фильтрует список

Что выведет этот код? В случае ошибки компиляции напишите `err`, а в случае неопределенного поведения — `ub`. {.task_text}

Считайте, что класс `Session` и функция `get_sessions()` реализованы в проекте. А в вектор `sessions` попадает 5 сессий, 3 из которых активны. {.task_text}

```cpp {.example_for_playground}
std::vector<Session> sessions = get_sessions();

for (auto it = sessions.begin(); it != sessions.end(); ++it)
{
    if (!it->is_active())
        sessions.erase(it);
}

std::println("{} sessions are currently active", sessions.size());
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0040}
```
После первого же вызова метода `erase()` все последующие итераторы становятся невалидными, так как элементы в памяти сдвигаются. Обращение к элементу, на который указывает невалидный итератор, является неопределённым поведением. **Темы, затронутые в задаче:** [инвалидация итераторов.](/courses/cpp/chapters/cpp_chapter_0062/#block-invalidation) {.task_hint}
```cpp {.task_answer}
ub
```

Многие новички в C++ пытались фильтровать контейнер, организовав цикл с итераторами и вызывая для итераторов на ненужные элементы метод `erase()`. Это приводит к тому, что элемент, на который указывает итератор, удаляется. А все последующие за ним элементы — смещаются. И мы получаем невалидный итератор, обращение по которому приведет к UB.

Нас, конечно, интересует не только факт наличия UB в коде, но и то, как правильно такой код исправить.

Миддл написал бы так:

```cpp
for (auto it = sessions.begin(); it != sessions.end(); )
{
    if (it->is_active())     // Инкрементируем it,
        ++it;                // только если ничего не удаляли  
    else
        it = sessions.erase(it); // Обновляем it
}
```

В этом варианте инкремент итератора происходит, если на текущей итерации цикла элемент не удаляется. А при удалении итератор обновляется значением, которое [возвращает](https://en.cppreference.com/w/cpp/container/vector/erase) метод `erase()`.

А вот синьор скорее всего вместо цикла выбрал бы алгоритм стандартной библиотеки:

```cpp
std::erase_if(sessions, needs_removal);
```

Функция [std::erase_if()](https://en.cppreference.com/w/cpp/container/vector/erase2) принимает ссылку на контейнер и предикат. Она удаляет все элементы контейнера, для которых предикат вернул `true`. Предикат `needs_removal()` мог бы выглядеть так:

```cpp
bool needs_removal(Session s)
{
    return !s.is_active();
}
```

## А std::remove() что-нибудь удаляет?

Что выведет этот код? {.task_text}

```cpp {.example_for_playground}
std::string args = "-j2 -g";

std::remove(args.begin(), args.end(), '-');

std::println("{}", args.size());
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0050}
```
Алгоритм [std::remove()](https://en.cppreference.com/w/cpp/algorithm/remove.html) перегруппировывает элементы в диапазоне, сохраняя их относительный порядок. Но он их не удаляет! **Темы, затронутые в задаче:** [алгоритмы для удаления элементов](/courses/cpp/chapters/cpp_chapter_0082/#block-removal). {.task_hint}
```cpp {.task_answer}
6
```

Алгоритм [std::remove()](https://en.cppreference.com/w/cpp/algorithm/remove.html) проходит по диапазону и сдвигает элементы, которые не надо удалять, в начало. Их относительный порядок сохраняется. Этот алгоритм возвращает итератор на получившийся диапазон. Элементы за его концом остаются в корректном состоянии, но их значения не определены. Размер контейнера при этом не меняется. Чтобы удалить элементы по-настоящему, нужно вспомнить про [идиому erase-remove:](/courses/cpp/chapters/cpp_chapter_0082/#block-erase-remove)

```cpp {.example_for_playground}
std::string args = "-j2 -g";

std::remove(args.begin(), args.end(), '-');

args.erase(std::remove(args.begin(), args.end(), '-'), args.end());

std::println("{}", args.size());
```
```
4
```

