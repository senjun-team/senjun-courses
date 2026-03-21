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

Какая алгоритмическая сложность у вставки, удаления и поиска элемента в упорядоченном ассоциативном контейнере? Введите `O(1)`, `O(N)` или `O(log(N))`. {.task_text}

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

## Пони из бездны

Что выведет этот код? {.task_text}

```cpp {.example_for_playground}
std::map<int, std::string> ponies;

ponies[5] = "Rumble";
ponies[9] = "Braeburn";

if (ponies[2] == "Snails")
    std::print("1 ");

std::print("{}", ponies.size());
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0040}
```
Обращение к элементу ассоциативного контейнера через `[]` создает ключ, если он отсутствует. К нему привязывается значение по умолчанию. **Темы, затронутые в задаче:** [доступ к элементу ассоциативного контейнера](/courses/cpp/chapters/cpp_chapter_0073/#block-key-creation). {.task_hint}
```cpp {.task_answer}
3
```

В словарях `std::map` и `std::unordered_map` обращение к элементу через `[]` не является read-only операцией! Если ключ не найден, он добавляется в контейнер вместе со значением по умолчанию. Поэтому у вас не получится применить `[]` к константному словарю:

```cpp
const std::map<int, std::string> ponies = {
    {5, "Rumble"},
    {9, "Braeburn"},
};


if (ponies[2] == "Snails") // Ошибка компиляции
    std::print("1 ");
```

Вывод: когда нужен read-only доступ, вместо `[]` вызывайте методы `find()` и `contains()`.

И второй вывод: используйте знание о работе `[]`, чтобы писать лаконичный код без лишних проверок:


```cpp
std::string text = "3 + 3 = 6";

std::unordered_map<char, std::size_t> char_count;

// Нам не надо проверять, содержится ли ключ в словаре
// Если его нет, то по нему создается значение по умолчанию 0,
// которое сразу же инкрементируется
for (char c: text)
    ++char_count[c]; 

std::println("{}", char_count);
```
```
{'6': 1, '+': 1, ' ': 4, '=': 1, '3': 2}
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

```consoleoutput {.task_source #cpp_chapter_0083_task_0050}
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

## У меня std::remove() сломался

Что выведет этот код? {.task_text}

```cpp {.example_for_playground}
std::string args = "-j2 -g";

std::remove(args.begin(), args.end(), '-');

std::println("{}", args.size());
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0060}
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

## Да кому нужны эти ваши требования к компараторам

Что выведет этот код? В случае ошибки компиляции напишите `err`, а в случае неопределенного поведения — `ub`. {.task_text}

Считайте, что класс `Session` и функция `get_sessions()` реализованы в проекте. А в вектор `sessions` попадает 5 сессий, 3 из которых активны. {.task_text}

```cpp {.example_for_playground}
import std;

bool greater_or_eq(int a, int b)
{
    return a >= b; 
}

int main()
{
    std::vector<int> diffs = {8, 1, 0, 3};

    std::sort(diffs.begin(), diffs.end(), greater_or_eq);

    for (int val : diffs)
        std::println("{} ", val);
}
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0070}
```
В функции `greater_or_eq()` нарушен строгий частичный порядок. **Темы, затронутые в задаче:** [требования к компараторам.](/courses/cpp/chapters/cpp_chapter_0082/#block-comparator-requirements) {.task_hint}
```cpp {.task_answer}
ub
```

В этом коде реализована попытка сортировать числа с проверкой пар на «больше или равно». Она нарушет [аксиому антисимметричности.](/courses/cpp/chapters/cpp_chapter_0082/#block-comparator-requirements) Для компаратора с именем `is_less()` она выглядит так: если `is_less(x, y)`, то `!is_less(y, x)`. А функция `greater_or_eq()` для одинаковых чисел `a` и `b` всегда вернет `true` вне зависимости от того, в каком порядке они переданы.

Нарушение этой аксиомы в компараторе, переданном алгоритму стандартной библиотеки — это UB. Чтобы исправить это, достаточно заменить проверку на более строгую: `a > b`.

## Такие одинаковые, но такие разные

Какой способ получения итератора на элемент эффективнее — через функцию `std::lower_bound()` или одноименный метод? Введите соответственно `1` или `2`. {.task_text}

```cpp {.example_for_playground}
std::set<int> numbers;

for (int i = 0; i <= 1000001; ++i)
    numbers.insert(i);

const int x = 1000000;
auto it_f = std::lower_bound(numbers.begin(), numbers.end(), x); // 1

auto it_m = numbers.lower_bound(x); // 2
```

```consoleoutput {.task_source #cpp_chapter_0083_task_0080}
```
Вспомните, почему у многих контейнеров есть методы, имена которых совпадают с именами алгоритмов стандартной библиотеки. **Темы, затронутые в задаче:** [класс std::set;](/courses/cpp/chapters/cpp_chapter_0073/#block-set) [алгоритмы для поиска в упорядоченном диапазоне;](/courses/cpp/chapters/cpp_chapter_0081/#block-bounds) [когда использовать алгоритмы, а когда — методы класса.](/courses/cpp/chapters/cpp_chapter_0082/#block-algo) {.task_hint}
```cpp {.task_answer}
2
```

Контейнер `std::set` хранит _упорядоченное_ множество уникальных ключей. А `lower_bound()` как раз нужен для поиска в отсортированном диапазоне: он возвращает итератор на первое значение, _не меньшее_ `x`. Для решения этой задачи отлично подходит алгоритм бинарного поиска. На каждой его итерации рассматриваемый диапазон сокращается в 2 раза: поиск работает за `O(log(N))`.

Однако алгоритм `std::lower_bound()` справится гораздо медленнее: за `O(N)`. Почему? В него передаются только итераторы на диапазон. Итераторы по `std::set` относятся к [категории](/courses/cpp/chapters/cpp_chapter_0061/#block-iterator-categories) двунаправленных (bidirectional): с их помощью можно перемещаться по контейнеру в любом направлению. Но они не являются итераторами произвольного доступа (random access): через них нельзя переместиться на произвольный элемент, как в случае с итераторами на `std::vector`. А это необходимо для реализации бинарного поиска. Поэтому функции `std::lower_bound()` не остается ничего другого, кроме как последовательно перебирать контейнер за `O(N)`.

Зато метод `lower_bound()` имеет полное представление о внутреннем устройстве контейнера, и решает задачу эффективнее — за `O(log(N))`.

Если вас интересуют конкретные цифры, откройте эту задачу в песочнице по кнопке с зонтиком и измерьте время получения обоих итераторов.


