# Глава 6. Итераторы

Стандартная библиотека C++ содержит три важных компонента:
- Контейнеры, реализующие различные структуры данных.
- Алгоритмы для работы с контейнерами: сортировка, поиск и многое другое.
- Итераторы — связующее звено между контейнерами и алгоритмами.

Начнем с итераторов. А в следующих главах взглянем на многообразие контейнеров и алгоритмов стандартной библиотеки.

## Что такое итератор

**Итератор** (iterator) — это абстракция для доступа к элементам контейнера. Через объект итератора можно обходить элементы в цикле или работать с ними поштучно.

Итератор — это распространенный [паттерн проектирования.](https://ru.wikipedia.org/wiki/%D0%98%D1%82%D0%B5%D1%80%D0%B0%D1%82%D0%BE%D1%80_(%D1%88%D0%B0%D0%B1%D0%BB%D0%BE%D0%BD_%D0%BF%D1%80%D0%BE%D0%B5%D0%BA%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F)) На уровне стандартной библиотеки он реализован и в других языках, например в C# и Rust.

Какую задачу решает паттерн «итератор»? Представьте, что вы автор стандартной библиотеки C++. Вы создали набор контейнеров, среди которых массивы, хеш-таблицы, списки, очереди. Но контейнеров самих по себе недостаточно. Нужно предоставить алгоритмы для работы с ними: поиск, сортировку, слияние, фильтрацию и другие функции.

Как это сделать? Можно каждый из алгоритмов превратить в метод класса контейнера.

Но у такого подхода есть недостатки:
- Перегруженный интерфейс класса. Придется добавлять сотни публичных методов!
- Дублирование одних и тех же алгоритмов для разных контейнеров.
- Плохая масштабируемость. Чтобы добавить алгоритм, придется дополнить все контейнеры новым методом.
- Нет разграничения ответственности между кодом, реализующим структуру данных, и кодом алгоритма для обработки данных. Зачем классу `std::string` знать, как устроен бинарный поиск?

Альтернативный подход заключается в использовании итераторов. Каждому классу контейнера соответствует свой класс итератора. Например, итератор по массиву или итератор по списку. Объект итератора имеет доступ к элементам контейнера и умеет их перебирать. Алгоритмы для работы с контейнерами оформляются в виде свободных функций, которые принимают на вход итераторы.

Это дает преимущества:
- Интерфейс класса контейнера содержит только самое необходимое. Таким классом удобно пользоваться.
- Отсутствует дублирование кода.
- Гибкость. Алгоритмы можно применять к контейнеру целиком или к диапазону элементов.
- Разделение обязанностей между кодом класса контейнера и кодом функции, реализующей алгоритм. Такое разделение позволяет развивать контейнеры и алгоритмы независимо друг от друга.

Посмотрим, как выглядит использование связки контейнеров, алгоритмов и итераторов. Заведем строку `s` и динамический массив `v`. Оба контейнера имеют методы `begin()` и `end()`. Они возвращают итераторы на первый элемент и на позицию за последним элементом. Вызовем [функцию std::reverse(),](https://en.cppreference.com/w/cpp/algorithm/reverse) которая меняет порядок элементов на обратный. Она принимает итераторы на начало и конец диапазона, который нужно «перевернуть»: {#block-vector}

```c++ {.example_for_playground .example_for_playground_001}
std::string s = "spam";
std::vector<int> v = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

std::reverse(s.begin(), s.end());
std::reverse(v.begin(), v.end());

std::println("Reversed.\nString: {}. Vector: {}", s, v);
```
```
Reversed.
String: maps. Vector: [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
```

Итераторы полезны и для написания собственных алгоритмов. Ведь они предоставляют гибкий и _единообразный_ доступ к элементам контейнеров разных типов. А значит, функции не надо знать, с элементами какого контейнера она работает.

## Реализация итераторов в C++

У каждого контейнера есть свой тип итератора. Тип итератора по строке — `std::string::iterator`, а итератор по массиву `std::vector<int>` — `std::vector<int>::iterator`. Здесь полное имя типа состоит из компонентов:
- `std` — пространство имен. 
- `vector` — класс динамически изменяемого массива.
- `int` — аргумент шаблонного класса `vector`. Это тип элементов контейнера.
- `iterator` — класс, реализованный внутри другого класса. То есть вложенный в `vector`.

Тип и реализация у всех итераторов разные, но работать с контейнерами они позволяют одинаково.

Разберем это на примере цикла по строке. Вы успели познакомиться с двумя вариантами перебора строки: циклом `for` по индексам и циклом `range-for` по символам:

```c++  {.example_for_playground .example_for_playground_002}
std::string s = "string";

for (std::size_t i = 0; i < s.size(); ++i)
    std::println("{}", s[i]);

for (char c: s)
    std::println("{}", c);
```

Но в отличие от строки не каждый контейнер поддерживает доступ к элементам по индексам. А цикл `range-for` перебирает все элементы диапазона без возможности задания шага, начальных и конечных условий. Поэтому в ряде случаев необходим третий способ организации циклов — через итераторы.

Цикл через итераторы абсолютно одинаков для строк, массивов и _любых_ контейнеров:

```c++ {.example_for_playground .example_for_playground_003}
for(std::string::iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Методы `begin()` и `end()` есть у всех контейнеров стандартной библиотеки:
- `begin()` возвращает итератор, указывающий на начальный элемент.
- `end()` возвращает итератор на позицию **за** последним элементом.
 
![Итераторы по строке](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-6/illustrations/cpp/iterators.jpg) {.illustration}

## Основные действия над итераторами

Есть несколько **операторов,** которые применимы к итераторам **всех** контейнеров:
- `==`, `!=` — строгое сравнение на равенство и неравенство.
- `*` — оператор разыменования. Он выглядит, как оператор умножения, но имеет другой смысл. Он нужен для обращения к элементу, на который указывает итератор: `std::println("{}", *it)`. Итератор, возвращаемый методом `end()`, разыменовывать нельзя.
- `++` — перемещение к следующему элементу.

Но есть и такие операторы, которые реализованы только для итераторов **некоторых** контейнеров:
- `>`, `>=`, `<`, `<=` — сравнение «больше-меньше». Итератор больше другого, если он ближе к концу контейнера.
- `--` — перемещение к предыдущему элементу: `--it`.
- `+`, `-`, `+=`, `-=` — перемещение на заданное количество элементов: `it - 5` означает получение итератора на 5 элементов ближе к началу контейнера, а `it += 2` — сдвиг `it` на 2 элемента к концу.

Почему эти операторы поддерживаются не везде? Дело в специфике контейнера. Например, каждый элемент односвязного списка ссылается лишь на следующий элемент и не обладает информацией о предыдущем. Поэтому к итератору контейнера `std::forward_list` применимы только операторы `==`, `!=`, `*` и `++`.

В зависимости от того, какие над итератором допустимы действия, итератор относится к одной из категорий: {#block-iterator-categories}
- чтение значений элементов (input),
- изменение значений (output),
- прямое итерирование (forward),
- двунаправленное итерирование (bidirectional),
- произвольный доступ к любым элементам (random access).

Чтобы показать, как эти категории соотносятся друг с другом, их можно представить вложенными друг в друга. Итераторы, изображенные внешними, обладают наибольшими возможностями.

![Категории итераторов](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-6/illustrations/cpp/iterator_categories.jpg) {.illustration}

Итераторы по каждому из стандартных контейнеров относятся к одной из этих категорий. Например, итераторы по строке являются итераторами произвольного доступа.
Так выглядит изменение символов строки в цикле по итераторам:

```c++  {.example_for_playground .example_for_playground_004}
std::string s = "iteration over string";

for(std::string::iterator it = s.begin(); it != s.end(); ++it)
{
    if (*it == ' ')
        *it = '_';
}

std::println("{}", s);
```
```
iteration_over_string
```

Перебирать контейнеры можно и с помощью цикла `while`:

```c++   {.example_for_playground .example_for_playground_018}
std::string card = "MasterCard N:5200 8282 8282 8210";
std::size_t hide_count = 12;

std::string::iterator it = card.begin();
while (it != card.end() && hide_count > 0)
{
    if (*it >= '0' && *it <= '9')
    {
        *it = '*';
        --hide_count;
    }
    ++it;
}
std::println("{}", card);
```
```
MasterCard N:**** **** **** 8210
```

Напишите функцию `is_palindrome()`, которая принимает строку и определяет, является ли строка палиндромом. Палиндром — это строка, одинаково выглядящая в обоих направлениях. Например, `"eve"`, `"sum summus mus"`. Пустая строка палиндромом не считается. {.task_text}

В своем решении используйте итераторы. {.task_text}

```c++ {.task_source #cpp_chapter_0060_task_0020}
bool is_palindrome(std::string text)
{

}
```
Чтобы проверить, является ли строка палиндромом, нужно сравнить ее нулевой символ с последним, первый — с предпоследним и так до середины строки. {.task_hint}
```c++ {.task_answer}
bool is_palindrome(std::string text)
{
    if (text.empty())
        return false;

    std::string::iterator it = text.begin();
    std::string::iterator it_tail = text.end() - 1;
    const std::string::iterator it_end = text.begin() + text.size() / 2;

    while (it != it_end)
    {
        if (*it != *it_tail)
            return false;
        ++it;
        --it_tail;
    }

    return true;
}
```

Многие алгоритмы стандартной библиотеки используют итераторы в качестве параметров функции или возвращаемого значения.

Так, [std::find_if()](https://en.cppreference.com/w/cpp/algorithm/find) принимает итераторы на границы интересующего диапазона и предикат (функцию, возвращающую `true` либо `false`). Начальный элемент диапазона участвует в поиске, а последний — нет. Функция возвращает итератор на первый элемент внутри диапазона, для которого предикат вернул `true`. Если такого элемента нет, функция возвращает итератор на последний элемент диапазона. {#block-find-if}

А [std::distance()](https://en.cppreference.com/w/cpp/iterator/distance) принимает итераторы на границы диапазона. И возвращает, сколько элементов расположено между этими границами, включая начальный элемент диапазона и не включая последний. {#block-distance}

```c++ {.example_for_playground}
import std;

// Параметром шаблона может быть литерал простого типа,
// в данном случае любой ASCII символ
template<char Sym>
bool expected(char c)
{
    return c == Sym;
}

// Параметр шаблона - функция-предикат
template <typename Pred>
void print_distance(std::string str, Pred pred)
{
    const std::string::iterator it =
        std::find_if(str.begin(), str.end(), pred);

    if (it == str.end())
    {
        std::println("a character cannot be found by predicate");
    }
    else
    {
        const auto d = std::distance(str.begin(), it);
        std::println("distance to '{}' is {}", *it, d);
    }
}

int main()
{
    std::string menu_item = "FAQ";
    print_distance(menu_item, expected<'F'>);
    print_distance(menu_item, expected<'A'>);
    print_distance(menu_item, expected<'Q'>);
    print_distance(menu_item, expected<'X'>);
}
```
```
distance to 'F' is 0
distance to 'A' is 1
distance to 'Q' is 2
a character cannot be found by predicate
```

Важно знать, что `std::distance()` может вернуть _отрицательное_ значение. Это допустимо, если переданы итераторы произвольного доступа (random access), а итератор на начало диапазона достижим из итератора на конец:

```c++
std::string menu_item = "FAQ";
std::println("{}", std::distance(menu_item.end(), menu_item.begin()));
```
```
-3
```

Напишите шаблонную функцию `index_of()`. Функция принимает строку и предикат. Тип предиката является параметром шаблона. Функция возвращает индекс первого символа, для которого предикат вернул `true`. Если такого символа нет, функция должна бросить исключение `std::runtime_error`. {.task_text}

В своем решении используйте алгоритмы `std::find_if()` и `std::distance()`. {.task_text}

```c++ {.task_source #cpp_chapter_0060_task_0010}
```
У шаблона единственный параметр — тип предиката. Назовем его `Fn`. Тогда функция будет выглядеть так: `template<class Fn> std::size_t index_of(std::string s, Fn pred)`. Внутри функции нужно вызвать `std::find_if()` от итераторов на начало и конец строки и предиката `pred`. Если итератор, который вернет `std::find_if()`, равен `s.end()`, нужно бросить исключение. Иначе вернуть расстояние от начала строки до этого итератора. Для этого вызовите функцию `std::distance()`. {.task_hint}
```c++ {.task_answer}
template<class Fn>
std::size_t index_of(std::string s, Fn pred)
{
    std::string::iterator it = std::find_if(
                                s.begin(), 
                                s.end(), 
                                pred);
    if (it == s.end())
        throw std::runtime_error("not found");

    return std::distance(s.begin(), it);
}
```

## Константные итераторы

Взгляните на этот код. Он не скомпилируется. Удостоверьтесь в этом, запустив его в плэйграунде.

```c++  {.example_for_playground .example_for_playground_005}
const std::string s = "string";

for(std::string::iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Дело в том, что строка `s` константная. А для константных контейнеров методы `begin()` и `end()` вместо обычного итератора возвращают тип `const_iterator`. Через него элементы доступны только на чтение.

Исправим ошибку компиляции заменой типа итератора `it`:

```c++  {.example_for_playground .example_for_playground_006}
for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Как это работает? Почему метод `begin()` возвращает итераторы разных типов? Все просто: методы класса можно перегружать. Причем перегрузка возможна не только по уникальному набору параметров, но и по квалификатору `const`, который относится целиком к методу. Методы объявляют константными, если внутри них не изменяются поля класса. У метода `begin()` класса `vector` есть две перегрузки. И при вызове метода от константного объекта класса компилятор выбирает перегрузку, помеченную `const`:

```c++
template<typename T, typename Alloc>
class vector
{
    // ...

    iterator begin()
    {
        return iterator(this->_M_impl._M_start);
    }

    const_iterator begin() const 
    { 
        return const_iterator(this->_M_impl._M_start);
    }

    // ...
};
```

Даже если сам контейнер не константный, зачастую безопаснее работать с ним через константные итераторы. Для получения константных итераторов даже от неконстантного контейнера предусмотрены методы `cbegin()` и `cend()`. Используйте их в случаях, не требующих изменения элементов.  Это поможет:
- Исключить случайную модификацию контейнера.
- Подчеркнуть намерение только читать значения. Код станет яснее.

```c++  {.example_for_playground .example_for_playground_007}
std::string s = "string"; // Не константная строка

for(std::string::const_iterator it = s.cbegin(); it != s.cend(); ++it)
    std::println("{}", *it);
```

В C++ есть квалификатор типа `const`, делающий объект иммутабельным (неизменяемым). Напрашивается вопрос: зачем потребовался тип `std::string::const_iterator`, если можно написать так: `const std::string::iterator it`?

Между константой `const std::string::iterator` и классом константного итератора `std::string::const_iterator` существует принципиальная разница.

Как и любую константу, константный итератор нельзя менять:

```c++  {.example_for_playground .example_for_playground_008}
const std::string::iterator it = s.begin();
++it; // Упс!
```

Зато можно менять значение объекта, на который он _указывает_. Ведь сам итератор при этом не меняется. Меняется значение элемента контейнера.

```c++   {.example_for_playground .example_for_playground_009}
const std::string::iterator it = s.begin();
*it = 'A'; // Ок
```

В случае с `const_iterator` ситуация обратная. Его можно менять:

```c++   {.example_for_playground .example_for_playground_010}
std::string::const_iterator it = s.cbegin();
++it; // Ок
```

Но через такой итератор нельзя модифицировать значение объекта, на который он указывает:

```c++   {.example_for_playground .example_for_playground_011}
std::string::const_iterator it = s.cbegin();
*it = 'A'; // Ошибка
```

И, конечно, в обоих случаях через итератор удастся прочитать значение элемента.

```c++  {.example_for_playground .example_for_playground_012}
const std::string::iterator c_it = s.begin();
std::string::const_iterator it_c = s.cbegin();

std::println("{} {}", *c_it, *it_c); // Ок
```

Через сочетание квалификатора `const` и типа `const_iterator` можно получить итератор, через который нельзя менять вообще ничего. Только читать:

```c++  {.example_for_playground .example_for_playground_013}
const std::string::const_iterator it = s.cbegin();
++it;      // Ошибка
*it = 'A'; // Ошибка

char c = *it; // Ок
```

## Обратные итераторы {#block-reverse-iterators}

При работе с некоторыми контейнерами может потребоваться доступ к элементам в обратном порядке. Для этого есть тип итератора `reverse_iterator`, а также методы контейнера `rbegin()` и `rend()`:
- `rbegin()` возвращает `reverse_iterator`, который указывает на последний элемент.
- `rend()` возвращает `reverse_iterator`, указывающий на позицию **перед** первым элементом. 

При инкременте `reverse_iterator` смещается ближе к началу контейнера.

![Обратные итераторы по строке](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-6/illustrations/cpp/reverse_iterators.jpg) {.illustration}

Так выглядит обратный проход по строке:

```c++   {.example_for_playground .example_for_playground_014}
std::string s = "string";

for(std::string::reverse_iterator it = s.rbegin(); it != s.rend(); ++it)
    std::println("{}", *it);
```

В этом примере мы не модифицировали элементы строки. Поэтому правильнее было бы заменить методы `rbegin()` и `rend()` на `crbegin()` и `crend()`, а тип `std::string::reverse_iterator` на `std::string::const_reverse_iterator`.

В функцию `hide_password()` приходит строка с логином и паролем вида `login:password`. Функция возвращает строку со скрытым паролем, в которой каждый символ пароля заменен на `'*'`. {.task_text}

Напишите тело этой функции с использованием обратных итераторов. {.task_text}

```c++ {.task_source #cpp_chapter_0060_task_0070}
std::string hide_password(std::string logpass)
{
    
}
```
С помощью обратных итераторов организуйте цикл по строке. Замените все символы на `'*'`, пока не достигните символа `':'`. {.task_hint}
```c++ {.task_answer}
std::string hide_password(std::string logpass)
{
    for (std::string::reverse_iterator it = logpass.rbegin();
        it != logpass.rend();
        ++it)
    {
        if (*it == ':')
            break;
        *it = '*';
    }
    return logpass;
}
```

Чтобы получить из обратного итератора обычный, предусмотрен метод `base()`. Он возвращает обычный итератор на элемент, который на одну позицию ближе к концу контейнера. Это нужно, чтобы итератор `rbegin()` можно было соотнести `begin()`, а `rend()` — итератору `end()`.

```c++  {.example_for_playground .example_for_playground_015}
std::string s = "string";

std::string::reverse_iterator rit = s.rbegin(); // g
++rit;                                          // n
std::string::iterator it = rit.base();          // g
```

Зачем нужны обратные итераторы, если можно просто применить декремент к обычному итератору `--it`? {#block-motivation-reverse-iterators}

Во-первых, обратные итераторы можно передавать в алгоритмы стандартной библиотеки. Например, для поиска элемента не с начала, а с конца функцией `std::find()`.

Во-вторых, через декремент не получится корректно проитерироваться в обратном порядке вплоть до самого первого элемента. Так писать нельзя:

```c++
std::string s = "string";

for(std::string::iterator it = s.end(); it != s.begin() - 1; --it)
    std::println("{}", *it);
```

Это приведет к обращению _за_ первый элемент контейнера `s.begin()`. Вы получите выход за границу контейнера.

Напишите тело функции `find_last()`, которая принимает пару _обратных_ итераторов на начало и конец диапазона строки и символ. Функция должна вернуть итератор типа `std::string::iterator` на _последнее_ вхождение символа в строку либо итератор на конец диапазона, если символ не найден. {.task_text}

```c++ {.task_source #cpp_chapter_0060_task_0030}
std::string::iterator find_last(std::string::reverse_iterator rbegin,
                                std::string::reverse_iterator rend,
                                char c)
{
    
}
```
С помощью обратных итераторов организуйте цикл по строке. Как только символ, на который указывает обратный итератор `rit`, совпадет с искомым, верните `(rit + 1).base()`. {.task_hint}
```c++ {.task_answer}
std::string::iterator find_last(std::string::reverse_iterator rbegin,
                                std::string::reverse_iterator rend,
                                char c)
{
    for(std::string::reverse_iterator rit = rbegin; rit != rend; ++rit)
    {
        if(*rit == c)
            return (rit + 1).base();
    }

    return rbegin.base();
}
```

Если на этом моменте вы почувствовали острое нежелание всякий раз писать длинные типы итераторов, то у нас хорошие новости. В C++ есть автоматический вывод типов!

## Ключевое слово auto

Если всегда указывать тип итератора, код выглядит громоздким:

```c++
for(std::string::iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Поэтому вместо типа зачастую пишут [ключевое слово auto.](https://en.cppreference.com/w/cpp/language/auto) Оно было введено в C++11 и позволяет компилятору самостоятельно определять тип переменной на основании того, как она инициализируется. Это называется автоматическим выводом типа.

```c++   {.example_for_playground .example_for_playground_016}
for(auto it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

## Инвалидация итераторов

В некоторых случаях итератор может перестать указывать туда, куда должен. Это называется [инвалидацией.](https://en.cppreference.com/w/cpp/container#Iterator_invalidation) В зависимости от типа контейнера к инвалидации итератора приводят разные причины. 

Одна из причин, по которой итератор по строке становится невалидным — это удаление символа, на который или после которого указывает итератор.

Заведем итератор на элемент строки. А затем удалим из строки все символы `'t'` с помощью функции [std::erase().](https://en.cppreference.com/w/cpp/container/vector/erase2) После этого итератор станет невалидным:

```c++  {.example_for_playground .example_for_playground_017}
std::string s = "iterator invalidation";
auto it = std::find(s.begin(), s.end(), 't');

std::erase(s, 't');      // Инвалидация

std::println("{}", *it); // Неопределенное поведение
```

Обращение к значению, на которое указывает невалидный итератор, в стандарте C++ относится к **неопределенному поведению** (UB, undefined behaviour).

Программа, в которой допущен UB, остается синтаксически корректной. Но она может вести себя непредсказуемо. Если не сразу, то при переносе с одной системы на другую, при смене компилятора или его версии. Непредсказуемое поведение может привести к чему угодно: к падению программы, странным ошибкам, повреждению данных, с которыми работает программа. {#block-ub}

Старайтесь не допускать в своем коде UB. А значит, будьте осторожны при работе с итераторами и следите, чтобы они не инвалидировались. В стандарте C++ [перечислены](https://timsong-cpp.github.io/cppwp/n4950/string.require#4) действия, которые потенциально могут привести к инвалидации итератора по строке:
- Передача строки по неконстантной ссылке в качестве аргумента функции из стандартной библиотеки.
- Вызов неконстантного метода строки кроме [некоторых](https://timsong-cpp.github.io/cppwp/n4950/string.require#4.2) методов вроде `begin()`, `end()`, `rbegin()`, `rend()`.

При работе со строкой к UB приводят и другие действия. Например:
- Разыменование итератора, возвращаемого методом `end()`: `*s.end()`.
- Разыменование итератора, указывающего перед первым элементом: `*(--s.begin())`.

Есть ли в этом коде UB? `Y/N`. {.task_text}

```c++
std::string title = "Discussion";
auto it = title.begin();
title = "";
```

```consoleoutput {.task_source #cpp_chapter_0060_task_0040}
```
Итератор `it` инвалидируется на 3-ей строке. Но обращения к нему не происходит, поэтому UB нет. {.task_hint}
```cpp {.task_answer}
N
```

Произойдет ли в этом коде инвалидация итератора? `Y/N`.  {.task_text}

[Метод строки erase()](https://en.cppreference.com/w/cpp/string/basic_string/erase) удаляет символ, на который указывает итератор, и _возвращает_ итератор на следующий за ним символ либо на `end()`, если удаленный символ был последним. {.task_text}

```c++
std::string text = "See also";

for (auto it = text.begin(); it != text.end(); ++it)
{
    if (*it == ' ')
        text.erase(it);
}
```

```consoleoutput {.task_source #cpp_chapter_0060_task_0050}
```
Цикл перебирает элементы от начала и до конца строки. В процессе некоторые элементы удаляются. Метод `erase()` удаляет элемент, на который указывает итератор. Соответственно итератор в этот момент инвалидируется. Метод возвращает итератор на следующий элемент, но в коде допущена ошибка: этот итератор никак не используется. {.task_hint}
```cpp {.task_answer}
Y
```

## Строка — это контейнер? {#block-string}

Все примеры использования итераторов и алгоритмов мы приводили применительно к типу `std::string`. Но считается ли строка полноценным контейнером?

Строки во многом схожи с контейнерами стандартной библиотеки. Но у них есть ограничения, о которых вы узнаете позже. Из-за этого строки иногда называют [псевдо-контейнерами](https://en.cppreference.com/w/cpp/container) (pseudo container).

Напишите функцию `rearrange_words()`. Она принимает строку, которая состоит из разделенных пробелами слов. Функция должна вернуть строку, содержащую те же слова, но в обратном порядке. Например, строка `not a bug` превратится в `bug a not`. {.task_text}

Строка начинается и заканчивается словом, а не пробелом. Одно слово отделяется от другого единственным пробелом. {.task_text}

Воспользуйтесь функциями [std::find()](https://en.cppreference.com/w/cpp/algorithm/find) и [std::reverse().](https://en.cppreference.com/w/cpp/algorithm/reverse)  {.task_text}

Эта задача имеет короткое и изящное решение. Если оно не приходит вам в голову, прочтите подсказку. {.task_text}

```c++ {.task_source #cpp_chapter_0060_task_0060}
std::string rearrange_words(std::string s)
{
}
```
Сначала разверните строку целиком. Строка `not a bug` превратится в `gub a ton`. Затем разверните каждое слово по отдельности: `bug a not`. {.task_hint}
```c++ {.task_answer}
std::string rearrange_words(std::string s)
{
    std::reverse(s.begin(), s.end());
    auto it_word = s.begin();
    
    while (true)
    {
        auto it_space = std::find(it_word, s.end(), ' ');
        std::reverse(it_word, it_space);
        if (it_space == s.end())
            break;

        it_word = it_space + 1;
    }

    return s;
}
```

----------

## Резюме

- Итератор — это абстракция для доступа к элементам контейнера.
- Итераторы позволяют единообразно работать с элементами контейнеров разных типов.
- Если доступ к элементам контейнера нужен только для чтения, используйте константный итератор.
- Для прохода по контейнеру в обратном порядке используйте обратные итераторы.
- Ключевое слово `auto` можно использовать для автоматического вывода типа переменной.
- Обращение к невалидному итератору — это UB (undefined behaviour).
