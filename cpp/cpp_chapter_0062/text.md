# Глава 6.2. Работа с итераторами

В этой главе вы узнаете, как безопасно и удобно работать с итераторами.

## Константные итераторы

Взгляните на этот код. Он не скомпилируется. Удостоверьтесь в этом, запустив его в песочнице.

```cpp  {.example_for_playground .example_for_playground_005}
const std::string s = "string";

for(std::string::iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Дело в том, что строка `s` константная. А для константных контейнеров методы `begin()` и `end()` вместо обычного итератора возвращают тип `const_iterator`. Через него элементы доступны только на чтение.

Исправим ошибку компиляции заменой типа итератора `it`:

```cpp  {.example_for_playground .example_for_playground_006}
for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Как это работает? Почему метод `begin()` возвращает итераторы разных типов? Все просто: методы класса можно перегружать. Причем перегрузка возможна не только по уникальному набору параметров, но и по квалификатору `const`, который относится целиком к методу. Методы объявляют константными, если внутри них не изменяются поля класса. У метода `begin()` класса `vector` есть две перегрузки. И при вызове метода от константного объекта класса компилятор выбирает перегрузку, помеченную `const`:

```cpp
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

```cpp  {.example_for_playground .example_for_playground_007}
std::string s = "string"; // Не константная строка

for(std::string::const_iterator it = s.cbegin(); it != s.cend(); ++it)
    std::println("{}", *it);
```

В C++ есть квалификатор типа `const`, делающий объект иммутабельным (неизменяемым). Напрашивается вопрос: зачем потребовался тип `std::string::const_iterator`, если можно написать так: `const std::string::iterator it`?

Между константой `const std::string::iterator` и классом константного итератора `std::string::const_iterator` существует принципиальная разница.

Как и любую константу, константный итератор нельзя менять:

```cpp  {.example_for_playground .example_for_playground_008}
const std::string::iterator it = s.begin();
++it; // Упс!
```

Зато можно менять значение объекта, на который он _указывает_. Ведь сам итератор при этом не меняется. Меняется значение элемента контейнера.

```cpp   {.example_for_playground .example_for_playground_009}
const std::string::iterator it = s.begin();
*it = 'A'; // Ок
```

В случае с `const_iterator` ситуация обратная. Его можно менять:

```cpp   {.example_for_playground .example_for_playground_010}
std::string::const_iterator it = s.cbegin();
++it; // Ок
```

Но через такой итератор нельзя модифицировать значение объекта, на который он указывает:

```cpp   {.example_for_playground .example_for_playground_011}
std::string::const_iterator it = s.cbegin();
*it = 'A'; // Ошибка
```

И, конечно, в обоих случаях через итератор удастся прочитать значение элемента.

```cpp  {.example_for_playground .example_for_playground_012}
const std::string::iterator c_it = s.begin();
std::string::const_iterator it_c = s.cbegin();

std::println("{} {}", *c_it, *it_c); // Ок
```

Через сочетание квалификатора `const` и типа `const_iterator` можно получить итератор, через который нельзя менять вообще ничего. Только читать:

```cpp  {.example_for_playground .example_for_playground_013}
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

```cpp   {.example_for_playground .example_for_playground_014}
std::string s = "string";

for(std::string::reverse_iterator it = s.rbegin(); it != s.rend(); ++it)
    std::println("{}", *it);
```

В этом примере мы не модифицировали элементы строки. Поэтому правильнее было бы заменить методы `rbegin()` и `rend()` на `crbegin()` и `crend()`, а тип `std::string::reverse_iterator` на `std::string::const_reverse_iterator`.

В функцию `hide_password()` приходит строка с логином и паролем вида `login:password`. Функция возвращает строку со скрытым паролем, в которой каждый символ пароля заменён на `'*'`. {.task_text}

Напишите тело этой функции с использованием обратных итераторов. {.task_text}

```cpp {.task_source #cpp_chapter_0062_task_0070}
std::string hide_password(std::string logpass)
{
    
}
```
С помощью обратных итераторов организуйте цикл по строке. Замените все символы на `'*'`, пока не достигните символа `':'`. {.task_hint}
```cpp {.task_answer}
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

```cpp  {.example_for_playground .example_for_playground_015}
std::string s = "string";

std::string::reverse_iterator rit = s.rbegin(); // g
++rit;                                          // n
std::string::iterator it = rit.base();          // g
```

Зачем нужны обратные итераторы, если можно просто применить декремент к обычному итератору `--it`? {#block-motivation-reverse-iterators}

Во-первых, обратные итераторы можно передавать в алгоритмы стандартной библиотеки. Например, для поиска элемента не с начала, а с конца функцией `std::find()`.

Во-вторых, через декремент не получится корректно проитерироваться в обратном порядке вплоть до самого первого элемента. Так писать нельзя:

```cpp
std::string s = "string";

for(std::string::iterator it = s.end(); it != s.begin() - 1; --it)
    std::println("{}", *it);
```

Это приведёт к обращению _за_ первый элемент контейнера `s.begin()`. Вы получите выход за границу контейнера.

Напишите тело функции `find_last()`, которая принимает пару _обратных_ итераторов на начало и конец диапазона строки и символ. Функция должна вернуть итератор типа `std::string::iterator` на _последнее_ вхождение символа в строку либо итератор на конец диапазона, если символ не найден. {.task_text}

```cpp {.task_source #cpp_chapter_0062_task_0030}
std::string::iterator find_last(std::string::reverse_iterator rbegin,
                                std::string::reverse_iterator rend,
                                char c)
{
    
}
```
С помощью обратных итераторов организуйте цикл по строке. Как только символ, на который указывает обратный итератор `rit`, совпадёт с искомым, верните `(rit + 1).base()`. {.task_hint}
```cpp {.task_answer}
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

```cpp
for(std::string::iterator it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

Поэтому вместо типа зачастую пишут [ключевое слово auto.](https://en.cppreference.com/w/cpp/language/auto) Оно было введено в C++11 и позволяет компилятору самостоятельно определять тип переменной на основании того, как она инициализируется. Это называется автоматическим выводом типа.

```cpp   {.example_for_playground .example_for_playground_016}
for(auto it = s.begin(); it != s.end(); ++it)
    std::println("{}", *it);
```

## Инвалидация итераторов {#block-invalidation}

В некоторых случаях итератор может перестать указывать туда, куда должен. Это называется [инвалидацией.](https://en.cppreference.com/w/cpp/container#Iterator_invalidation) В зависимости от типа контейнера к инвалидации итератора приводят разные причины. 

Одна из причин, по которой итератор по строке становится невалидным — это удаление символа, на который или после которого указывает итератор.

Заведем итератор на элемент строки. А затем удалим из строки все символы `'t'` с помощью функции [std::erase().](https://en.cppreference.com/w/cpp/container/vector/erase2) После этого итератор станет невалидным:

```cpp  {.example_for_playground .example_for_playground_017}
std::string s = "iterator invalidation";
auto it = std::find(s.begin(), s.end(), 't');

std::erase(s, 't');      // Инвалидация

std::println("{}", *it); // Неопределенное поведение
```

Обращение к значению, на которое указывает невалидный итератор, в стандарте C++ относится к **неопределённому поведению** (UB, undefined behaviour). Программа, в которой допущен UB, остаётся синтаксически корректной. Но она может вести себя непредсказуемо. Если не сразу, то при переносе с одной системы на другую, при смене компилятора или его версии. Непредсказуемое поведение может привести к чему угодно: к падению программы, странным ошибкам, повреждению данных, с которыми работает программа. {#block-ub}

Старайтесь не допускать в своём коде UB. А значит, будьте осторожны при работе с итераторами и следите, чтобы они не инвалидировались. В стандарте C++ [перечислены](https://timsong-cpp.github.io/cppwp/n4950/string.require#4) действия, которые потенциально могут привести к инвалидации итератора по строке:
- Передача строки по неконстантной ссылке в качестве аргумента функции из стандартной библиотеки.
- Вызов неконстантного метода строки кроме [некоторых](https://timsong-cpp.github.io/cppwp/n4950/string.require#4.2) методов вроде `begin()`, `end()`, `rbegin()`, `rend()`.

При работе со строкой к UB приводят и другие действия. Например:
- Разыменование итератора, возвращаемого методом `end()`: `*s.end()`.
- Разыменование итератора, указывающего перед первым элементом: `*(--s.begin())`.

Есть ли в этом коде UB? `Y/N`. {.task_text}

```cpp
std::string title = "Discussion";
auto it = title.begin();
title = "";
```

```consoleoutput {.task_source #cpp_chapter_0062_task_0040}
```
Итератор `it` инвалидируется на 3-ей строке. Но обращения к нему не происходит, поэтому UB нет. {.task_hint}
```cpp {.task_answer}
N
```

Произойдет ли в этом коде инвалидация итератора? `Y/N`.  {.task_text}

[Метод строки erase()](https://en.cppreference.com/w/cpp/string/basic_string/erase) удаляет символ, на который указывает итератор, и _возвращает_ итератор на следующий за ним символ либо на `end()`, если удалённый символ был последним. {.task_text}

```cpp
std::string text = "See also";

for (auto it = text.begin(); it != text.end(); ++it)
{
    if (*it == ' ')
        text.erase(it);
}
```

```consoleoutput {.task_source #cpp_chapter_0062_task_0050}
```
Цикл перебирает элементы от начала и до конца строки. В процессе некоторые элементы удаляются. Метод `erase()` удаляет элемент, на который указывает итератор. Соответственно итератор в этот момент инвалидируется. Метод возвращает итератор на следующий элемент, но в коде допущена ошибка: этот итератор никак не используется. {.task_hint}
```cpp {.task_answer}
Y
```

----------

## Резюме

- Если доступ к элементам контейнера нужен только для чтения, используйте константный итератор.
- Для прохода по контейнеру в обратном порядке используйте обратные итераторы.
- Ключевое слово `auto` можно использовать для автоматического вывода типа переменной.
- Если итератор перестает указывать на существующий элемент контейнера, он становится невалидным.
- Обращение к невалидному итератору — это UB (undefined behaviour).
