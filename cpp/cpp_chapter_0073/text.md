# Глава 7.3. Упорядоченные ассоциативные контейнеры

Упорядоченные ассоциативные контейнеры хранят в _отсортированном виде_ ключи или пары ключ-значение. Отсюда возникает требование к типу ключа: к нему должно быть применимо сравнение оператором `<`. Это нужно для сравнения, какой ключ меньше, а какой — больше.

Доступ к элементу осуществляется по ключу и работает за `O(log(N)`.

![Ассоциативные контейнеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_assotiative.jpg) {.illustration}

Упорядоченные ассоциативные контейнеры реализуются через [бинарные деревья поиска.](https://ru.wikipedia.org/wiki/%D0%94%D0%B2%D0%BE%D0%B8%D1%87%D0%BD%D0%BE%D0%B5_%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D0%BE_%D0%BF%D0%BE%D0%B8%D1%81%D0%BA%D0%B0) Чаще всего — через [красно-чёрные деревья.](https://ru.wikipedia.org/wiki/%D0%9A%D1%80%D0%B0%D1%81%D0%BD%D0%BE-%D1%87%D1%91%D1%80%D0%BD%D0%BE%D0%B5_%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D0%BE)


Познакомимся с классом `std::map`. Но сперва сделаем отступление про класс для хранения пар ключ-значение в `std::map` и подобных контейнерах.

## Класс pair {#block-pair}

Шаблонный класс `std::pair` позволяет хранить пару объектов. Типы объектов задаются двумя параметрами шаблона и могут отличаться. А доступ к объектам осуществляется через поля `first` и `second`.

```cpp  {.example_for_playground .example_for_playground_006}
std::pair<std::size_t, std::string> user = {507, "bot_master"};

std::size_t id = user.first;
std::string login = user.second;
```

## Класс map

Контейнер [std::map](https://ru.cppreference.com/w/cpp/container/map) предназначен для хранения пар ключ-значение с уникальным ключом. Элементы `std::map` — это объекты класса `std::pair`. Шаблон класса `std::map` принимает два параметра: тип ключа и тип значения. Ключи хранятся отсортированными по возрастанию.

```cpp  {.example_for_playground .example_for_playground_007}
// Ключ - имя пользователя.
// Значение - Unix timestamp последнего логина
std::map<std::string, std::time_t> user_last_login = {
    {"Alice", 1153044881},
    {"Bob", 1743461707},
};

// Вывод отсортирован по имени пользователя
std::println("{}", user_last_login);
```
```
{"Alice": 1743461707, "Bob": 1153044881}
```

**Доступ к элементу** можно организовать несколькими способами: через оператор `[]`, методы `at()` и `find()`.

Обращение через `[]` _создает_ ключ, если он отсутствует, и к нему привязывается значение по умолчанию.

```cpp  {.example_for_playground .example_for_playground_008}
// Ключ существует, получаем его значение
std::time_t login_time_a = user_last_login["Alice"];

// Ключа нет, он добавляется
std::time_t login_time_e = user_last_login["Eve"];

std::println("{}", user_last_login);
```
```
{"Alice": 1153044881, "Bob": 1743461707, "Eve": 0}
```

Добавление ключа в случае его отсутствия может быть нежелательным. Если по логике программы ключ _обязан_ присутствовать в контейнере, используйте метод `at()`. Он вернёт значение либо в случае его отсутствия бросит исключение. 

```cpp  {.example_for_playground .example_for_playground_009}
try
{
    std::time_t login_time_e = user_last_login.at("Eve");
    std::println("{}", login_time_e);
}
catch(const std::out_of_range & e)
{
    std::println("Not found");
}
```

На случаи, когда уверенности в присутствии ключа нет, предусмотрен метод `find()`. Он принимает ключ и возвращает итератор.

```cpp  {.example_for_playground .example_for_playground_010}
auto it = user_last_login.find("Eve");

if (it == user_last_login.end())
{
    std::println("Not found");
}
else
{
    std::pair<const std::string, std::time_t> record = *it;
    std::println("Key: {}. Value: {}", record.first, record.second);
}
```

Оператор разыменования `*` применяется к итератору для получения элемента, на который он указывает. Обратите внимание, что в паре ключ-значение ключ константен: `std::pair<const std::string, std::time_t>`. Это говорит о том, что у элемента нельзя изменить ключ.

В реальном коде практически никогда не требуется явное получение из итератора переменной класса `std::pair`. Доступ к ключу и значению организуется проще:

```cpp
std::println("Key: {}. Value: {}", (*it).first, (*it).second);
```

Разберем, что означают записи `(*it).first` и `(*it).second`. Для обращения к полю используется оператор доступа к элементу `.`. Но у `.` [приоритет выше,](https://en.cppreference.com/w/cpp/language/operator_precedence) чем у унарного оператора `*`. Чтобы сначала получить пару, а после этого обратиться к её полю, разыменование заключается в скобки: `(*it)`. И к полученному объекту пары применяется оператор доступа к элементу: `(*it).first`.

В C++ есть оператор `->`, позволяющий выразить то же самое, но короче. Эти две записи эквивалентны:

```cpp
(*it).first
```

```cpp
it->first
```

Перебор элементов `std::map` реализуется двумя способами: циклом по итераторам и циклом `range-for`.

Цикл по итераторам:

```cpp  {.example_for_playground .example_for_playground_011}
for (auto it = user_last_login.begin(); it != user_last_login.end(); ++it)
    std::println("Key: {}. Value: {}", it->first, it->second);
```

Цикл `range-for`:

```cpp  {.example_for_playground .example_for_playground_012}
for (std::pair<std::string, std::time_t> record: user_last_login)
    std::println("Key: {}. Value: {}", record.first, record.second);
```

При обходе контейнера через `range-for` мы работаем с парами ключ-значение. Вместо явного указания типа можно использовать `auto`:

```cpp  {.example_for_playground .example_for_playground_013}
for (auto record: user_last_login)
    std::println("Key: {}. Value: {}", record.first, record.second);
```

В C++17 появилась возможность распаковывать пары (и другие агрегатные типы) в несколько переменных с помощью синтаксиса, известного как structured binding. Сравните: {#block-structured-binding}

```cpp  {.example_for_playground .example_for_playground_022}
for (auto [login, time]: user_last_login)
    std::println("Key: {}. Value: {}", login, time);
```

Чтобы в цикле _изменять_ значения по ключу, используйте цикл с итераторами. В главе про ссылки вы узнаете, как это делать в цикле `range-for`.

```cpp  {.example_for_playground .example_for_playground_014}
for (auto it = user_last_login.begin(); it != user_last_login.end(); ++it)
    it->second = 0;
```

Для **вставки элемента** в контейнер `std::map` предусмотрено несколько способов. Перечислим три из них: оператор `[]`, методы `try_emplace()` и `insert_or_assign()`. {#block-insert}

Оператор `[]` добавляет значение по ключу либо перезаписывает уже существующее. Используйте его, если вам не требуется отличать вставку от перезаписи:

```cpp   {.example_for_playground .example_for_playground_015}
// Ключ - id сервера, значение - имя
std::map<std::size_t, std::string> server_names;

server_names[902] = "stage";
```

Метод [try_emplace()](https://en.cppreference.com/w/cpp/container/map/try_emplace) принимает ключ и значение. Он возвращает пару из итератора и флага. Флаг равен `true`, если вставка произошла, и `false`, если элемент по ключу уже существовал. Замены значения существующего элемента на новое не происходит. А возвращаемый итератор в любом случае указывает на элемент по ключу.

```cpp   {.example_for_playground .example_for_playground_016}
std::map<std::size_t, std::string> server_names;

std::size_t server_id = 902;
std::string name = "stage";

auto [it, inserted] = server_names.try_emplace(server_id, name);

if (inserted)
    std::println("Inserted key {} with value {}", it->first, it->second);
else
    std::println("Key {} exists. Value: {}", it->first, it->second);
```

Чтобы в случае существования ключа обновить его значение, есть метод [insert_or_assign()](https://en.cppreference.com/w/cpp/container/map/insert_or_assign).

```cpp {.example_for_playground .example_for_playground_017}
auto [it, inserted] = server_names.insert_or_assign(server_id, name);

if (inserted)
    std::println("Inserted key {} with value {}", it->first, it->second);
else
    std::println("Updated key's {} value to {}", it->first, it->second);
```

Методы `try_emplace()` и `insert_or_assign()` появились в C++17. До них для вставки элементов использовался не такой удобный и эффективный [insert()](https://en.cppreference.com/w/cpp/container/map/insert).

Для **удаления** элемента предусмотрен метод [erase()](https://en.cppreference.com/w/cpp/container/map/erase). У него несколько перегрузок для удаления по ключу, итератору и диапазону итераторов. Перегрузка, принимающая ключ, возвращает количество удалённых элементов (0 или 1). А перегрузки с итераторами возвращают итератор на элемент после удалённого.

```cpp
bool removed = server_names.erase("stage") > 0;
```

Реализуйте функцию `print_frequency()`. Она принимает вектор слов и выводит в консоль частоту, с которой встречается каждое слово. Вывод должен быть отсортирован по убыванию частоты. Слова с одинаковой частотой должны быть отсортированы по алфавиту. {.task_text}

В своём решении используйте `std::map`, `std::vector` и функцию для сортировки [std::sort()](https://en.cppreference.com/w/cpp/algorithm/sort), перегрузка которой принимает итераторы на диапазон и предикат. Функция сортирует элементы диапазона, попарно применяя предикат к его элементам: если предикат вернул `true` для элементов `a` и `b`, то в отсортированном диапазоне элемент `a` будет идти перед `b`. В качестве предиката используйте функцию `is_less()`. {.task_text #block-sort}

Пример консольного вывода функции `print_frequency()` для вектора `{"login", "register", "login", "start_course"}`: {.task_text}

```
2 login  
1 register
1 start_course
```

```cpp {.task_source #cpp_chapter_0073_task_0040}
bool is_less(std::pair<std::string, std::size_t> left,
             std::pair<std::string, std::size_t> right)
{
    if (left.second > right.second)
        return true;
    if (left.second < right.second)
        return false;
    
    return left.first < right.first;
}

void print_frequency(std::vector<std::string> words)
{

}
```
Заполните контейнер `std::map`, ключами которого будут слова, а значениями — их частоты. Затем заполните вектор, хранящий пары со словами и частотами. К вектору примените функцию сортировки с предикатом `is_less()`. Например: `std::sort(v.begin(), v.end(), is_less)`. {.task_hint}
```cpp {.task_answer}
bool is_less(std::pair<std::string, std::size_t> left,
             std::pair<std::string, std::size_t> right)
{
    if (left.second > right.second)
        return true;
    if (left.second < right.second)
        return false;
    
    return left.first < right.first;
}

void print_frequency(std::vector<std::string> words)
{
    std::map<std::string, std::size_t> words_count;

    for(std::string word: words)
    {
        auto [it, inserted] = words_count.try_emplace(word, 1);

        if (!inserted)
            ++it->second;    
    }

    std::vector<std::pair<std::string, std::size_t>> words_with_freq;
    words_with_freq.reserve(words_count.size());
    
    for (auto it = words_count.begin(); it != words_count.end(); ++it)
        words_with_freq.push_back(*it);

    std::sort(words_with_freq.begin(), words_with_freq.end(), is_less);

    for (auto & word_freq: words_with_freq)
        std::println("{} {}", word_freq.second, word_freq.first);
}
```

Для **поиска** элемента по ключу есть метод `find()`. Он возвращает итератор на нужный элемент. Если такового не нашлось, итератор указывает на позицию после последнего элемента.

## Класс set

Класс [std::set](https://en.cppreference.com/w/cpp/container/set) нужен, чтобы работать со множеством уникальных ключей. В отличие от `std::map`, значения к ним не привязаны. В целом эти контейнеры схожи за исключением некоторых нюансов. Например, у `std::set` нет метода `try_emplace()`, потому что он не имел бы особого смысла. Вместо него есть метод [emplace()](https://en.cppreference.com/w/cpp/container/set/emplace).

```cpp {.example_for_playground .example_for_playground_018}
std::set<std::string> words = {"a", "the"};

words.emplace("then");
words.emplace("a");

words.erase("the");

std::println("Key \"then\" exists: {}. Set items:", words.contains("then"));

for (std::string word: words)
    std::print("{} ", word);
```
```
Key "then" exists: true. Set items:
a then
```

Класс `IPv4Pool` — это пул свободных [IPv4](https://ru.wikipedia.org/wiki/IPv4) адресов. С помощью пула можно зарезервировать ip-адрес и освободить его после использования. Необходимо реализовать методы класса `IPv4Pool`. {.task_text}

`IPv4Pool(IPv4Range range)` — конструктор, принимающий диапазон доступных ip-адресов. Диапазон не может быть пустым. {.task_text}

`std::pair<IPv4, bool> reserve_ip()` — находит первый свободный `IPv4` адрес и резервирует его. Возвращает `true` в случае успеха. Если свободных адресов не осталось, то возвращает пару `{IPv4(), false}`.  {.task_text}

`bool release_ip(IPv4 ip4)` — освобождает зарезервированный ip-адрес и возвращает `true`. Если переданный адрес отсутствует в пуле, то метод возвращает `false`. {.task_text}

Классы `IPv4` и `IPv4Range` уже есть в проекте. `IPv4` реализует ip-адрес. Для его объектов доступны все виды сравнения и форматированный вывод. {.task_text}

Класс диапазона `IPv4Range` позволяет перечислить все входяшие в него ip-адреса. Он пригоден для обхода циклом [range-for](/courses/cpp/chapters/cpp_chapter_0040/#block-range-for). Также можно воспользоваться методами `cbegin()`, `cend()` и `find()`, которые возвращают `IPv4Range::const_iterator` на начало, конец и на указанный ip-адрес соответственно. Размер диапазона можно получить с помощью метода `size()`. {.task_text}

```cpp {.task_source #cpp_chapter_0073_task_0100}
class IPv4Pool
{
public:
    explicit IPv4Pool(IPv4Range range)
    {

    }

    std::pair<IPv4, bool> reserve_ip()
    {

    }

    bool release_ip(IPv4 ip4)
    {

    }

private:

};
```
Заведите множество `std::set` для хранения зарезервированных ip-адресов. Для поиска свободного ip-адреса обойдите диапазон, переданный в конструкторе пула. В цикле проверяйте наличие текущего адреса в множестве зарезервированных. Верните первый не найденный среди зарезервированных адресов. Для освобождения ip-адреса достаточно удалить его из множества зарезервированных. Предложенное решение неэффективно при резервировании большого количества ip-адресов сразу. Подумайте над более оптимальным вариантом. {.task_hint}
```cpp {.task_answer}
class IPv4Pool
{
public:
    explicit IPv4Pool(IPv4Range range)
    {
        m_range = range;
        m_cursor = m_range.cbegin();
    }

    std::pair<IPv4, bool> reserve_ip()
    {
        if (m_range.size() == m_reserved.size())
            return { IPv4(), false };

        const auto end = m_range.cend();

        while (true)
        {
            if (m_cursor == end)
                m_cursor = m_range.cbegin();

            auto emplaced = m_reserved.emplace(*m_cursor);

            if (emplaced.second)
                return { *m_cursor, true };

            ++m_cursor;
        }
    }

    bool release_ip(IPv4 ip4)
    {
        const bool released = m_reserved.erase(ip4) > 0;
        if (released)
            m_cursor = m_range.find(ip4);

        return released;
    }

private:
    IPv4Range m_range;
    std::set<IPv4> m_reserved;
    IPv4Range::const_iterator m_cursor;
};
```

В C++17 в `std::set` был добавлен метод `merge()`. Он нужен, чтобы объединять два множества:

```cpp {.example_for_playground .example_for_playground_019}
std::set<int> a = {1, 2, 3};
std::set<int> b = {0, 2, 4};

a.merge(b);

std::println("{}", a);
```
```
{0, 1, 2, 3, 4}
```

## Классы multimap и multiset

В классах [std::multimap](https://en.cppreference.com/w/cpp/container/multimap) и [std::multiset](https://en.cppreference.com/w/cpp/container/multiset) ключи могут повторяться. Допустим, у нас есть несколько серверов, на которых выполняются задания. Мы могли бы представить это как `std::map` с ключом — именем сервера и значением — вектором заданий:

```cpp {.example_for_playground .example_for_playground_020}
std::map<std::string, std::vector<std::string>> server_jobs;

auto [it, inserted] = server_jobs.try_emplace("stage", 
                                            std::vector<std::string>
                                            {"db_replication"}
                                            );
if (!inserted)
    it->second.push_back("db_replication"); // Вызываем push_back() у вектора
```

А можно использовать `std::multimap`. Его метод [equal_range()](https://en.cppreference.com/w/cpp/container/multimap/equal_range) принимает ключ и возвращает итераторы на начало и конец диапазона элементов с этим ключом. У `std::multimap` нет метода `try_emplace()`, потому что пара ключ-значение может быть добавлена, даже если ключ уже существует в контейнере. Вместо него есть метод [emplace()](https://en.cppreference.com/w/cpp/container/multimap/emplace). {#block-equal-range}

```cpp {.example_for_playground .example_for_playground_021}
std::multimap<std::string, std::string> server_jobs;

server_jobs.emplace("stage", "db_replication");
server_jobs.emplace("stage", "file_storage_backup");
server_jobs.emplace("prod", "packages_update");

for (auto[it, it_end] = server_jobs.equal_range("stage"); it != it_end; ++it)
    std::println("Job {}", it->second);
```

----------

## Резюме

Алгоритмическая сложность работы с упорядоченными ассоциативными контейнерами:


![Алгоритмическая сложность работы с упорядоченными ассоциативными контейнерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_associative_algo_complexity.jpg) {.illustration}
