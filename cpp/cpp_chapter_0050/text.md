# Глава 5. Экскурсия по C++

Краткая экскурсия по C++ заняла бы _целую книгу._ В этой главе мы пробежимся только по основным возможностям языка. Они не уникальны для C++, но без них современный C++ невообразим. Позже каждая тема будет раскрыта подробнее. А пока вы познакомитесь с известными фишками C++ и научитесь их применять на базовом уровне.

## Перегрузка функций 

В C++ допустима перегрузка функций (overloading) — заведение функций с одинаковыми именами, но разным набором параметров. Отличаться могут типы параметров и их количество.

Перегрузка функций — один из видов [полиморфизма.](https://ru.wikipedia.org/wiki/%D0%9F%D0%BE%D0%BB%D0%B8%D0%BC%D0%BE%D1%80%D1%84%D0%B8%D0%B7%D0%BC_(%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%82%D0%B8%D0%BA%D0%B0)) Она избавляет от придумывания разных имен функций, которые делают практически одно и то же, но для разных параметров. Это упрощает кодовую базу.

```c++  {.example_for_playground .example_for_playground_001}
void log_error(std::size_t error_code)
{
    std::println("Error. Code: {}", error_code);
}

void log_error(std::size_t error_code, std::string message)
{
    std::println("Error. Code {}: {}", error_code, message);
}

int main()
{
    log_error(404);               // Error. Code: 404
    log_error(404, "not found");  // Error. Code 404: not found
}
```

Перегруженные функции _могут_ возвращать значения разных типов. Но нельзя сделать перегрузку функции, отличающуюся _только_ возвращаемым типом. Это ограничение действует для обычных функций, но не распространяется на шаблонные функции. О них вы узнаете в конце главы.

Какие перегрузки функций конфликтуют между собой? Перечислите номера строк через пробел. {.task_text}

```
1 int  handle_response(int status, std::string raw_body)
2 bool handle_response(std::string raw_body)
3 int  handle_response(std::string encoded_body)
4 int  handle_response(int status)
```

```consoleoutput {.task_source #cpp_chapter_0050_task_0010}
```
Функции 2 и 3 отличаются именем параметра и типом возвращаемого значения. На основании только этого невозможно создать перегрузку. {.task_hint}
```cpp {.task_answer}
2 3
```

## Исключения

Концептуально есть два подхода к обработке ошибок. Первый заключается в том, что функция так или иначе возвращает наружу некий признак. _По месту вызова_ функции этот признак проверяется. Затем либо обрабатывается ошибочная ситуация, либо продолжается штатное выполнение бизнес-логики. Если эта же функция вызывается пятью строками ниже — что ж, все те же проверки должны быть и там.

Второй подход основывается на разделении бизнес-логики и кода для обработки ошибок. К этому подходу относится и механизм исключений. В случае ошибки:
1. Функция создает специальный объект — исключение (exception). Его цель — передать информацию об ошибке из точки обнаружения в точку обработки.
2. Функция прерывает свое выполнение — бросает исключение.
3. Управление передается обработчику исключения. Он находится в коде, вызвавшем функцию явно _или косвенно._ Если подходящий обработчик не найден, программа завершает работу.

У каждого из подходов есть свои достоинства и недостатки. В проекте «Деление без деления», который вы только что выполнили, реализован первый подход. В нем деление на ноль обрабатывается через возврат специального значения:

```c++
inline std::size_t divide(std::size_t a, std::size_t b)
{
    if (b == 0)
        return std::numeric_limits<std::size_t>::max();
    // ...
}
```

Во всех местах вызова `divide()` нужно не забыть проверку: вдруг вернулось значение, говорящее об ошибке?

```c++
const std::size_t res = divide(blob_len, chunk_len);

if (res == std::numeric_limits<std::size_t>::max())
    std::println("Error in divide()");
```

Давайте перепишем этот код. Чтобы избежать деления на ноль, бросим исключение [оператором throw:](https://en.cppreference.com/w/cpp/language/throw)

```c++
inline std::size_t divide(std::size_t a, std::size_t b)
{
    if (b == 0)
        throw std::runtime_error("division by zero");
    // ...
}
```

На вершине [иерархии исключений](https://en.cppreference.com/w/cpp/error/exception) стандартной библиотеки находится класс `std::exception`. Это наиболее общее исключение, от которого наследуются все остальные. Использованный в примере `std::runtime_error` также является его потомком.

Выполнение кода, бросившего исключение, прерывается. Управление передается обработчику исключения. Для этого бросивший исключение код должен _явно или косвенно_ вызываться из блока `try` [конструкции try-catch.](https://en.cppreference.com/w/cpp/language/catch)

Блок `catch` и является обработчиком. Он перехватывает и обрабатывает исключения указанного класса и его потомков.

```c++
try
{
    const std::size_t res = divide(blob_len, chunk_len);
    // ...
}
catch(const std::runtime_error & e)
{
    std::println("Error {}", e.what());
    return;
}
```

В круглых скобках блока `catch` указывается объект исключения. Между именем объекта и его классом ставится символ амперсанда `&`. Это означает, что объект передается по ссылке, а не по значению. В следующих главах вы узнаете, что такое ссылки. А пока просто не забывайте ставить `&` перед именем исключения в блоке `catch`.

В ряде случаев нет необходимости ловить конкретный тип исключения. Можно ловить сразу группу исключений, используя общий родительский класс. В нашем примере мы могли заменить `std::runtime_error` в блоке `catch` на `std::exception`, чтобы ловить _все_ стандартные исключения и их наследников.

У каждого из классов исключений есть метод `what()` для получения строковой информации об ошибке.

Функция `handle_daily_stats()` обрабатывает статистику действий пользователей на сайте. Функции подготовки статистики, которые она вызывает, в случае неудачи возвращают `false`. {.task_text}

Было принято решение отказаться от обработки возвращаемого из функций флага в пользу обработки исключений. Теперь функции `filter()`, `sort()` и `count()` ничего не возвращают, а в случае ошибки бросают `std::runtime_error` с соответствующим текстом. {.task_text}

Требуется переписать код таким образом, чтобы исключение обрабатывалось в функции `handle_daily_stats()`: в консоль должна выводиться ошибка, возвращаемая методом `what()`. {.task_text}

```c++ {.task_source #cpp_chapter_0050_task_0020}
void handle_daily_stats()
{
    if (!filter())
    {
        std::println("filter error");
        return;
    }

    if (!sort())
    {
        std::println("sort error");
        return;
    }

    if (!count())
    {
        std::println("count error");
        return;
    }

    std::println("success");
}

```
В функцию `handle_daily_stats()` добавьте конструкцию `try-catch`. В блоке `try` последовательно вызовите функции `filter()`, `sort()`, `count()`, а также выведите в консоль строку `"success"`. В блоке `catch` перехватите исключение `const std::runtime_error & e` и выведите в консоль строку, возвращаемую методом `e.what()`. {.task_hint}
```c++ {.task_answer}
void handle_daily_stats()
{
    try
    {
        filter();
        sort();
        count();
        std::println("success");
    }
    catch(const std::runtime_error & e)
    {
        std::println("{}", e.what()); 
    }
}
```

Механизм исключений позволяет:
- Разграничивать бизнес-логику и логику обработки ошибок.
- Выбирать, в каком месте кода обрабатывать ошибки какого типа.
- Группировать обработку ошибок по типам.

## Пространства имен {#block-namespaces}

[Пространство имен](https://en.cppreference.com/w/cpp/language/namespace) (namespace) — это область в коде, объединяющая логически связанные функции, константы, классы и другие сущности.

```c++  {.example_for_playground .example_for_playground_002}
namespace Net
{
    const std::string loopback = "127.0.0.1";

    std::string get_hostname(IPv4 ip) { /* ... */ }

    // ...
}
```

Для обращения к содержимому пространства имен применяется оператор разрешения области видимости `::`.

```c++  {.example_for_playground .example_for_playground_003}
std::println(Net::loopback);

std::println(Net::get_hostname("192.0.2.1"));
```

Пространства имен решают две основные задачи:
- Предотвращение конфликта имен. Например, если завести два пространства имен `crt` (cartesian) и `geo` (geography), то в них можно создавать функции с одинаковыми именами. Их вызов будет выглядеть так: `crt::distance(point1, point2)`, `geo::distance(point1, point2)`.
- Управление сложностью проекта: пространства имен упрощают организацию кода. Они структурируют проект на отдельные компоненты.

Пространства имен могут быть вложенными: `data::ODBC::bind()`. Чтобы к ним было удобно обращаться, имена лучше делать короткими: `std`, `Core`, `io`.

Все содержимое [коллекции библиотек boost](https://www.boost.org/) находится в одноименном пространстве имен. Внутри него есть пространство имен `asio` библиотеки для работы с сетью. Внутри него — пространство `ip`, и уже внутри него — `tcp`. В этом пространстве имен есть класс `acceptor`. {.task_text}

Напишите, как выглядит полное обращение к `acceptor`. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0050_task_0030}
```
Вложенность: boost -> asio -> ip -> tcp -> acceptor. Используйте оператор `::`.  {.task_hint}
```cpp {.task_answer}
boost::asio::ip::tcp::acceptor
```

## Перечисления enum {#block-enum}

[Перечисления](https://en.cppreference.com/w/c/language/enum) (enumerations) облегчают работу с данными, которые принимают фиксированное и заранее известное множество значений.

Перечисление — это тип, значения которого ограничены набором именованных констант. Во многих код-стайлах имена констант задаются заглавными буквами:

```c++   {.example_for_playground .example_for_playground_004}
// Создаем новый тип
enum HashType
{
    MD5,
    SHA1,
    SHA2
};

// Создаем объект этого типа
HashType hash_type = SHA2;
```

Мы завели тип `HashType`. Переменные этого типа принимают одно из 3-х значений: `MD5`, `SHA1`, `SHA2`. Под капотом каждая из этих констант равна какому-то целому числу. За счет этого компилятор может выполнить неявное преобразование: привести значение типа `HashType` к целочисленному типу.

```c++  {.example_for_playground .example_for_playground_005}
// Оба варианта обращения к константам корректны:
int k = HashType::MD5;
int m = SHA1;

HashType hash_type = SHA2;
int n = hash_type;

std::println("{} {} {}", k, m, n);
```
```
0 1 2
```

Взгляните еще раз на то, как выглядит создание перечисления. Создадим новый тип `Compression`, переменные которого могут принимать два значения — `QuickLZ` и `LZ4`:

```c++   {.example_for_playground .example_for_playground_006}
enum Compression
{
    QuickLZ,
    LZ4,
};
```

После объявления перечисления ставится оператор `;`. Такой синтаксис — очередное наследие Си, используемое и для классов со структурами. Он был введен, чтобы сразу после определения типа создавать его объект.

Так выглядит одновременное создание нового типа `Compression` и переменной этого типа `compression_method`:

```c++   {.example_for_playground .example_for_playground_007}
enum Compression
{
    QuickLZ,
    LZ4,
} compression_method;

// ...

compression_method = LZ4;
```

В современном C++ коде такая практика практически не встречается.

Значение константы внутри перечисления можно определить явно после оператора `=`. Если этого не сделано, константа равна значению предыдущей, увеличенному на 1. Первая константа по умолчанию равна нулю.

Каковы целочисленные значения, принимаемые перечислением `State`? Укажите их через пробел. {.task_text}

```c++
enum State
{
    INITIALIZED,
    PAUSED,
    DONE = 6,
    RESET
};
```

```consoleoutput {.task_source #cpp_chapter_0050_task_0040}
```
`INITIALIZED` — первая константа, значит, она равна нулю. Значение следующей константы равно значению предыдущей, увеличенному на 1. Значит, `PAUSED` равна 1. `DONE` равна 6. И поэтому `RESET` равна 7. {.task_hint}
```cpp {.task_answer}
0 1 6 7
```

## Перечисления enum class

Как вы только что узнали, у перечислений `enum` есть две особенности:
- Имя константы доступно и без указания имени перечисления. Вместо `State::RESET` допустимо написать просто `RESET`. Это может привести к конфликту имен, если существует другая константа с таким же именем.
- Значения перечислений неявно преобразуются к целочисленным типам и другим перечислениям.

Такое поведение порождает массу ошибок. Чтобы их избежать, в C++11 был введен `enum class`. Он ничем не отличается от обычного `enum` кроме запрета неявных преобразований и безопасной работы с именами констант. [Старайтесь всегда использовать](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Renum-class) `enum class` вместо `enum`.

```c++   {.example_for_playground .example_for_playground_008}
enum class ByteOrder
{
    NATIVE        = 1,
    LITTLE_ENDIAN = 2,
    BIG_ENDIAN    = 3
};

ByteOrder bo1 = NATIVE;            // Ошибка
ByteOrder bo2 = ByteOrder::NATIVE; // ОК
int n = ByteOrder::NATIVE;         // Ошибка
```

Заведите `enum class` `Rarity` со значениями `COMMON`, `RARE`, `LEGENDARY`. {.task_text}

Напишите функцию `to_string()`, которая принимает значение типа `Rarity` и возвращает соответствующую ему строку: `"Common"`, `"Rare"`, `"Legendary"`. {.task_text}

В теле функции используйте `switch-case`. {.task_text}

```c++ {.task_source #cpp_chapter_0050_task_0050}
// Перечисление Rarity

// Функция to_string()
```
После объявления `enum class` не забудьте точку с запятой. Объявление `Rarity` должно идти до объявления функции `to_string()`. Все возвращаемые функцией строки начинаются с заглавной буквы. {.task_hint}
```c++ {.task_answer}
enum class Rarity
{
    COMMON,
    RARE,
    LEGENDARY
};

std::string to_string(Rarity rarity)
{
    switch(rarity)
    {
        case Rarity::COMMON:
            return "Common";
        case Rarity::RARE:
            return "Rare";
        case Rarity::LEGENDARY:
            return "Legendary";
    }
}
```

## Классы

[Класс](https://en.cppreference.com/w/cpp/language/class) (class) — это составной тип данных. Он позволяет:
- Объединять данные и методы их обработки.
- Хранить состояние.
- Описывать абстракции и предоставлять интерфейс для работы с ними.
- Ограничивать доступ к полям и методам, то есть [инкапсулировать](https://ru.wikipedia.org/wiki/%D0%98%D0%BD%D0%BA%D0%B0%D0%BF%D1%81%D1%83%D0%BB%D1%8F%D1%86%D0%B8%D1%8F_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)) внутреннюю реализацию.
- Автоматически управлять ресурсами (об этом чуть позже).

Верхнеуровнево класс выглядит так:

```c++
class имя_класса
{
    // содержимое: поля и методы
};
```

Данные класса хранятся в его полях. { #block-class-message }

```c++
class Message
{
public:
    std::string id;
    std::string text;
};
```

[Спецификатор доступа](https://en.cppreference.com/w/cpp/language/access) `public` означает, что перечисленные после него поля и методы составляют его публичный интерфейс и открыты пользователям класса.

Методы класса выглядят как функции, определенные внутри класса. Они имеют доступ ко всем его полям и методам. Методы могут иметь перегрузки. { #block-class-unixtimestamp }

```c++   {.example_for_playground .example_for_playground_009}
class UnixTimestamp
{
public:
    void show_days()
    {
        const std::time_t seconds_in_day = 24 * 60 * 60;
        std::println("{} days since 01.01.1970", seconds / seconds_in_day);
    }

    std::time_t seconds = 0;
};
```

Для хранения временной метки формата [Unix time](https://en.wikipedia.org/wiki/Unix_time) мы воспользовались [типом std::time_t](https://en.cppreference.com/w/cpp/chrono/c/time_t). Его реализация зависит от компилятора, но чаще всего это знаковое целое число. Оно хранит количество секунд, прошедших с 00:00 1 января 1970 года (UTC).

Обратите внимание, что поле `seconds` мы инициализировали нулем. Явно инициализировать поля простых типов значениями можно _и нужно._

Для обращения к полям и методам _объекта класса_ используется оператор доступа к элементу `.`:

```c++   {.example_for_playground .example_for_playground_010}
UnixTimestamp ts;

ts.seconds = 1153044000;
ts.show_days();
```

Поля и методы можно сделать внутренними — доступными только из других методов, но не снаружи. Для этого предназначен спецификатор доступа `private`. Все, что перечислено после него, составляет внутреннюю реализацию класса.

Рассмотрим класс `Task`, описывающий задачу в типичном таск-трекере. У него есть методы для логирования времени работы над задачей и обновления ее статуса. Поля, хранящие время и статус, сделаны приватными. В некоторых код-стайлах приватные поля именуются с префиксом `m_` (member). { #block-class-task }

```c++ {.example_for_playground .example_for_playground_011}
enum class State { TODO = 1, INPROGRESS = 2, DONE = 3 };

class Task
{
public:
    bool log_work_hours(std::size_t hours)
    {
        if (m_state == State::TODO)
            m_state = State::INPROGRESS;

        if (m_state == State::INPROGRESS)
            m_workHours += hours;

        return m_state == State::INPROGRESS;
    }

    bool update_state(State new_state)
    {
        if (new_state > m_state)
            m_state = new_state;

        return m_state == new_state;
    }

  void show_work_hours() { std::println("{}", m_workHours); }

private:
    State m_state = State::TODO;
    std::size_t m_workHours = 0;
};
```

А теперь поработаем с объектом класса `Task`:

```c++ {.example_for_playground .example_for_playground_012}
Task t;
t.update_state(State::INPROGRESS);
t.log_work_hours(5);
t.show_work_hours();
```

Напишите класс `Device` с публичными методами: {.task_text}
- `void start()` — помечает устройство как запущенное. Сохраняет время включения в секундах (`std::time_t`). Для получения текущего
 времени в проекте уже есть функция `std::time_t get_cur_time()`. Используйте ее в этом и других методах.
- `void stop()`— помечает устройство как выключенное. По умолчанию (если метод `start()` ни разу не вызывался) устройство считается выключенным.
- `void set_latest_healthcheck()` — сохраняет время последнего хэлсчека — аудита «здоровья». Если на этот момент устройство выключено, кидает исключение `std::logic_error`.
- `bool is_active()` — возвращает `true`, если устройство включено и с момента последнего хэлсчека прошло не больше 1 минуты.
- `std::time_t uptime()` — возвращает время работы. Если устройство выключено, возвращает 0.

```c++ {.task_source #cpp_chapter_0050_task_0060}
```
Чтобы хранить состояние устройства, заведите 3 приватных поля: `bool is_on` — включено устройство или нет; `time_started` — время запуска; `time_checked` — время последнего хэлсчека. Метод `start()` должен устанавливать поле `is_on` в `true`, а `time_started` в `get_cur_time()`. Метод `stop()` должен устанавливать `is_on` в `false`. Метод `set_latest_healthcheck()` должен проверять `is_on` и бросать `std::logic_error`. А если устройство включено, устанавливать `time_checked` в `get_cur_time()`. Метод `is_acitve()` должен возвращать результат выражения `is_on && get_cur_time() < time_checked + 60`. А метод `uptime()` — результат выражения `is_on ? get_cur_time() - time_started : 0`. {.task_hint}
```c++ {.task_answer}
class Device
{
public:
    void start()
    {
        is_on = true;
        time_started = get_cur_time();
    }

    void stop()
    {
        is_on = false;
    }

    void set_latest_healthcheck()
    {
        if (!is_on)
            throw std::logic_error("device is off");
        
        time_checked = get_cur_time();
    }

    bool is_active()
    {
        return is_on && get_cur_time() < time_checked + 60;
    }

    std::time_t uptime()
    {
        return is_on ? get_cur_time() - time_started : 0;
    }

private:
    std::time_t time_checked = 0;
    std::time_t time_started = 0;
    bool is_on = false;
};
```

### Конструкторы и деструкторы

У классов есть особые методы — конструкторы и деструкторы. Конструкторы нужны для корректного создания объектов. А деструкторы — для их разрушения. Из конструкторов и деструкторов, как и из обычных методов, можно обращаться к полям класса, вызывать методы и свободные функции.

Базовые факты о конструкторах:
- Конструктор нужен для инициализации состояния объекта.
- Имя конструктора совпадает с именем класса.
- Возвращаемый тип конструктора не указывается. Он ничего не возвращает. Но может бросить исключение.
- Класс может иметь несколько конструкторов: допустима их перегрузка.
- Конструктор, не принимающий параметров, называется конструктором по умолчанию.
- У класса может не быть явно добавленных конструкторов. Тогда при создании объекта вызывается конструктор по умолчанию. Он в свою очередь вызывает конструкторы по умолчанию для полей класса.
- Конструктор вызывается неявно при создании объекта, но может быть вызван и напрямую.

Базовые факты о деструкторах:
- Деструктор нужен для освобождения используемых объектом ресурсов.
- Имя деструктора совпадает с именем класса, перед которым ставится символ `~`. Например, `~Device()`.
- Как и у конструктора, у деструктора не указывается возвращаемый тип.
- Если деструктор бросит исключение, в общем случае программа аварийно завершится. Позже вы узнаете о причинах такого поведения и о способах его обхода.
- Перегрузка деструктора невозможна.
- Если деструктор не указан явно, вызывается деструктор по умолчанию. Он вызывает деструкторы полей класса.
- Деструктор срабатывает в момент удаления объекта. Практически никогда не требуется вызывать его напрямую.

```c++  {.example_for_playground .example_for_playground_013}
class File
{
public:
    // Конструктор по умолчанию
    File() { }

    // Конструктор с параметром
    File(std::string path) { }

    // Деструктор
    ~File(){ }
};

int main()
{
    // Создание объекта с помощью конструктора по умолчанию
    File f1;

    // Создание объекта с помощью конструктора с параметром
    File f2("/tmp/dump.csv");

    // При выходе из main() для f1 и f2 будут вызваны
    // деструкторы
}
```

### Идиома RAII {#block-raii}

Конструкторы и деструкторы позволяют в C++ реализовывать идиому [RAII.](https://ru.wikipedia.org/wiki/%D0%9F%D0%BE%D0%BB%D1%83%D1%87%D0%B5%D0%BD%D0%B8%D0%B5_%D1%80%D0%B5%D1%81%D1%83%D1%80%D1%81%D0%B0_%D0%B5%D1%81%D1%82%D1%8C_%D0%B8%D0%BD%D0%B8%D1%86%D0%B8%D0%B0%D0%BB%D0%B8%D0%B7%D0%B0%D1%86%D0%B8%D1%8F) Resource Acquisition Is Initialization (получение ресурса есть инициализация) — это подход для автоматического управления ресурсами, требующими парных действий. Например, выделение и освобождение памяти, подключение и отключение от БД, открытие и закрытие файла. 

Получение ресурса реализуется в конструкторе, а освобождение — в деструкторе. Когда объект разрушается, ресурс освобождается автоматически. Разработчику не нужно заботиться об этом в каждой из веток кода.

В проекте есть класс `DBConn` для работы с БД. Этот класс — обертка над старым кодом на Си. Он реализует методы для подключения, отключения и выполнения запроса к БД. {.task_text}

Перепишите `DBConn` в соответствии с идиомой RAII. Заведите конструктор, внутри которого происходит подключение к БД. В случае неудачи конструктор должен бросать исключение `std::runtime_error`. Отключение от БД должно быть в деструкторе. {.task_text}

Затем перепишите в соответствии с этими изменениями класса функцию `handle_metrics()`. Перехватывать в ней исключения не нужно: они обрабатываются в коде, вызывающем `handle_metrics()`. {.task_text}

```c++ {.task_source #cpp_chapter_0050_task_0070}
class DBConn
{
public:
    bool open(std::string conn_str)
    {
        if (is_open())
            return false;

        // Метод строки c_str() возвращает 
        // указатель на строку в сишном стиле.
        // Про указатели вы узнаете позже.
        m_handle = open_db(conn_str.c_str());
        return is_open();
    }

    // После переписывания класса на RAII этот
    // метод больше не будет не нужен.
    bool is_open()
    {
        return m_handle != INVALID_DB_HANDLE;
    }

    // Этот метод тоже будет не нужен.
    // Соединение будет закрывать деструктор.
    void close()
    {
        if (is_open())
        {
            close_db(m_handle);
            m_handle = INVALID_DB_HANDLE;
        }
    }

    bool exec(std::string query)
    {
        return is_open() ? 
        exec_db_query(m_handle, query.c_str()) == 0 : false;
    }

private:
    db_handle m_handle = INVALID_DB_HANDLE;
};


// Перехватывать исключения в этом методе не нужно.
// Пусть они пробрасываются дальше.
void handle_metrics()
{
    DBConn db_conn;
    
    if (!db_conn.open("postgresql://user:secret@localhost"))
        return;

    if (!db_conn.exec("select * from metrics"))
    {
        // Упс! Здесь забыт вызов db_conn.close().
        // Открытое соединение продолжит висеть.
        // С RAII подобные ошибки исключены.
        return;
    }
    
    db_conn.close();
}
```
Заведите конструктор `DBConn(std::string conn_str)`, открывающий подключение к БД. Закрывайте подключение в деструкторе `~DBConn()`. {.task_hint}
```c++ {.task_answer}
class DBConn
{
public:
    DBConn(std::string conn_str)
    {
        m_handle = open_db(conn_str.c_str());
        
        if (m_handle == INVALID_DB_HANDLE)
            throw std::runtime_error("Couldn't connect");
    }

    ~DBConn()
    {
        close_db(m_handle);
    }

    bool exec(std::string query)
    {
        return exec_db_query(m_handle, query.c_str());
    }

private:
    db_handle m_handle = INVALID_DB_HANDLE;
};


void handle_metrics()
{
    DBConn db_conn("postgresql://user:secret@localhost");
    db_conn.exec("select * from metrics");
}
```

Идиома RAII — один из столпов современного C++. Мы не раз к ней вернемся.

## Структуры

Помимо классов в C++ есть [структуры](https://en.cppreference.com/w/c/language/struct) (struct). Они очень похожи. Но поля и методы класса по умолчанию приватные (private). А у структуры — публичные (public):

```c++  {.example_for_playground .example_for_playground_014}
struct UserMessage
{
    std::size_t id = 0;
    std::size_t time_created = 0;
    std::string text;
};

UserMessage msg;
msg.text = "C++: Simula in wolf’s clothing";
```

Когда лучше использовать структуры, а когда — классы?

Структуры подходят для группировки данных, каждое поле которых изменяется отдельно от других без нарушения целостности объекта. Все поля таких структур остаются открытыми. И зачастую такие структуры не имеют методов.

Например, координата точки на плоскости состоит из двух значений. Изменение любого из них не приводит координату в неконсистентное состояние.

```c++  {.example_for_playground .example_for_playground_015}
struct Point
{
    double x = 0.0;
    double y = 0.0;
};

Point p;
p.x = 10.1;
p.y = -2.0;
```

Если же изменение полей способно сломать состояние объекта, то вместо структур применяются классы. Поля делаются закрытыми, а их чтение и изменение реализуется через методы.

В этой главе мы обсудили классы и привели три простых примера: [Message,](/courses/cpp/chapters/cpp_chapter_0050/#block-class-message) [UnixTimestamp,](/courses/cpp/chapters/cpp_chapter_0050/#block-class-unixtimestamp) [Task.](/courses/cpp/chapters/cpp_chapter_0050/#block-class-task) Перечислите те из них, которые логичнее было бы сделать структурами. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0050_task_0080}
```
Только в классе `Task` изменение одного из полей на произвольное значение приведет объект в неконсистентное состояние. {.task_hint}
```cpp {.task_answer}
Message UnixTimestamp
```

## Шаблоны {#block-templates}

[Шаблоны](https://en.cppreference.com/w/cpp/language/templates) (templates) позволяют писать обобщенный код без привязки к конкретным типам и константам. Это эффективный способ переиспользования кода.

[Помните](/courses/cpp/chapters/cpp_chapter_0030/#block-max) функцию `max()`?

```c++
int max(int a, int b)
{
    return a < b ? b : a;
}
```

Как быть, если нужно применять ее не только для `int`, но и для других типов — `double`, `char` и даже строк? Под каждый тип мы могли бы завести перегрузку. Но это приведет к копированию одного и того же кода, отличающегося лишь типами. Мы нарушим отличный принцип «не повторяйся» [(DRY, Don’t repeat yourself).](https://ru.wikipedia.org/wiki/Don%E2%80%99t_repeat_yourself)

Так давайте же научим `max()` работать с параметрами любого типа. Создадим шаблон функции:

```c++   {.example_for_playground .example_for_playground_016}
template<class T> // T - параметр шаблона
T max(T a, T b)   // a и b - параметры функции
{
    return a < b ? b : a;
}
```

Ключевое слово `template` на первой строке означает, что перед нами шаблон. После него в треугольных скобках через запятую перечисляются параметры шаблона. После ключевого слова `class` указывается имя параметра. Чаще всего шаблонным параметрам дают короткие имена с заглавной буквы. Например, `T` от слова `type`.

После перечисленных в треугольных скобках параметров шаблона идет функция. В ней доступны параметры шаблона по их именам. Функция `T max(T a, T b)` принимает параметры `a` и `b` некоего заранее неизвестного типа `T`. И возвращает значение этого же типа.

А теперь убедимся, что при вызове функция умеет принимать аргументы самых разных типов!

```c++   {.example_for_playground .example_for_playground_017}
max(5, 6);              // 6
max('z', 'a');          // z
max(1.01, 1.02);        // 1.02
```

Как это работает? В процессе сборки программы компилятор находит все вызовы шаблонных функций. Для каждого уникального набора аргументов он _инстанцирует_ шаблон: порождает обычную функцию из шаблона функции. Практически это означает, что компилятор вместо программиста многократно копирует одну и ту же функцию, меняя лишь типы. В нашем примере это компилятор создал три _специализации шаблона:_

```c++
int max(int a, int b) { return a < b ? b : a; }

char max(char a, char b) {return a < b ? b : a; }

double max(double a, double b) { return a < b ? b : a; }
```

Порой без подсказки компилятор не может самостоятельно вывести все типы, чтобы инстанцировать шаблон. Тогда их требуется явно указать _при вызове_ функции. В случае с `max()` это избыточно: компилятор сам определяет тип `T`, ведь это тип переданных в функцию аргументов. Тем не менее, мы можем задать аргумент шаблона явно, передав его в треугольных скобках:

```c++ {.example_for_playground .example_for_playground_018}
max<int>(5, 6);
max<char>('z', 'a');
max<double>(1.01, 1.02);
```

Напишите шаблонную функцию `str_none_of()`. Функция должна принимать строку и предикат. Предикат — это функция, возвращающая `true` либо `false`. Функция `str_none_of()` должна вернуть `true`, если предикат не выполняется ни над одним символом строки. {.task_text #block-predicate}

Тип предиката — это и есть параметр шаблона. {.task_text}


Например, у нас есть строка `std::string s="generic"` и предикат `bool is_a(char c) {return c == 'a'; }`. Вызов `str_none_of(s, is_a)` должен вернуть `true`. {.task_text}

```c++ {.task_source #cpp_chapter_0050_task_0090}
```
У шаблона должен быть единственный параметр, например `class Fn`. Тогда функция будет выглядеть так: `bool str_none_of(std::string s, Fn pred)`. В ней в цикле по каждому символу `c` строки `s` нужно применить предикат: `pred(c)`. Если он хотя бы раз вернул `true`, функция возвращает `false`. {.task_hint}
```c++ {.task_answer}
template<class Fn>
bool str_none_of(std::string s, Fn pred)
{
    for (char c: s)
    {
        if (pred(c))
            return false;
    }

    return true;
}
```

Шаблонными могут быть не только функции, но и классы. Например, все контейнеры из стандартной библиотеки — это шаблоны!

----------

## Резюме

- Перегрузка — это создание функций с одинаковым именем и разным набором параметров.
- Оператор `throw` и конструкция `try-catch` реализуют механизм исключений.
- Пространства имен нужны для структурирования кода и предотвращения конфликта имен.
- Перечисление — это тип, значения которого ограничены набором именованных констант.
- Классы позволяют описывать абстракции, отделять интерфейс для работы с ними от внутренней реализации, объединять данные и методы их обработки.
- Конструкторы и деструкторы классов позволяют реализовать идиому RAII.
- Структуры подходят для группировки полей, изменение которых по отдельности не приведет объект в неконсистентное состояние.
- Шаблоны нужны для написания обобщенного кода без привязки к конкретным типам.
