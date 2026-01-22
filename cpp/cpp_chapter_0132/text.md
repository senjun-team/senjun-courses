# Глава 13.2. Инициализация полей классов

В этой главе мы разберём, как правильно инициализировать поля классов.

Инициализация по умолчанию (default-initialization) объектов классов приводит к вызову конструктора по умолчанию. Если этот конструктор не задан разработчиком явно, то компилятор его создаёт. Сгенерированный конструктор вызывает _конструкторы по умолчанию_ для всех полей класса.

Но поле может быть встроенного типа, например, `char` или `int`, и не иметь конструктора. Тогда при инициализации по умолчанию в него не будет записано никакого значения. В поле будет лежать любой мусор:

```cpp  {.example_for_playground}
import std;

struct Context
{
    bool is_logged_in;
    int connections_count;
};

int main()
{
    Context ctx; // Поля инициализируются мусором

    const int n = ctx.connections_count; // UB
}
```

Никогда не оставляйте поля не инициализированными. Поэтому вместо инициализации по умолчанию используйте другие способы.

## Default member initialization: прямая инициализация полей {#block-dmi}

Предпочтительный способ инициализации полей вам уже [известен.](/courses/cpp/chapters/cpp_chapter_0050/#block-class-init-field-example) Вы его применяли, хотя и не знали, что он называется [default member initialization](https://en.cppreference.com/w/cpp/language/data_members#Member_initialization) (DMI) или прямая инициализация полей.

Это инициализация поля по месту его объявления:

```cpp   {.example_for_playground}
import std;

struct Context
{
    // DMI, копирующая инициализация:
    bool is_logged_in = true;
    int connections_count = 1;
};

int main()
{
    Context ctx; // Поля корректно инициализируются

    const int n = ctx.connections_count; // 1
}
```

Вместо копирующей инициализации через оператор `=` поля можно инициализировать универсально:

```cpp
struct Context
{
    // DMI, универсальная инициализация:
    bool is_logged_in{true};
    int connections_count{1};
};
```

Прямая инициализация полей появилась в C++11. Она работает по правилам:
- Поля инициализируются в порядке своего объявления. В примере выше сначала инициализируется `is_logged_in`, а затем `connections_count`.
- Это происходит _до_ вызова [конструктора.](/courses/cpp/chapters/cpp_chapter_0050/#block-constructors-destructors})
- Значение, заданное полю через прямую инициализацию, можно переопределить в конструкторе.

Мы подходим к следующему способу инициализации полей — через конструктор.

## Member initializer list: список инициализации конструктора {#block-member-initializer-list}

Конструктор класса или структуры нужен для корректной инициализации объекта. В первую очередь для установки полей в требуемые значения. До сих пор мы [инициализировали поля](/courses/cpp/chapters/cpp_chapter_0050/#block-task-7) прямо в теле конструктора:

```cpp   {.example_for_playground}
import std;

struct SessionInfo
{
    // Конструктор по умолчанию
    SessionInfo()
    {
        requests_count = 0;
        user_id = 1;
    }

    // Конструктор с параметрами
    SessionInfo(std::size_t requests, std::size_t user)
    {
        requests_count = requests;
        user_id = user;
    }

    std::size_t requests_count;
    std::size_t user_id;
};

int main()
{
     // Поля инициализируются конструктором по умолчанию:
    SessionInfo info1;

    // Поля инициализируются конструктором с параметрами:
    SessionInfo info2(3, 607);
    SessionInfo info3{3, 607};
}
``` 

На практике значения полей задаются в теле конструктора, только если они определяются по сложным правилам, которые не уместить в одно выражение. В остальных случаях [используется](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#rc-initialize) способ инициализации под названием [member initializer list](https://en.cppreference.com/w/cpp/language/constructor) (список инициализации полей). Он появился ещё в C++98.

Перепишем наш пример с использованием списка инициализации полей:

```cpp    {.example_for_playground .example_for_playground_001}
struct SessionInfo
{
    SessionInfo()
    { }

    SessionInfo(std::size_t requests, std::size_t user)
    : requests_count(requests), user_id(user) // Member initializer list
    { }

    std::size_t requests_count = 0;
    std::size_t user_id = 1;
};
``` 

Список инициализации указывается _после_ круглых скобок с параметрами конструктора и _до_ его тела. В нем после оператора `:` через запятую перечисляются поля с присваиваемыми им в скобках значениями. Можно использовать круглые скобки для прямой инициализации, а можно фигурные для универсальной:

```cpp
SessionInfo() : requests_count{0}, user_id{1}
{ }
```

Запомните правило: поля инициализируются в порядке своего объявления. А вовсе не в порядке перечисления в списке инициализации полей. Если они не совпадают, компилятор выдаёт [предупреждение:](https://clang.llvm.org/docs/DiagnosticsReference.html#wreorder-ctor)

```cpp    {.example_for_playground .example_for_playground_002}
struct Message
{
    Message(std::string message, std::time_t updated, std::time_t created)
    : text(message), time_updated(updated), time_created(created)
    { }

    std::time_t time_created = 0;
    std::time_t time_updated = 0;

    std::string text;
};
```
```
main.cpp:8:7: warning: initializer order does not match the declaration order
    8 |     : text(message), time_updated(updated), time_created(created)
      |       ^~~~~~~~~~~~~                         ~~~~~~~~~~~~~~~~~~~~~
      |       time_created(created)                 text(message)
```

Часть полей в списке инициализации конструктора может быть опущена. Это допустимо, если конструктор не имеет параметра, который бы влиял на поле, а само поле имеет адекватный конструктор по умолчанию:

```cpp    {.example_for_playground .example_for_playground_003}
struct Message
{
    Message(std::string message, std::time_t updated, std::time_t created)
    : time_created(created), time_updated(updated), text(message)
    { }

    // Поля time_created и time_updated уже инициализированы через DMI,
    // а эта перегрузка конструктора устанавливает только поле text
    Message(std::string message) : text(message)
    { }

    std::time_t time_created = 0;
    std::time_t time_updated = 0;

    std::string text;
};
```

У списка инициализации конструктора есть недостаток. Если в классе несколько полей и перегрузок конструктора, то при удалении или добавлении поля легко ошибиться и сформировать неправильный список инициализации. Если поля имеют одинаковый тип или в игру вступает неявное приведение типов, то закравшуюся ошибку можно очень долго не замечать. Поэтому для инициализации полей константными значениями [выбирайте](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#rc-in-class-initializer) DMI вместо списка инициализации конструктора.

Список инициализации конструктора напрашивается в практике [«LRU кеш».](/courses/cpp/practice/cpp_lru_cache/) Используйте его и убедитесь, что тесты по-прежнему проходят.

Что выведет этот код?  {.task_text}

Напишите `err` в случае ошибки компиляции или `ub` в случае неопределённого поведения. {.task_text}

```cpp {.example_for_playground}
import std;

class Storage
{
public:
    int val;

    Storage(): val(val)
    { }
};

int main()
{
    Storage s{};
    std::println("{}", s.val);
}
```

```consoleoutput {.task_source #cpp_chapter_0132_task_0010}
```
В прошлой главе вы уже [сталкивались](/courses/cpp/chapters/cpp_chapter_0131/#block-use-not-initialized) с подобным кодом, в котором в выражении участвовал ещё не проинициализированный объект. В этом коде поле класса иницилизируется собой. {.task_hint}
```cpp {.task_answer}
ub
```

## Designated-initialization: назначенная инициализация

В некоторых случаях при инициализации объектов классов полезно _видеть имена полей,_ которым передаются значения. Это особенно актуально, если:
- Не очевидно, как поля называются. `Settings s{cur_val, prev_val};` — здесь названия переменных вовсе не подсказывают, какие поля мы заполняем. `GeoPoint p{45.5, 43.0};` — первой идёт широта или долгота координаты?
- Полей слишком много, и в них легко запутаться.
- Часть полей можно пропустить, чтобы инициализировать значениями по умолчанию.

На помощь приходит [назначенная инициализация](https://en.cppreference.com/w/cpp/language/aggregate_initialization.html#Designated_initializers) (designated-initialization). Она появилась в C++20 и позволяет в инициализаторе перечислять имена полей:

```cpp    {.example_for_playground}
import std;

struct GeoPoint
{
    double lat{0.0};
    double lon{0.0};
};

int main()
{
    // Назначенная инициализация:
    GeoPoint p{.lat=-27.1193, .lon=-109.3546};
}
```

Эта инициализация работает только для [агрегатов](https://timsong-cpp.github.io/cppwp/n4868/dcl.init.aggr#:initialization,aggregate) (aggregate). Два важных, но не единственных свойства, которыми должен обладать агрегат:
- Все поля открыты (`public`).
- Нет определённых пользователем конструкторов.

Если добавить в структуру `GeoPoint` хотя бы пустой конструктор, то она перестанет быть агрегатом. И применение назначенной инициализации приведёт к ошибке компиляции:

```cpp    {.example_for_playground}
import std;

struct GeoPoint
{
    GeoPoint() {} // Этот конструктор ничего не делает
    double lat{0.0};
    double lon{0.0};
};

int main()
{
    // Назначенная инициализация:
    GeoPoint p{.lat=1.0, .lon=6.0};
    std::println("{} {}", p.lat, p.lon);
}
```
```
main.cpp:13:14: error: initialization of non-aggregate type 'GeoPoint' with a designated initializer list
   13 |     GeoPoint p{.lat=1.0, .lon=6.0};
      |          
```

Передавайте поля в инициализатор строго в том порядке, в котором они объявлены в классе. Иначе компилятор выдаст предупреждение. А в случае переданного флага `-Werror` завершит сборку с ошибкой:

```cpp
GeoPoint p{.lon=6.0, .lat=1.0}; // lat и lon идут в неправильном порядке
```
```
error: ISO C++ requires field designators to be specified in declaration order; field 'lon' will be initialized after field 'lat' [-Werror,-Wreorder-init-list]
   12 |     GeoPoint p{.lon=6.0, .lat=1.0};
```

Поля в инициализаторе нельзя менять местами, но часть из них можно пропустить. Пропущенным полям должны быть заданы значения по умолчанию через прямую инициализацию полей, иначе при обращении к ним вас ждёт UB.

```cpp    {.example_for_playground}
import std;

struct ConnInfo
{
    std::string unix_socket_path;
    std::string address = "::";
    std::size_t port = 0;
};

int main()
{
    // Инициализация поля address значением по умолчанию
    ConnInfo ci{.unix_socket_path="/tmp/p0fsock", .port=7689};
}
```

Вместо копирующей инициализации полей можно использовать универсальную:

```cpp
 ConnInfo ci{.unix_socket_path{"/tmp/p0fsock"}, .port{7689}};
```

Что выведет этот код?  {.task_text}

Напишите `err` в случае ошибки компиляции или `ub` в случае неопределённого поведения. {.task_text}

```cpp {.example_for_playground}
import std;

class CharacterRange
{
public:
  std::size_t lo{0};
private:
  std::size_t hi{0};
};

int main()
{
    CharacterRange range{.lo=0x1F0CF};
    std::println("{}", range.lo);
}
```

```consoleoutput {.task_source #cpp_chapter_0132_task_0020}
```
Класс `CharacterRange` не является агрегатом, потому что имеет закрытые (`private`) поля. {.task_hint}
```cpp {.task_answer}
err
```

----------

## Резюме

Подытожим главу рекомендациями по выбору способа инициализации полей классов.

Всегда инициализируйте поля. Обращение к неинициализированному полю — это UB. Чтобы инициализировать поле константой - значением по умолчанию, применяйте прямую инициализацию полей. Чтобы инициализировать поле в конструкторе, используйте список инициализации конструктора.

Если структура или класс является агрегатом и вы считаете, что уместно было бы явно прописать имена полей, пользуйтесь назначенной инициализацией:

```cpp
// designated-initialization: назначенная инициализация
std::pair<int, char> p{.first = 1, .second = 'A'};
```
