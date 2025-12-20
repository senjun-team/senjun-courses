# Глава 14.2. Составные типы

Если тип не относится к фундаментальным, то он считается составным. Вы уже имеете представление про некоторые из составных типов: [перечисления,](/courses/cpp/chapters/cpp_chapter_0050/#block-enum) [структуры,](/courses/cpp/chapters/cpp_chapter_0050/#block-struct) [классы](/courses/cpp/chapters/cpp_chapter_0050/#block-class) и [функции.](/courses/cpp/chapters/cpp_chapter_0020/#block-functions)


![Составные типы](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-14/illustrations/cpp/types_compound.jpg) {.illustration}


В этой главе кратко обсудим уже известные вам составные типы и познакомимся с остальными.


## Перечисления

Прямо или косвенно, но любой составной тип опирается на фундаментальный. Так, [перечисление](/courses/cpp/chapters/cpp_chapter_0050/#block-enum) — это тип, значения которого ограничены набором именованных констант. Каждую из них компилятор соотносит целому числу. Его тип называется базовым типом перечисления (underlying type). Он зависит от реализации компилятора, но может указываться явно.

Базовый тип задается после двоеточия в определении перечисления:

```cpp
enum class QuotaType : std::uint8_t
{
    Queries,
    ExecTime
};
```

В каких случаях имеет смысл явно прописывать базовый тип? Например, если перечисление нужно сериализовывать или хочется сэкономить память:

```cpp  {.example_for_playground}
import std;

enum class IpVersion // Базовый тип по умолчанию
{
    Ipv4,
    Ipv6,
    Ipv4_or_v6
};

enum class AddressType : std::uint8_t // Определяем базовый тип
{
    IpAddress,
    BroadcastAddress,
    SubnetMask
};

int main()
{
    std::println("Size of IpVersion: {} bytes\nSize of AddressType: {} byte",
                 sizeof(IpVersion), sizeof(AddressType));
}
```
```
Size of IpVersion: 4 bytes
Size of AddressType: 1 byte
```

Перечисление `Facility` содержит 261 константу по количеству компонентов системы, для которых нужно собирать метрики. Какой базовый тип подойдет для `Facility` лучше всего? Выберите один из целочисленных типов [фиксированной ширины.](/courses/cpp/chapters/cpp_chapter_0141/#block-fixed-width) {.task_text}

```consoleoutput {.task_source #cpp_chapter_0142_task_0010}
```
Для хранения 261 значения (от 0 до 260) требуется 9 бит. {.task_hint}
```cpp {.task_answer}
std::uint16_t
```

Базовый тип в комбинации с присваиванием конкретных значений константам дает интересные результаты:

```cpp  {.example_for_playground}
import std;

enum class AccessType : char
{
    Read = 'r',
    Write = 'w',
    Exec = 'e'
};

int main()
{
    std::println("{}", std::to_underlying(AccessType::Write));
}
```
```
w
```

Символьный тип `char` является целочисленным, поэтому может выступать в качестве базового для перечисления. В этом примере мы вызвали появившуюся в C++23 функцию [std::to_underlying()](https://en.cppreference.com/w/cpp/utility/to_underlying.html). Она приводит значение перечисления к базовому типу. До C++23 с той же целью приходилось выполнять приведение типов через `static_cast`:

```cpp
std::println("{}", static_cast<char>(AccessType::Write));
```

Функция `std::to_underlying()` более удобна, чем `static_cast`, потому что не требует явного указания базового типа. А значит, она:
- Не приводит к опасному сужающему преобразованию, если базовый тип был изменен на более широкий.
- Может быть использована при написании обобщенного кода в шаблонах.

Так выглядит использование `std::to_underlying()` в шаблонной функции:

```cpp  {.example_for_playground}
import std;

enum ServerState { down, starting, ready, stopping };
enum HttpPort : std::uint16_t { def = 80, tls = 443 };

template <class T> 
void print_enum_value(std::string name, T value)
{
    std::println("{}: {}", name, std::to_underlying(value));
}

int main()
{
    print_enum_value("down", ServerState::down);
    print_enum_value("ready", ServerState::ready);
    print_enum_value("tls", HttpPort::tls);
}
```
```
down: 0
ready: 2
tls: 443
```

Что выведет этот код? {.task_text}

Напишите `err` в случае ошибки компиляции или `?`, если поведение не определено стандартом и зависит от реализации. {.task_text}

```cpp {.example_for_playground}
import std;

using TaskId = int;
using TaskPool = std::vector<TaskId>;

enum Priority {low, high};

int main()
{
    std::array<TaskPool, 2> tasks = {
        TaskPool{46, 32},
        TaskPool{15}
    };

    std::size_t tasks_to_exec = tasks[std::to_underlying(Priority::low)].size() +
                                tasks[std::to_underlying(Priority::high)].size();

    std::println("{}", tasks_to_exec);
}
```

```consoleoutput {.task_source #cpp_chapter_0142_task_0020}
```
Если значение первой константны перечисления не задано, то оно [равно нулю.](/courses/cpp/chapters/cpp_chapter_0050/#block-enum-const-values) Значение следующей константы равно значению предыдущей, увеличенному на единицу. {.task_hint}
```cpp {.task_answer}
3
```

## Функции

У функции может быть как [определение, так и объявление.](/courses/cpp/chapters/cpp_chapter_0100/#block-declaration-definition) Объявление состоит из типа возвращаемого значения, имени функции и типов параметров:

```cpp
bool exists(std::string path);
```

Как вы помните, объявления может и не быть: достаточно только определения. Имена параметров в объявлении _опциональны:_

```cpp  {.example_for_playground}
import std;

bool exists(std::string);  // Объявление

int main()
{
    std::vector<std::string> pathes = {"/tmp", "/tmp/dump.csv"};
    for(std::string p: pathes)
    {
        std::println("{} exists: {}", p, exists(p));
    }
}

bool exists(std::string path)  // Определение
{
    return std::filesystem::exists(path);
}
```
```
/tmp exists: true
/tmp/dump.csv exists: false
```

### Сигнатура функции

У каждой функции есть [сигнатура.](https://timsong-cpp.github.io/cppwp/n4868/defns.signature) Это часть объявления, в которую входит пространство имен функции, ее имя и типы параметров. Тип возвращаемого значения _не входит_ в сигнатуру. Дело в том, что у функции могут быть [перегрузки,](/courses/cpp/chapters/cpp_chapter_0050/#block-overloading) отличающиеся набором параметров. И именно сигнатура помогает компилятору выбрать нужную перегрузку по каждому месту вызова.

```cpp
import std;

namespace utils
{
    // Сигнатура: utils::run_service(std::string, bool)
    void run_service(std::string config, bool hot_reload)
    {
        // ...
    }

    // Сигнатура: utils::run_service(std::unordered_map<std::string, std::string>)
    void run_service(std::unordered_map<std::string, std::string> params)
    {
        // ...
    }
}

int main()
{
    utils::run_service("./stage_conf.yaml", true);
}
```

### Тип функции

Помимо сигнатуры у каждой функции есть тип. Он состоит из типа возвращаемого значения и типов параметров. Обычно он записывается в формате:

```cpp
return_type(type_param_1, type_param_2, ..., type_param_n)
```

Так выглядит тип функции `void run_service(std::string config, bool hot_reload)`:

```cpp
void(std::string, bool)
```

Напишите тип функции, которая ничего не возвращает и не принимает параметров. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0142_task_0030}
```
Если функция ничего не возвращает, то тип возвращаемого значения `void`. Если функция не принимает параметров, то внутри круглых скобок ничего не указывается. {.task_hint}
```cpp {.task_answer}
void()
```

Если сигнатура в первую очередь нужна компилятору, то для чего полезен тип? В C++ функция не является [объектом первого класса.](https://ru.wikipedia.org/wiki/%D0%9E%D0%B1%D1%8A%D0%B5%D0%BA%D1%82_%D0%BF%D0%B5%D1%80%D0%B2%D0%BE%D0%B3%D0%BE_%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B0) Ее нельзя _напрямую_ присвоить переменной, передать аргументом или вернуть как значение. Но есть способы, позволяющие добиться _практически_ этого:
- Обернуть функцию в класс [std::function](https://en.cppreference.com/w/cpp/utility/functional/function.html) и работать с объектом этого класса.
- Работать с переменной — ссылкой на функцию.
- Работать с переменной — указателем на функцию.
- Вместо обычной функции использовать лямбда-функцию.

Каждый из этих способов явно или неявно задействует тип функции. Остановимся на варианте с `std::function`, а остальные рассмотрим позже.

При оборачивании функции в шаблонный класс [std::function](https://en.cppreference.com/w/cpp/utility/functional/function.html) ее тип становится аргументом шаблона. А объект класса присваивается переменной, которую можно абсолютно законно возвращать из другой функции или передавать в нее аргументом:

```cpp   {.example_for_playground}
import std;

bool is_valid(double val)
{
    return !std::isnan(val) && !std::isinf(val);
}

int main()
{
    std::function<bool(double)> check{is_valid};

    std::println("{}", check(-2.2));
}
```
```
true
```

Реализуйте функцию `sum_if()`, которая принимает два параметра: `data` и `pred`.  {.task_text} 

Параметр `data` — это словарь `std::map` с ключами - строками и значениями типа `std::size_t`. {.task_text}

Параметр `pred` - функция-предикат, принимающая строку и возвращающая `bool`.  {.task_text}

Функция `sum_if()` должна вернуть сумму значений элементов словаря `data`, для ключей которых предикат `pred` возвращает `true`. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0040}


```
Объявление функции: `std::size_t sum_if(std::map<std::string, std::size_t> data, std::function<bool(std::string)> pred);`. {.task_hint}
```cpp {.task_answer}
std::size_t sum_if(std::map<std::string, std::size_t> data,
                   std::function<bool(std::string)> pred)
{
    std::size_t res = 0;

    for (auto it = data.cbegin(); it != data.cend(); ++it)
    {
        if (pred(it->first))
            res += it->second;
    }

    return res;
}
```

С помощью `std::function` удобно организуется логика, основанная на коллбэках. Коллбэк — это функция, которая передается другой функции, чтобы ее запустили в ответ на определенное событие:

```cpp   {.example_for_playground .example_for_playground_001}
import std;

// Для удобства определяем псевдоним типа функции
using ActionCallback = void(std::string);

// Второй параметр - объект std::function
void ask_confirmation(std::string prompt, std::function<ActionCallback> callback)
{
    if (!prompt.empty())
        std::println("{}", prompt);
    
    std::string user_input;
    std::getline(std::cin, user_input);

    callback(user_input);
}

void on_update(std::string input)
{
    if (input == "Y")
        std::println("Updating...");
    else
        std::println("Cancelled update");
}

void on_search(std::string input)
{
    std::println("Searching for package {}...", input);
}

int main()
{
    // При подстановке второго аргумента происходит
    // неявный вызов конструктора std::function
    ask_confirmation("Update? Y/N", on_update);
    ask_confirmation("", on_search);
}
```
```
Update? Y/N
Updating...
Searching for package astroterm...
```

## Классы и структуры

Разработчикам, пришедшим в C++ из таких языков как Python, нужно запомнить несколько важных фактов про классы.

Тип — это более широкое понятие, чем класс. Иными словами, классы — это подмножество типов. Любой класс можно также назвать типом, но не любой тип — это класс. Например, `char` — это тип, но уж точно не класс.

Классы — это более сложные абстракции, чем фундаментальные типы. Сравним их.

Фундаментальные типы:
- Максимально близки к аппаратному представлению чисел.
- Переменная фундаментального типа — это всего лишь небольшая область памяти, которую компилятор трактует определенным образом.
- Большинство операций над фундаментальными типами сводится к единственной машинной команде.

Классы:
- Имеют конструктор и деструктор.
- Методы для доступа к полям могут содержать дополнительную логику, а не просто возвращать или присваивать значение.
- Все поля экземпляра класса располагаются в единой области памяти. Однако поля могут указывать на другие области динамической памяти, управление которой реализуется методами класса.
- Поля располагаются в памяти в том же порядке, в котором их объявили.

Перед вами класс `File`, хранящий секции файла некоего формата. {.task_text}

Под объявлением класса добавьте определение метода `find_section()`. Он возвращает пару: первую секцию файла, для которой предикат `pred` вернул `true`, и ее индекс. Если такой секции нет, функция возвращает пустую секцию и индекс, равный `std::numeric_limits<std::size_t>::max()`. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0050}
struct Section
{
    std::string header;
    std::string name;
    std::string contents;
};

class File
{
public:
    explicit File(std::vector<Section> sections);
    std::size_t sections_count();
    std::pair<Section, std::size_t> find_section(std::function<bool(Section, std::size_t)> pred);
       
private:
    std::vector<Section> m_sections;
};

// Добавьте определение метода

```
При определении метода класса не забудьте указать имя класса: `std::pair<Section, std::size_t> Section::find_section(std::function<bool(Section, std::size_t)> pred) { /* ... */ }`. {.task_hint}
```cpp {.task_answer}
struct Section
{
    std::string header;
    std::string name;
    std::string contents;
};

class File
{
public:
    explicit File(std::vector<Section> sections);
    std::size_t sections_count();
    std::pair<Section, std::size_t> find_section(std::function<bool(Section, std::size_t)> pred);
       
private:
    std::vector<Section> m_sections;
};

std::pair<Section, std::size_t> 
File::find_section(std::function<bool(Section, std::size_t)> pred)
{
    for(std::size_t i = 0; i < m_sections.size(); ++i)
    {
        if (pred(m_sections[i], i))
            return {m_sections[i], i};
    }

    return {{}, std::numeric_limits<std::size_t>::max()};
}
```


## Массивы

Вы уже поработали с массивами, реализованными как шаблонные классы стандартной библиотеки. Контейнер `std::vector` — это динамический массив, а `std::array` — статический.

В C++ есть и встроенный тип данных «массив», для которого предусмотрена специальная конструкция языка. Такие массивы называют традиционными или сишными, потому что они достались C++ в наследие от Си. 

Перед вами синтаксис объявления массива с именем `name`, состоящего из `n` элементов типа `T`:

```cpp
T name[n];
```

Такой массив по сути — непрерывная область памяти, которую компилятор трактует как последовательность элементов конкретного типа. Ее размер определяется количеством элементов и их типом. Чтобы компилятор мог выделить под массив память, размер массива должен быть известен в момент компиляции, то есть быть константным.

Откройте этот код в плэйграунде, уберите у переменной `len` константность и убедитесь, что создать массив неконстантной длины невозможно.

```cpp  {.example_for_playground .example_for_playground_002}
const std::size_t len = 3;
float samples[len];

samples[0] = 4.5;
samples[1] = 2.0;
samples[2] = -1.1;
std::println("{}", samples);
```
```
[4.5, 2, -1.1]
```

В этом примере мы заполнили массив значениями сразу после создания. Если этого не сделать, то элементы будут [инициализированы по умолчанию.](/courses/cpp/chapters/cpp_chapter_0131/#block-default-initialization) Для фундаментальных типов это означает _отсутствие_ инициализации, которое приведет к UB на строке с выводом массива в консоль.

Всегда инициализируйте элементы массива. Не допускайте UB.

Так выглядит заполнение элементов в момент инициализации:

```cpp
float samples[3] = {4.5, 2.0, -1.1};
```

Если длина списка инициализации меньше длины массива, оставшиеся элементы заполняются нулями (даже если 0 не является корректным значением для типа).

```cpp  {.example_for_playground .example_for_playground_003}
float samples[3] = {4.5};
std::println("{}", samples);
```
```
[4.5, 0, 0]
```

Что выведет этот код? {.task_text}

Напишите `err` в случае ошибки компиляции или `ub` в случае неопределенного поведения. {.task_text}

```cpp {.example_for_playground}
import std;

enum class WindingOrder
{
  Clockwise = 1,
  CounterClockwise
};

int main()
{
    WindingOrder wo[1000] = {WindingOrder::Clockwise};

    std::println("{}", std::to_underlying(wo[999]));
}
```

```consoleoutput {.task_source #cpp_chapter_0142_task_0060}
```
Если длина списка инициализации меньше длины массива, оставшиеся элементы заполняются нулями. 0 не является корректным значением для `WindingOrder`, но это не UB. Такой код скомпилируется. {.task_hint}
```cpp {.task_answer}
0
```

Чтобы инициализировать массив значениями по умолчанию, скобки оставляют пустыми:

```cpp  {.example_for_playground .example_for_playground_004}
std::string labels[4] = {};

std::println("{}", labels);
```
```
["", "", "", ""]
```

При наличии списка инициализации размер массива можно опустить: компилятор его выведет автоматически.

```cpp
int offsets[] = {-1, 0, -1, 2}; // автоматический вывод длины
```

Реализуйте функцию `is_sorted()`, которая принимает сишный массив, его длину и [компаратор.](/courses/cpp/chapters/cpp_chapter_0082/#block-comparator) Функция должна вернуть `true`, если массив отсортирован в соответствии с компаратором. Массив не может быть пустым. {.task_text}

Пример вызова функции от массива из 4-х элементов и предиката `std::less()`: `is_sorted(arr, 4, std::less())`. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0070}
bool is_sorted(int arr[], std::size_t n, std::function<bool(int, int)> comp)
{

}
```
Функция должна возвращать `false`, если `comp()` вернул `false` для двух последовательных элементов. {.task_hint}
```cpp {.task_answer}
bool is_sorted(int arr[], std::size_t n, std::function<bool(int, int)> comp)
{
    if (n == 0)
        return true;
    
    for(std::size_t i = 0; i < n - 1; ++i)
    {
        if (!comp(arr[i], arr[i+1]))
            return false;
    }
    
    return true;
}
```

### Длина массива

Давайте получим длину массива, разделив его размер на размер типа элемента:

```cpp  {.example_for_playground .example_for_playground_005}
int offsets[] = {-1, 0, -1, 2};

std::size_t len = sizeof(offsets) / sizeof(int);

std::println("{}", len);
```
```
4
```

Для получения размера массива `offsets` в байтах мы применили к нему оператор [sizeof](https://en.cppreference.com/w/cpp/language/sizeof.html). В предыдущей главе мы [использовали](/courses/cpp/chapters/cpp_chapter_0141/#block-sizeof) `sizeof` для того, чтобы узнать, сколько байт выделяется под тип. Но `sizeof` можно применять не только для типов, но и для переменных.

Деление размера массива на размер его элемента — это способ определения длины, который работает во всех версиях C++, но не отличается удобством. В C++17 появилась функция [std::size()](https://en.cppreference.com/w/cpp/iterator/size.html), упрощающая задачу:

```cpp  {.example_for_playground .example_for_playground_006}
int offsets[] = {-1, 0, -1, 2};

std::size_t len = std::size(offsets);

std::println("{}", len);
```
```
4
```

Оба способа получения длины **не работают** внутри функций, в которые массив передается. Виной тому низведение массива (array to pointer decay) — неявное приведение сишного массива к указателю на его первый элемент. Указатель — это переменная, которая хранит адрес другой переменной. Низведение массива происходит при передаче его в функцию:

```cpp   {.example_for_playground}
import std;

void print_size(int data[])
{
   std::println("{}", sizeof(data) / sizeof(int)); // Ошибка
}

int main()
{
    int arr[] = {100, 200, 400, 600};

    std::println("{}", sizeof(arr) / sizeof(int)); // Ок
    
    print_size(arr);
}
```
```
main.cpp:5:29: error: sizeof on array function parameter will return size of 'int *' instead of 'int[]' [-Werror,-Wsizeof-array-argument]
    5 |    std::println("{}", sizeof(data) / sizeof(int));
```

Поэтому при передаче массива в функцию вместе с ним передается дополнительный парметр — длина.

### Многомерные массивы

Сишные массивы могут быть многомерными.

Так выглядит создание и инициализация двумерного массива `measurements`:

```cpp    {.example_for_playground .example_for_playground_007}
double measurements[2][3] = {
    // accelerometer axis
    // x    y      z 
    {0.2,  0.0,  -0.11},  // previous measurement
    {0.19, 0.09, -0.15}   // current  measurement
};
```

Мы получили массив, каждый элемент которого — это замер, полученный с акселерометра. У этого массива 2 элемента для хранения актуального и предыдущего замеров. Каждый замер — это массив из 3-х чисел типа `double`: ускорения по осям x, y, z.

Выведем значения массива `measurements` в консоль: 

```cpp   {.example_for_playground .example_for_playground_008}
const std::size_t n = 2;
const std::size_t m = 3;

double measurements[n][m] = {
    {0.2,  0.0,  -0.11},
    {0.19, 0.09, -0.15}
};

for (std::size_t i = 0; i < n; ++i)
{
    std::println("Accelerometer measurement #{}", i);
    for (std::size_t j = 0; j < m; ++j)
    {
        std::print("{}  ", measurements[i][j]);
    }
    std::println("");
}
```
```
Accelerometer measurement #0
0.2  0  -0.11  
Accelerometer measurement #1
0.19  0.09  -0.15  
```

Многомерные массивы хранятся в [построчном порядке](https://en.wikipedia.org/wiki/Row-_and_column-major_order) (row-major order). Это означает, что элементы каждой строки идут в памяти последовательно, один за другим, а затем следуют элементы новой строки. Построчный порядок применяется в C++, Си, Python и многих других языках. А например в MATLAB реализован столбцовый порядок (column-major order), при котором подряд идут элементы столбцов, а не строк.


![Построчный и столбцовый порядок](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-142/illustrations/cpp/row_and_column_major_order.jpg) {.illustration}



### Недостатки массивов

Сишным массивам присущи серьезные недостатки.

Такой массив «не знает» своей длины. Ее хранение или вычисление становится ответственностью разработчика. Так, при передаче массива в функцию вместе с ним отправляется и еще один параметр — длина. Если массив двумерный, то дополнительных параметров уже два.

Кроме того, отсутствует удобный способ избежать случайного выхода за границы сишного массива. Зато в контейнере `std::array` такой способ есть. Метод `at()`, в отличие от оператора `[]`, проверяет индекс и в случае выхода за границы бросает исключение.

Сишные массивы нельзя присваивать друг другу и сравнивать через операторы `=` и `==`. Все присваивания и сравнения выполняются поэлементно, в цикле.


### Советы по работе с массивами

Главный совет по работе с сишными массивами: **не используйте их** без явной на то необходимости. Предпочитайте контейнеры стандартной библиотеки.

И не верьте распространенному заблуждению, что сишные массивы эффективнее `std::array`. Контейнер `std::array` — одна из [абстракций с нулевой стоимостью,](/courses/cpp/chapters/cpp_chapter_0010/#block-zero-overhead) которую компилятор прекрасно оптимизирует.

Если в ваш код так или иначе передается _одномерный_ сишный массив, вы можете превратить его в `std::array` с помощью появившейся в C++20 функции [std::to_array()](https://en.cppreference.com/w/cpp/container/array/to_array.html).

Также с сырым массивом можно безопасно работать через класс-обертку [std::span](https://en.cppreference.com/w/cpp/container/span.html) (срез).

Шаблонный класс `std::span<T>` — это «окно» на любую последовательность элементов типа `T`, будь то сырой массив или контейнер. 

```cpp   {.example_for_playground}
import std;

int main()
{
    int arr[] = {6, 3, 2};
    std::span<int> s{arr};

    std::println("1st elem: {}. last: {}. Size: {}",
                 s.front(), s.back(), s.size());
}
```
```
1st elem: 6. last: 2. Size: 3
```

Срезы позволяют переиспользовать одни и те же функции для обработки сишных массивов и контейнеров:

```cpp   {.example_for_playground}
import std;

int count_zeroes(std::span<int> data)
{
    int res = 0;

    for(auto val: data)
    {
        if(val == 0)
            ++res;
    }

    return res;
}

int main()
{
    std::vector<int> v{9, 0, 0, -1, 0};
    std::println("{}", count_zeroes(v));

    int arr[] = {0, 2, 4, 0};
    std::println("{}", count_zeroes(arr));
}
```
```
3
2
```

Работать со срезами на порядок удобнее и безопаснее, чем с сырыми массивами. Помимо прочего у них есть:
- методы для получения итераторов,
- оператор `[]` для взятия элемента по индексу,
- метод `at()` для получения элемента по индексу с проверкой границ,
- методы `front()` и `back()` для взятия первого и последнего элемента,
- методы `size()` и `empty()` для работы с размером массива.

Напишите функцию `max_profit()`, на вход которой подается срез `prices`. Значение `prices[i]` описывает цену акций на `i`-ый день торгов. Нужно максимизировать прибыль, выбрав наиболее подходящие дни для покупки и продажи акций. День для продажи должен идти _после_ дня покупки. {.task_text}

Верните максимальную прибыль, которую можно получить. Если прибыль невозможна, верните 0. {.task_text}

Например, в функцию переданы цены акций за пять дней: [8, 1, 3, 2, 4, 1]. Максимальная прибыль составит 3: акции будут куплены по цене 1 и проданы по цене 4. А если в функцию переданы цены [5, 4, 1], то она должна вернуть 0, потому что нельзя продать акции по более выгодной цене, чем они были куплены. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0080}
int max_profit(std::span<int> prices)
{

}
```
Заведите две переменные типа `int`: `profit`, изначально равную нулю, и `buy_price`, изначально равную цене акций в первый день (нулевой элемент среза). Затем в цикле с первого по последний элемент среза сопоставляйте эти значения с теми, которые могут быть в `i`-ый день торгов. {.task_hint}
```cpp {.task_answer}
int max_profit(std::span<int> prices)
{
    int profit = 0;

    if (prices.empty())
        return profit;

    int buy_price = prices[0];
    
    for (std::size_t i = 1; i < prices.size(); ++i)
    {
        if (prices[i] < buy_price)
        {
            buy_price = prices[i];
            continue;
        }

        if (prices[i] - buy_price > profit)
            profit = prices[i] - buy_price;
    }
    
    return profit;
}
```

## Ссылки и указатели

Итак, мы вплотную подобрались к двум фундаментальным для C++ концепциям: ссылкам и указателям. Ссылка — это псевдоним для уже существующей переменной. А указатель — это переменная, которая содержит некий адрес оперативной памяти. Например, адрес другой переменной. Мы детально рассмотрим ссылки и указатели уже в следующих главах.

----------

## Резюме

- У перечислений `enum` и `enum class` можно задать целочисленный базовый тип.
- Классы — это подмножество типов. Не любой тип — это класс. У классов есть поля, конструкторы, деструкторы и другие методы. У фундаментальных типов ничего из этого нет.
- Тип функции определяется ее возвращаемым типом и типами параметров.
- С помощью класса-обертки `std::function` функции можно передавать аргументами в другие функции, возвращать как значения, присваивать переменным.
- Сишные массивы — это непрерывная область памяти, которую компилятор интерпретирует как подряд идущие элементы заданного типа.
- Размер сишного массива должен быть константным.
- Избегайте сишных массивов. Предпочитайте контейнер `std::array` либо работайте с сишным массивом через `std::span`.
