# Глава 14.2. Составные типы

Если тип не относится к фундаментальным, то он считается составным. Вы уже имеете представление про некоторые из составных типов: [перечисления,](/courses/cpp/chapters/cpp_chapter_0050/#block-enum) [структуры,](/courses/cpp/chapters/cpp_chapter_0050/#block-struct) [классы](/courses/cpp/chapters/cpp_chapter_0050/#block-class) и [функции.](/courses/cpp/chapters/cpp_chapter_0020/#block-functions)


![Составные типы](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-14/illustrations/cpp/types_compound.jpg) {.illustration}


В этой главе кратко обсудим уже известные вам составные типы и познакомимся с остальными.


## Перечисления

Прямо или косвенно, но любой составной тип опирается на фундаментальный. Так, [перечисление](/courses/cpp/chapters/cpp_chapter_0050/#block-enum) — это тип, значения которого ограничены набором именованных констант. Каждую из них компилятор соотносит целому числу. Его тип называется базовым типом перечисления (underlying type). Он зависит от реализации компилятора, но может укзываться явно.

Базовый тип задается после двоеточия в определении перечисления:

```cpp
enum class QuotaType : short
{
    Queries,
    ExecTime
};
```

В каких случаях имеет смысл явно прописывать базовый тип? Например, если перечисление нужно сериализовывать или хочется сэкономить память:

```cpp
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

Перечисление `Facility` содержит 261 константу по количеству компонентов системы, для которых нужно собирать метрики. Какой базовый тип подойдет для `Facility` лучше всего? Выберите один из целочисленных типов фиксированной ширины. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0142_task_0010}
```
. {.task_hint}
```cpp {.task_answer}
std::uint16_t
```

Базовый тип в комбинации с присваиванием конкретных значений константам дает интересные результаты:

```cpp
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

Символьный тип `char` является целочисленным, поэтому может выступать в качестве базового для перечисления. В этом примере мы вызвали появившуюся в C++23 функцию [std::to_underlying()](https://en.cppreference.com/w/cpp/utility/to_underlying.html). Она приводит значение перечисления к базовому типу. До C++23 с той же целью приходилось выполнять явное приведение типов через `static_cast`:

```cpp
std::println("{}", static_cast<char>(AccessType::Write));
```

Что выведет этот код? {.task_text}

Напишите `err` в случае ошибки компиляции или `?`, если поведение не определено стандартом и зависит от реализации. {.task_text}

```cpp {.example_for_playground}
import std;

enum class LogLevel : char
{
    Trace,
    Debug,
    Info,
    Warn,
    Error
};

int main()
{
    const char level = std::to_underlying(LogLevel::Info);
    std::println("{}", sizeof(level));
}
```

```consoleoutput {.task_source #cpp_chapter_0142_task_0020}
```
. {.task_hint}
```cpp {.task_answer}
1
```

## Функции

У функции может быть как [определение, так и объявление.](/courses/cpp/chapters/cpp_chapter_0100/#block-declaration-definition) Объявление состоит из типа возвращаемого значения, имени функции и типов параметров:

```cpp
bool exists(std::string path);
```

Имена параметров в объявлении опциональны:

```cpp
import std;

bool exists(std::string);                            // Объявление

int main()
{
    std::vector<std::string> pathes = {"/tmp", "/tmp/dump.csv"};
    for(std::string p: pathes)
    {
        std::println("{} exists: {}", p, exists(p));
    }
}

bool exists(std::string path)                        // Определение
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

```
return_type(type_param_1, type_param_2, ..., type_param_n)
```

Так выглядит тип функции `void run_service(std::string config, bool hot_reload)`:

```
void(std::string, bool)
```

Напишите тип функции, которая ничего не возвращает и не принимает параметров {.task_text}

```consoleoutput {.task_source #cpp_chapter_0142_task_0080}
```
. {.task_hint}
```cpp {.task_answer}
void()
```

Если сигнатура в первую очередь нужна компилятору, то для чего полезен тип? В C++ функция не являетя [объектом первого класса.](https://ru.wikipedia.org/wiki/%D0%9E%D0%B1%D1%8A%D0%B5%D0%BA%D1%82_%D0%BF%D0%B5%D1%80%D0%B2%D0%BE%D0%B3%D0%BE_%D0%BA%D0%BB%D0%B0%D1%81%D1%81%D0%B0) Ее нельзя _напрямую_ присвоить переменной, передать аргументом или вернуть как значение. Но есть способы, позволяющие добиться _практически_ этого:
- Обернуть функцию в класс [std::function](https://en.cppreference.com/w/cpp/utility/functional/function.html) и работать с объектом этого класса.
- Работать с переменной — ссылкой на функцию.
- Работать с переменной — указателем на функцию.

Каждый из этих способов явно или неявно задействует тип функции. Остановимся на варианте с `std::function`, а два других рассмотрим позже.

При оборачивании функции в шаблонный класс [std::function](https://en.cppreference.com/w/cpp/utility/functional/function.html) ее тип становится аргументом шаблона. А объект класса присваивается переменной, которую можно абсолютно законно возвращать из другой функции или передавать в нее аргументом:

```cpp
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

Реализуйте функцию `sum_if()`, которая принимает два параметра. Перый — это словарь `std::map` с ключами - строками и значениями типа `std::size_t`. Второй параметр - функция-предикат, принимающая строку и возвращающая `bool`. Функция `sum_if()` должна вернуть сумму значений элементов словаря, для ключей которых предикат возвращает `true`. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0030}


```
. {.task_hint}
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

```cpp
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

Кстати, в предыдущих главах вы уже [решали](/courses/cpp/chapters/cpp_chapter_0050/#block-predicate) задачи, в которых требовалось в одну функцию передать другую. Вы реализовывали это с помощью шаблонов:

```cpp
import std;

template<class Fn>
bool str_none_of(std::string s, Fn pred) // pred - это функция
{
    for (char c: s)
    {
        if (pred(c))
            return false;
    }

    return true;
}

bool is_a(char c)
{
    return c == 'a';
}

int main()
{
    std::println("str_none_of(\"generic\", is_a) = {}", str_none_of("generic", is_a));
}
```


## Классы и структуры

Разработчикам, пришедшим в C++ из таких языков как Python, нужно запомнить несколько важных фактов про классы.

Тип — это более широкое понятие, чем класс. Иными словами, классы — это подмножество типов. Любой класс можно также назвать типом, но не любой тип — это класс. Например, `char` — это тип, но уж точно не класс.

Классы — это более сложные абстракции, чем фундаментальные типы:
- Фундаментальные типы максимально близки к аппаратному представлению чисел. Переменные фундаментальных типов — это всего лишь небольшай область памяти, которую компилятор трактует определенным образом. Большинство операций над фундаментальными типами сводится к единственной машинной команде.
- Классы же имеют конструктор и деструктор. Их поля могут находиться в разных участках памяти. Для чтения и записи полей могут быть реализованы методы произвольной сложности.

Перед вами класс `File`, хранящий секции файла некоего формата. {.task_text}

Добавьте в объявление класса метод `findSection()`. Он принимает объект `std::function` с аргументом шаблона — типом функции-предиката. Она в свою очередь принимает два параметра: секцию `Section` и индекс секции `std::size_t`. Функция `findSection()` возвращает пару: первую секцию файла\, для которой предикат вернул `true`, и ее индекс. Если такой секции нет, функция возвращает пустую секцию и индекс, равный `std::numeric_limits<std::size_t>::max()`. {.task_text}

Под классом добавьте определение этого метода. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0040}
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
       // Добавьте объявление метода
       
    private:
     std::vector<Section> m_sections;
};

// Добавьте определение метода

```
. {.task_hint}
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
       std::pair<Section, std::size_t> findSection(std::function<bool(Section s, std::size_t idx)>);
       
    private:
     std::vector<Section> m_sections;
};

// ...
```


## Массивы

Вы уже поработали с массивами, реализованными как шаблонные классы стандартной библиотеки. Контейнер `std::vector` — это динамический массив, а `std::array` — статический.

C++ достались в наследие от Си и сырые массивы. Их называют сишными, традиционными или встроенными (built-in), потому что для их создания предусмотрена специальная конструкция языка. Это статические массивы, а по сути — непрерывная область памяти, которую компилятор трактует как последовательность элементов конкретного типа. Ее размер определяется количеством элементов и их типом.

Чтобы компилятор мог выделить под массив память, размер массива должен быть известен в момент компиляции, то есть быть константным.

Перед вами синтаксис создания массива с именем `name`, состоящего из `n` элементов типа `type`:

```cpp
type name[n];
```

Обратите внимание на константность длины массива:

```cpp
const std::size_t len = 3;
float samples[len];

samples[0] = 4.5;
samples[1] = 2.0;
samples[2] = -1.1;
std::println("{}", samples);
```

В этом примере мы заполнили массив значениями сразу после создания. Если этого не сделать, то элементы будут [инициализированы по умолчанию.](/courses/cpp/chapters/cpp_chapter_0131/#block-default-initialization) Для фундаментальных типов это означает _отсутствие_ инициализации, которое приведет к UB на строке с выводом массива в консоль.

Так выглядит заполнение элементов в момент инициализации:

```cpp
float samples[3] = {4.5, 2.0, -1.1};
```

Если длина списка инициализации меньше длины массива, оставшиеся элементы заполняются нулями (даже если 0 не является корректным значением для типа).

```cpp
int samples[3] = {4.5};
std::println("{}", samples);
```
```
[4.5, 0, 0]
```

Так выглядит инициализация единицами массива из тысячи элементов:

```cpp
int offsets[1000] = {1};
```

Всегда инициализируйте элементы массива. Не допускайте UB.

При наличии списка инициализации размер массива можно опустить: компилятор его выведет автоматически. Давайте получим длину такого массива, разделив его размер на размер типа элемента:

```cpp
int offsets[] = {-1, 0, -1, 2}; // автоматический вывод длины

std::size_t len = sizeof(offsets) / sizeof(int);

std::println("{}", len);
```
```
4
```

Оператор `=` при инициализации можно не ставить:

```cpp
int offsets[]{-1, 0, -1, 2};
```

Реализуйте функцию `is_sorted()`, которая принимает сишный массив, его длину и компаратор. Функция должна вернуть `true`, если массив отсортирован в соответствии с компаратором. Пустой массив считается отсортированным. {.task_text}

Пример вызова функции от массива из 4-х элементов и предиката `std::less()`: `is_sorted(arr, 4, std::less())`. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0050}
bool is_sorted(int arr[], std::size_t n, std::function<bool(int, int)> comp)
{

}
```
. {.task_hint}
```cpp {.task_answer}
bool is_sorted(int arr[], std::size_t n, F comp)
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

### Многомерные массивы

Сишные массивы могут быть многомерными.

Так выглядит создание и инициализация двумерного массива `measurements`:

```cpp
double measurements[2][3] = {
    // accelerometer axis
    // x    y      z 
    {0.2,  0.0,  -0.11},  // previous measurement
    {0.19, 0.09, -0.15}   // current  measurement
};
```

Мы получили массив, каждый элемент которого — это измерение, снятое с акселерометра. У этого массива 2 элемента для хранения актуального и предыдущего измерений. Каждое измерение — это массив из 3-х чисел типа `double`: ускорения по осям x, y, z.

Выведем значения массива `measurements` в консоль: 

```cpp
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

Дан двумерный массив `arr` и два числа: `m` и `k`. Поменяйте в массиве местами столбцы с индексами `m` и `k`. Реализуйте это функцией `swap_columns()`, которая возвращает новый массив. {.task_text}

```cpp {.task_source #cpp_chapter_0142_task_0090}
int[5][3] swap_columns(int arr[5][3], std::size_t m, std::size_t k)
{

}
```
. {.task_hint}
```cpp {.task_answer}

```

### Недостатки массивов

Сишным массивам присущи серьезные недостатки.

Такой массив не знает своей длины. Ее хранение становится ответственностью разработчика. Так, при передаче массива в функцию вместе с ним отправляется и еще один параметр — длина. Если массив двумерный, то дополнительных параметров функции уже два.

Кроме того, отсутствует удобный способ избежать случайного выхода за границы сишного массива. Зато в контейнере `std::array` такой способ есть: метод `at()`, в отличие от оператора `[]`, проверяет индекс и в случае выхода за границы бросает исключение.

Сишные массивы нельзя присваивать друг другу оператором `=` и сравнивать друг с другом через `==`. Все присваивания и сравнения делаются поэлементно, в цикле.

### Советы по работе с массивами

Главный совет по работе с сишными массивами: **не используйте их** без явной на то необходимости.

Предпочитайте контейнеры стандартной библиотеки. И не верьте распространенному заблуждению, что сишные массивы эффективнее `std::array`. Это не так. Контейнер `std::array` — одна из [абстракций с нулевой стоимостью,](/courses/cpp/chapters/cpp_chapter_0010/#block-zero-overhead) которую компилятор прекрасно оптимизирует.

Если в ваш код так или иначе передается сишный массив, то превратите его в `std::array` с помощью появившейся в C++20 функции [std::to_array()](https://en.cppreference.com/w/cpp/container/array/to_array.html). Или безопасно работайте с сырым массивом через класс-обертку [std::span](https://en.cppreference.com/w/cpp/container/span.html) (срез).

Срез `std::span<T>` — это «окно» на любую последовательность элементов типа `T`, будь то сырой массив или контейнер. 

```cpp
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

```cpp
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



## Ссылки и указатели



----------

## Резюме

- У перечислений `enum` и `enum class` можно задать целочисленный базовый тип.
- Классы — это подмножество типов. Не любой тип — это класс. У классов есть поля, конструкторы, деструкторы и другие методы. У фундаментальных типов ничего из этого нет.
- Тип функции определяется ее возвращаемым типом и типами параметров.
- С помощью класса-обертки `std::function` функции можно передавать аргументами в другие функции, возвращать как значения, присваивать переменным.
- Сишные массивы — это непрерывная область памяти, которую компилятор интерпретирует как подряд идущие элементы заданного типа.
- Размер сишного массива должен быть консантным.
- Избегайте сишных массивов. Предпочитайте контейнер `std::array` или работайте с ними через `std::span`.
