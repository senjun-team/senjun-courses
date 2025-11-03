# Глава 13.1. Основные способы инициализации

Что может быть проще, чем завести переменную и проинициализировать ее значением? Ведь так?

В C++ сложилась удивительная для мейнстримного языка ситуация: [инициализация](https://en.cppreference.com/w/cpp/language/initialization.html) переменной — запутанное действо, способное сбить с толку даже опытного разработчика. Одних только вариантов инициализации целочисленной переменной насчитывается _больше десятка._

Способы инициализации определяют правила, по которым в переменную сохраняется значение при создании. Мы будем знакомиться с ними постепенно. В этой главе мы в первом приближении рассмотрим лишь некоторые из них:

```cpp
// default-initialization: инициализация по умолчанию
int n;
std::vector<int> v;
```

```cpp
// copy-initialization: копирующая инициализация
int n = 1;
std::vector<int> v = v_other;
```

```cpp
// direct-initialization: прямая инициализация
int n(1);
std::vector<int> v(5);
```

```cpp
// uniform-initialization: универсальная инициализация
int n{1};
std::vector<int> v{1, 2, 3};
```

## Default-initialization: инициализация по умолчанию

Вы можете завести переменную без присваивания значения:

```cpp
int count;
```

Это называется [инициализацией по умолчанию](https://en.cppreference.com/w/cpp/language/default_initialization) (default-initialization). Мы [предупреждали,](/courses/cpp/chapters/cpp_chapter_0030/#block-initialization) что ее следует избегать. А теперь объясним, почему.

Для [фундаментальных типов](/courses/cpp/chapters/cpp_chapter_0020/#block-fundamental-types) инициализация по умолчанию сводится к тотальному _отсутствию инициализации._ Компилятор выделяет под переменную область памяти, но не записывает туда никакого значения. В переменной может находиться что угодно!

```cpp  {.example_for_playground}
import std;

int main()
{
    double distance;
    std::println("{}", distance); // UB
}
```
```
6.90910253244167e-310
```

В этом примере значение `distance` меняется от запуска к запуску. Если компилятор _на этот раз_ проинициализировал переменную нулем `0.0`, это ничего не гарантирует. Стандарт языка относит чтение неинициализированной переменной к [UB.](/courses/cpp/chapters/cpp_chapter_0060/#block-ub)

Не допускайте в своем коде UB. [Избегайте](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-always) инициализации по умолчанию.

Сколько раз в этом коде встречается UB? Напишите 0, если UB отсутствует. {.task_text}

```cpp {.example_for_playground}
import std;

void show_trip_info(int time, int dist)
{
    std::println("Journey will take {} hours.", time);
    std::println("You will cover {} kilometers.", dist);
}

int main()
{
    int t; // hours
    int d; // km

    t = 5;
    show_trip_info(t, d);
    return d;
}
```

```consoleoutput {.task_source #cpp_chapter_0131_task_0010}
```
Переменная `d` остается неинициализированной. Обращение к ней происходит в момент вывода значения в консоль и в момент возврата значения из функции `main()`. {.task_hint}
```cpp {.task_answer}
2
```

Однако в двух случаях инициализация по умолчанию _действительно_ сохраняет в переменную предсказуемое значение. Это справедливо для:
- Объектов классов и структур, у которых есть конструктор по умолчанию, корректно инициализирующий поля.
- Переменных со [статическим временем жизни](/courses/cpp/chapters/cpp_chapter_0090/#block-static-lifetime). Для них вызывается конструктор по умолчанию. Если конструктора нет, переменная [заполняется нулями](/courses/cpp/chapters/cpp_chapter_0090/#block-zeroes) вне зависимости от того, является ли 0 корректным значением для переменной данного типа. В главе [«Время жизни и область видимости»](/courses/cpp/chapters/cpp_chapter_0090/) описывалось, как это работает.

В этом коде нет UB, потому что глобальные и `static` переменные имеют статическое время жизни.

```cpp  {.example_for_playground}
import std;

bool healthcheck_ok;

int main()
{
    static int max_rps;
    std::println("{} {}", healthcheck_ok, max_rps); // Ok
}
```


В этом коде тоже нет UB, потому что `std::string` и `std::vector` — классы, а не фундаментальные типы. У них есть конструкторы по умолчанию, корректно инициализирующие объекты:

```cpp  {.example_for_playground}
import std;

int main()
{
    std::string uuid;           // Пустая строка
    std::vector<int> checksums; // Пустой вектор

    std::println("{} {}", uuid, checksums); // Ok
}
```

Если так опасна инициализация по умолчанию, почему она вообще существует?

Во-первых, это наследие языка Си. Пол века назад никто не видел криминала в том, чтобы объявить переменную сейчас, а заполнить как-нибудь потом. Зато многие современные языки запрещают любые действия над неинициализированными переменными. В Go переменные всегда инициализируются значением по умолчанию: нулем, `false` и т.д. В Rust и Kotlin чтение неинициализированной переменной приводит к ошибке компиляции. Если вы пришли в мир C++ из таких языков, не теряйте бдительность!

Во-вторых, инициализация по умолчанию важна для разработки эффективного кода. Представьте офлайн навигатор. В нем заведена переменная, ожидающая сигнала от гироскопа. Зачем совершать избыточное действие и инициализировать ее заранее, если значение гарантированно придет с сенсора? Экономия заряда аккумулятора важнее.

Область видимости локальной переменной начинается на строке с ее объявлением. Как считаете, что произойдет на строке с объявлением переменной `n`? Выберите один из вариантов: {.task_text #block-use-not-initialized}

`err`: ошибка компиляции.  {.task_text}

`ub`: неопределенное поведение.  {.task_text}

`ok`: в `n` сохранится предсказуемое и корректное значение.  {.task_text}

```cpp {.example_for_playground}
import std;

int main()
{
    int n = 2 + n;
}
```

```consoleoutput {.task_source #cpp_chapter_0131_task_0020}
```
Из-за того, что область видимости переменной начинается со строки с ее объявлением, мы получили синтаксически корректное выражение. То есть ошибки компиляции не будет. Однако в этом выражении участвует еще не проинициализированный объект! Поэтому мы получаем неопределенное поведение. {.task_hint}
```cpp {.task_answer}
ub
```

## Copy-initialization: копирующая инициализация

Синтаксис [копирующей инициализации](https://en.cppreference.com/w/cpp/language/copy_initialization) (copy-initialization) унаследован от Си. Значение переменной указывается после оператора `=`:

```cpp
bool is_answer_correct = false;
```

Знак равенства может создать ложное впечатление, что происходит присваивание значения. Но копирующая инициализация — это не то же самое, что присваивание. Это именно инициализация нового объекта с использованием другого объекта. Отсюда и название.

```cpp
double distance = 4.6; // Копирующая инициализация
distance = 5.5;        // Присваивание
```

Вы почувствуете разницу, когда познакомитесь с [перегрузками](/courses/cpp/chapters/cpp_chapter_0050/#block-overloading) конструкторов и операторов в классах. Они задают различное поведение для инициализации и присваивания.

У копирующей инициализации есть важное свойство: если тип переменной не совпадает с типом значения, которым она инициализируется, то выполняется неявное приведение типов.

### Неявное приведение типов

[Неявное приведение типов](https://en.cppreference.com/w/cpp/language/implicit_conversion) (implicit conversion) — это автоматическое преобразование одного типа в другой, выполняемое компилятором. 

```cpp  {.example_for_playground .example_for_playground_001}
int length = 8.7;       // double -> int
double weight = true;   // bool -> double

std::println("length={} weight={}", length, weight);
```
```
length=8 weight=1
```

В этом примере при инициализации целого числом с плавающей точкой произошло **сужающее преобразование** (narrowing conversion). А при инициализации числа с плавающей точкой типом `bool` — **расширяющее преобразование** (widening conversion). Сравним эти два вида преобразований.

Сужающее преобразование:
- Возможна потеря информации.
- Потенциально опасно. Может привести к потере информации или неправильному значению.
- Компилятор генерирует предупреждение либо ошибку, если ему передан флаг `-Werror`.
- Пример: `char c = 5000;`. Здесь `int` преобразуется к `char`. Значение 5000 гарантированно выходит за диапазон типа `char`.

Расширяющее преобразование:
- Потери информации нет.
- Всегда безопасно.
- Компилятор не генерирует предупреждений.
- Пример: `int n = 'x';`. Здесь тип `char` приводится к `int`. Диапазон целого знакового `int` шире, чем у `char`. Поэтому в `n` сохраняется ASCII-код латинского символа `x`.

Вот некоторые правила преобразований:
- `bool` к числу: `false` приводится к 0, `true` — к 1.
- Число к `bool`: 0 приводится к `false`, остальные значения — к `true`.
- Число с плавающей точкой к целому: дробная часть отбрасывается.

Так выглядит приведение типов при копирующей инициализации объекта класса:

```cpp  {.example_for_playground}
import std;

class Array
{
public:
    Array(std::size_t length)
    {
        len = length;
    }

private:
    std::size_t len;
};

int main()
{
    Array arr = 3; // Вызывает конструктор Array(3)
}
```

Какое значение сохранится в переменную `total_dist`?  {.task_text}

Напишите получившееся число, `ub` в случае неопределенного поведения, либо `err` в случае ошибки компиляции. {.task_text}

```cpp {.example_for_playground}
import std;

int main()
{
    bool go_shopping = true;
    bool go_to_work = false;

    int dist_to_shop = 1; // km
    int dist_to_work = 3; // km

    int total_dist = go_shopping * dist_to_shop + go_to_work * dist_to_work;
}
```

```consoleoutput {.task_source #cpp_chapter_0131_task_0030}
```
В выражении, инициализирующем переменную `total_distance`, есть расширяющее преобразование `bool` к числу. {.task_hint}
```cpp {.task_answer}
1
```

Неявные преобразования часто приводят к проблемам. Особенно [опасны](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-narrowing) сужающие преобразования. Во многих современных языках неявные преобразования запрещены. Например, в Kotlin и Rust нельзя переменную типа `int` инициализировать через `float`. И наоборот.

А в C++ чтобы при создании переменной избежать неявных преобразований, вместо копирующей инициализации используют универсальную. О ней чуть позже.

### Запрет неявного преобразования аргументов конструктора

Неявное преобразование аргументов конструктора легко становится источником трудно обнаруживаемых ошибок:

```cpp   {.example_for_playground .example_for_playground_002}
import std;

class Error
{
public:
    Error(int code)
    {
        m_code = code;
    }

    // ...

private:
    int m_code = 0;
};

void handle_error(Error err, int retries_count)
{
    std::println("Handling error: {} Retries left: {}", err, retries_count);
    // ...
}

int main()
{
    Error err(503);
    int retries = 2;
    handle_error(retries, retries); // Упс!
}
```
```
Handling error: 2 Retries left: 2
```

Здесь в функцию `handle_error()` первым аргументом вместо объекта `err` случайно передано целое число `retries`. Тем не менее, этот код успешно скомпилировался и запустился. Дело в том, что компилятор выполнил неявное преобразование `int` к классу `Error`, вызвав конструктор `Error`, принимающий `int`.

Чтобы запретить неявные преобразования аргументов конструктора, пометьте его спецификатором [explicit](https://en.cppreference.com/w/cpp/language/explicit):

```cpp    {.example_for_playground .example_for_playground_003}
import std;

class Error
{
public:
    explicit Error(int code)
    {
        m_code = code;
    }

    // ...

private:
    int m_code = 0;
};

void handle_error(Error err, int retries_count)
{
    std::println("Handling error: {} Retries left: {}", err, retries_count);
    // ...
}

int main()
{
    Error err(503);
    int retries = 2;
    handle_error(retries, retries); // Упс!
}
```
```
main.cpp:36:5: error: no matching function for call to 'handle_error'
   36 |     handle_error(retries, retries);
      |     ^~~~~~~~~~~~
```

Теперь компилятор не может выполнить неявное преобразование и завершает сборку с ошибкой.

Спецификатор `explicit` запрещает применение копирующей инициализации, для которой необходимо неявное приведение типов:

```cpp    {.example_for_playground .example_for_playground_004}
class Array
{
public:
    explicit Array(std::size_t length)
    { /*... */ }
};

int main()
{
    Array arr1 = 3; // Ошибка!
    
    Array arr2(3);  // Ок
}
```

Добавление `explicit` к конструктору класса напрашивается в заданиях на практику [«Скользящее среднее»,](/courses/cpp/practice/cpp_moving_average/) [«Интерпретатор Brainfuck»](/courses/cpp/practice/cpp_brainfuck_interpreter/) и [«LRU кеш».](/courses/cpp/practice/cpp_lru_cache/) Пометьте конструкторы классов из практики спецификатором `explicit` и убедитесь, что тесты по-прежнему проходят.

## Direct-initialization: прямая инициализация

[Direct-initialization](https://en.cppreference.com/w/cpp/language/direct_initialization) (прямая инициализация) заключается в явном вызове конструктора и передаче в него аргументов. Она существует еще со времен C++98.

Так выглядит прямая инициализация для переменной фундаментального типа:

```cpp
double score(7.89);
```

Если в скобках не указывать значение, то произойдет инициализация значением по умолчанию:

```cpp
double f()
{
    // ...
    return double(); // 0.0
}
```

А это — прямая инициализация объекта класса:

```cpp   {.example_for_playground}
import std;

struct TaskContext
{
    // Конструктор по умолчанию
    TaskContext()
    {
        parent_id = 0;
        is_suspended = false;
    }

    // Конструктор с параметрами
    TaskContext(int parent_proc_id, bool suspended)
    {
        parent_id = parent_proc_id;
        is_suspended = suspended;
    }

    int parent_id;
    bool is_suspended;
};

int main()
{
    // Прямая инициализация
    TaskContext tc(9034, true);

    std::println("{} {}", tc.parent_id, tc.is_suspended);
}
``` 
```
9034 true
```

Прямая инициализация предназначена для явного вызова необходимого конструктора. Компилятор перебирает все перегрузки конструктора и определяет, какая лучше соответствует переданным аргументам. А для встроенных типов она ведет себя так же, как копирующая:

```cpp
int x(5); // То же самое, что int x = 5;
```

Скорее всего, вы будете часто использовать прямую инциализацию при работе с [контейнерами.](/courses/cpp/chapters/cpp_chapter_0070) У них есть множество перегрузок конструктора. У `std::vector` [их 12,](https://en.cppreference.com/w/cpp/container/vector/vector) в том числе для копирования из другого вектора или из диапазона.

Инициализация вектора 8-ю элементами. Элементы при этом инициализируются конструктором по умолчанию:

```cpp
std::vector<std::string> v(8); // Прямая инициализация
std::println("{}", v);
```
```
["", "", "", "", "", "", "", ""]
```

Инициализация вектора 4-мя элементами со значением "-":

```cpp
std::vector<std::string> v(4, "-"); // Прямая инициализация
std::println("{}", v);
```
```
["-", "-", "-", "-"]
```

Реализуйте [шаблонную функцию](/courses/cpp/chapters/cpp_chapter_0050/#block-templates) `count_unique()`, которая принимает вектор и возвращает количество его уникальных элементов. Реализация должна занять одну строку. Вам поможет [одна из перегрузок](https://en.cppreference.com/w/cpp/container/unordered_set/unordered_set.html) конструктора `std::unordered_set`. Она создает множество из элементов, на которые указывает диапазон, ограниченный парой итераторов. {.task_text}

```cpp {.task_source #cpp_chapter_0131_task_0040}
template<class T>
std::size_t count_unique(std::vector<T> v)
{

}
```
Воспользуйтесь перегрузкой, принимающей итераторы на диапазон. У получившегося временного объекта вызовите метод `size()`. {.task_hint}
```cpp {.task_answer}
template<class T>
std::size_t count_unique(std::vector<T> v)
{
    return std::set(v.begin(), v.end()).size();
}
```


### The most vexing parse

У прямой инициализации есть подводный камень, имя которому [the most vexing parse](https://en.wikipedia.org/wiki/Most_vexing_parse). Если переводить дословно, то это «самый раздражающий синтаксический разбор». Речь идет о правиле: все, что компилятор может трактовать как [объявление](/courses/cpp/chapters/cpp_chapter_0010/#block-declaration-definition) (declaration), должно рассматриваться как таковое. Это правило синтаксического анализа раздражает разработчиков, когда они хотят завести переменную, а получают объявление функции.

Рассмотрим с виду невинный пример: {#block-the-most-vexing-parse}

```cpp    {.example_for_playground}
import std;

int main()
{
    int x();
}
```

С точки зрения разработчика конструкция `int x();` выглядит как создание переменной `x`, инициализированной нулем. А с точки зрения компилятора это объявление функции `x()`, возвращающей `int`.

Откройте этот пример в [плэйграунде.](https://senjun.ru/playground/cpp/) Перейдите в файл CMakeLists.txt и убедитесь, что компилятору передается [флаг -Werror.](/courses/cpp/chapters/cpp_chapter_0012/#block-flags) После этого попробуйте собрать проект и посмотрите, как выглядит ошибка компиляции, связанная с the most vexing parse.

А теперь покажем, как избежать столкновения с the most vexing parse.
 
## Uniform-initialization: универсальная инициализация

Универсальная инициализация (uniform-initialization) появилась в C++11. Она также известна как brace-initialization, потому что значение переменной указывается в фигурных скобках:

```cpp
bool is_answer_correct{false};

int count{1};

int count = {1}; // То же самое, что без '='

int count{};     // 0 - инициализация значением по умолчанию
```

Еще одно название этой инициализации — unicorn initialization или «единорожья инициализация». 


![Unicorn power](https://raw.githubusercontent.com/senjun-team/senjun-courses/cpp-chapter-13/illustrations/cpp/unicorn.webp) {.illustration}


Откуда такое название? Во-первых, скобка `}` похожа на рог единорога. Во-вторых, эта инициализация по меркам C++ творит магию: она запрещает неявное приведение типов.

```cpp
int a = 8.7;                  // ОК
int b{8.7};                   // Ошибка компиляции
int c{static_cast<int>(8.7)}; // ОК
```

В этом примере для инициализации переменной `c` мы явно привели типы через `static_cast`. Вы [познакомились с этим способом](/courses/cpp/practice/cpp_moving_average/#block-static-cast) в практике «Скользящее среднее».

Универсальная инициализация обладает еще одним достоинством: она решает проблему the most vexing parse. Чтобы исправить ее в примере кода из предыдущего раздела, достаточно заменить прямую инициализацию на универсальную:

```cpp
// The most vexing parse: объявление функции x()
// int x(); 

// Инициализация переменной x
int x{};
```

Универсальная инициализация считается более предпочтительным и современным способом, чем копирующая и прямая инициализация.

Вы уже применяли универсальную инициализацию [при создании](/courses/cpp/chapters/cpp_chapter_0072/#block-initialization) контейнеров:

```cpp
std::set<std::size_t> s{4, 5};

std::vector<int> v = {1, 2, 3}; // То же самое, что и без '='
```

Она подходит и для инициализации классов:

```cpp   {.example_for_playground}
import std;

struct Range
{
    Range(std::size_t min_val, std::size_t max_val)
    {
        min = min_val;
        max = max_val;
    }

    std::size_t min = 0;
    std::size_t max = 0;
};

int main()
{
    // Универсальная инициализация:
    Range range{3, 607};

    std::println("{} {}", range.min, range.max);
}
``` 
```
3 607
```

Универсальная инициализация может применяться для аргументов и возвращаемых значений функций:

```cpp   {.example_for_playground}
import std;

std::pair<int, bool> get_val()
{
    return {6, true};       // Универсальная инициализация 
}

void show_val(std::pair<int, bool> p)
{
    std::println("{}", p);
}

int main()
{
    show_val({7, false});   // Универсальная инициализация 
    show_val(get_val());
}
```
```
(7, false)
(6, true)
```

По возможности [предпочитайте универсальную инициализацию.](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-list) Она:
- Запрещает неявные преобразования.
- Решает проблему the most vexing parse.
- Предоставляет единообразный синтаксис для инициализации встроенных типов, классов и стандартных контейнеров списком значений.

Комфортно ли вам читать код с универсальной инициализацией?

```cpp
for(int i{0}; i < 10; ++i)
{
    // ...
}

for (auto it{v.rbegin()}; it != v.rend(); ++it)
{
    // ...
}
```

Если такой код кажется вам странным и трудным для восприятия, то наш совет: больше практикуйтесь. Читайте чужой код в стиле современного С++ и почаще сами применяйте универсальную инициализацию.

Вам нужно исправить функцию `make_new_job()`, на первой строке которой есть инициализация, приводящая к the most vexing parse.  {.task_text}

С точки зрения разработчика конструкция `LoadJob job(LoadStatus());` выглядит как создание переменной `job` с вызовом конструктора `LoadJob(LoadStatus status)` и передачей в него нового объекта типа `LoadStatus`. У компилятора другое мнение на этот счет: он расценивает это как объявление функции `job()`, принимающей другую функцию, которая возвращает `LoadStatus` и не принимает параметров. {.task_text}

```cpp {.task_source #cpp_chapter_0131_task_0050}
enum class LoadStatus
{
    NotStarted,
    InProgress,
    Ok, 
    Failed,
    Canceled
};

struct LoadJob
{
    LoadJob(LoadStatus status)
    {
        load_status = status;
    }

    void start()
    {
        load_status = LoadStatus::InProgress;
    }

    LoadStatus load_status;
};

LoadJob make_new_job()
{
    LoadJob job(LoadStatus());
    job.start();
    return job;
}
```
Вместо прямой инициализации переменной `job` используйте универсальную. {.task_hint}
```cpp {.task_answer}
enum class LoadStatus
{
    NotStarted,
    InProgress,
    Ok, 
    Failed,
    Canceled
};

struct LoadJob
{
    LoadJob(LoadStatus status)
    {
        load_status = status;
    }

    void start()
    {
        load_status = LoadStatus::InProgress;
    }

    LoadStatus load_status;
};

LoadJob make_new_job()
{
    LoadJob job{LoadStatus{}};
    job.start();
    return job;
}
```

----------

## Резюме

Подытожим главу рекомендациями по выбору способа инициализации переменных.

По умолчанию предпочитайте _универсальную инициализацию._ Она подходит для встроенных типов, классов, а также для заполнения контейнера значениями в момент создания:

```cpp
// Uniform-initialization

bool in_range{true};

std::pair<std::size_t, bool> res{read_data()};

std::set<int> ids{834, 244, 11};
```

А для выбора конкретной перегрузки конструктора применяйте _прямую инициализацию:_

```cpp
// Direct-initialization

std::string three_tildas('~', 3);

Message msg("...");
```
