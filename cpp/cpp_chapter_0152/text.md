# Глава 15.2. Адресная арифметика

Итак, указатель — это переменная, хранящая адрес. И размер указателя не зависит от типа, на который он ссылается. Зачем же в момент объявления сообщать компилятору об этом типе? Какая разница, чей адрес содержит указатель — `bool`, `long int` или `std::queue`?

## Зачем указателю знать свой тип данных

Память упрощённо можно представить как массив ячеек по 1 байту. Кстати, именно поэтому нельзя завести переменную размером в 3 или 11 бит при условии, что байт равен 8 бит.

Допустим, у нас есть три локальных переменных размером в 1, 4 и 8 байт:

```cpp  {.example_for_playground .example_for_playground_001}
int ret_code = -5;
bool retry = false;
bool * p = &retry;
```


![Переменные в адресном пространстве](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-16-2/illustrations/cpp/pointers_and_addresses.jpg) {.illustration}


И если бы мы захотели по указателю `p` обновить значение `retry`, но компилятор имел бы _неправильное_ представление о размере этой переменной, то вместо перезаписи 1 байта мы бы перезаписали 4, 8 или больше. Мы бы повредили значения соседних переменных. А возможно, даже ячеек, в которых расположен сам указатель `p`! Такая ситуация называется **повреждением памяти** (memory corruption).

Вы заметили, что в коде переменная `retry` создается _после_ `ret_code`, на на картинке мы её расположили в памяти _до_ `ret_code`? Это сделано специально: Стандарт не гарантирует порядок следования переменных в памяти.

Ещё одна причина, по которой компилятору важно знать о типе указателя — это **адресная арифметика.** 

## Что такое адресная арифметика

С помощью указателей можно перемещаться по памяти. В частности, перебирать элементы массива. Для этого к указателю применяются арифметические операции `+` и `-`, работающие _с учётом типа._

Допустим, у нас есть указатель `p` типа `T *` и целое число `n`. Тогда к `p` применимы арифметические операции:
- `p + n`. Сложение указателя с целым числом увеличивает адрес на это число, домноженное на размер типа `T`.
- `p - n`. Вычитание из указателя целого числа уменьшает адрес на значение `n`, домноженное на размер типа.
- `p - p_other`. Вычитание из указателя другого указателя даёт количество элементов типа `T` между ними. Операция имеет смысл для указателей одного типа, ссылающихся на непрерывный блок памяти.

Также к указателям применимо сравнение операторами `>`, `>=`, `<`, `<=`, `==` и `!=`. При этом происходит сравнение адресов, на которые они указывают. Сравнение на больше-меньше имеет смысл только в случае, если указатели ссылаются на одну и ту же область памяти.

Разберём подробнее каждую из арифметических операций. Удобнее всего это делать на примере указателей на элементы сишных массивов. Заодно узнаем, что такое сишные строки.

## Указатели на элементы массива

Элементы контейнера `std::array` и сишных массивов расположены друг за другом [в непрерывной области памяти.](/courses/cpp/chapters/cpp_chapter_0132/#block-c-array-under-the-hood) Убедимся в этом: переберём в цикле все элементы массива и выведем их адреса. Для консольного вывода в виде таблицы используем [спецификаторы форматирования.](https://en.cppreference.com/w/cpp/utility/format/spec.html)

```cpp  {.example_for_playground .example_for_playground_002}
std::println("Size of int: {} bytes", sizeof(int));
std::array<int, 5> pow_series = {16, 32, 64, 128, 256};

std::println("\nArray of ints:");
std::string line(30, '-');
std::println("{}", line);
std::println("{:>2} {:>16} {:>9}", 'i', "address", "value");
std::println("{}", line);

for (std::size_t i = 0; i < pow_series.size(); ++i)
{
    std::println("{:>2} {:>16} {:>9}",
                    i,
                    static_cast<void *>(&pow_series[i]),
                    pow_series[i]);
}

std::println("{}", line);
```
```
Size of int: 4 bytes

Array of ints:
------------------------------
 i          address     value
------------------------------
 0   0x7ffe9438f630        16
 1   0x7ffe9438f634        32
 2   0x7ffe9438f638        64
 3   0x7ffe9438f63c       128
 4   0x7ffe9438f640       256
------------------------------
```

Как видите, разность между адресами соседних элементов совпадает с размером типа элемента.

Чтобы присвоить указателю адрес элемента массива, к нему применяется оператор взятия адреса `&`:

```cpp  {.example_for_playground .example_for_playground_003}
std::array<int, 3> arr = {-2, -1, 0};

int * p = &arr[0];

std::println("{}", *p);
```
```
-2
```

Для получения указателя _на нулевой элемент сишного массива_ есть более лаконичная запись:

```cpp  {.example_for_playground .example_for_playground_004}
int arr[] = {-2, -1, 0};

int * p = arr;

std::println("{}", *p);
```
```
-2
```

В выражении `int * p = arr` тип массива `int[3]` приводится к типу указателя `int *` на нулевой элемент: происходит [низведение массива]((/courses/cpp/chapters/cpp_chapter_0132/#block-array-to-pointer-decay) ) (array-to-pointer decay). Вообще оно срабатывает не только при передаче массива в функцию, но и в большинстве случаев, когда имя массива участвует в выражении. К исключениям относятся, например, такие случаи:
- Применение к массиву оператора взятия адреса `&`.
- Вызов `sizeof` для массива.
- Инициализация ссылки на массив.

За счет низведения массива работа с массивом напрямую или через указатель схожа. Например, синтаксис языка позволяет применять к указателю оператор `[]` и через него обращаться к элементам массива:

```cpp  {.example_for_playground .example_for_playground_005}
std::uint64_t seed[] = {
        9621534751069176051UL,
        2054564862222048242UL
    };

std::uint64_t * p = seed;
std::println("{} {}", *p, p[1]);
```
```
9621534751069176051 2054564862222048242
```

Что выведет этот код? Напишите `err` в случае ошибки или `ub` в случае неопределённого поведения. {.task_text}

Вспомните [способы](/courses/cpp/chapters/cpp_chapter_0132/#block-array-length) определения длины массива. {.task_text}

```cpp  {.example_for_playground .example_for_playground_006}
int arr[] = {3, 9};
std::println("{}", sizeof(arr) / sizeof(*arr));
```

```consoleoutput {.task_source #cpp_chapter_0152_task_0010}
```
Здесь `arr` — это сишный массив, и `sizeof(arr)` возвращает размер массива в байтах. А `sizeof(*arr)` возвращает размер нулевого элемента в байтах. Оператор разыменования `*` применяется к массиву, который неявно приводится к указателю. {.task_hint}
```cpp {.task_answer}
2
```

## Нуль-терминированные строки

Вы уже знакомы с классом строки `std::string` из стандартной библиотеки. У него [около двадцати](https://en.cppreference.com/w/cpp/string/basic_string/basic_string.html) перегрузок конструктора. Наверное, наиболее популярна перегрузка для инициализации литералом в двойных кавычках:

```cpp
std::string protocol = "UART";
```

Но [какой тип](https://en.cppreference.com/w/cpp/language/string_literal.html) у самого литерала? Перед вами [нуль-терминированная строка.](https://ru.wikipedia.org/wiki/%D0%9D%D1%83%D0%BB%D1%8C-%D1%82%D0%B5%D1%80%D0%BC%D0%B8%D0%BD%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%BD%D0%B0%D1%8F_%D1%81%D1%82%D1%80%D0%BE%D0%BA%D0%B0) По сути это статический массив символов с типом `const char[n]`, где `n` — длина литерала + 1.

Например, у литерала `"UART"` тип `const char[5]`. Дополнительный элемент отводится под [завершающий ноль](https://en.wikipedia.org/wiki/Null_character) (terminating null character) — маркер конца строки с кодом `0`. В C++ это управляющий символ `\0`:

```cpp
char null_char = '\0';
```

В обиходе нуль-терминированные строки называют сишными. Создание такой строки из литерала равносильно инициализации массива набором символов, но к литералу не нужно добавлять завершающий ноль вручную.

Перед вами три варианта создания сишной строки:

```cpp  {.example_for_playground .example_for_playground_007}
const char * a = "C-like string";

const char b[] = "C-like string";

const char c[] = {'C',
                        '-',
                        'l', 'i', 'k', 'e',
                        ' ', 
                        's', 't', 'r', 'i', 'n', 'g', 
                        '\0'}; // Добавляем сами

std::println("{}\n{}\n{}", a, b, c);
```
```
C-like string
C-like string
C-like string
```

Записывать строку в двойных кавычках удобнее, чем перечислять посимвольно. Поэтому определяйте строку как массив только чтобы избежать добавления завершающего нуля.

При передаче в функцию сишная строка приводится к указателю (array-to-pointer decay). Но в отличие от массива, она не требует передачи дополнительного параметра — длины. Не обязательно знать длину строки, чтобы избежать выхода за ее границы: достаточно найти символ `\0`.

В стандартной библиотеке C++ есть [функции](https://en.cppreference.com/w/cpp/header/cstring.html) для работы с сишными строками. Например, [std::strcmp()](https://en.cppreference.com/w/cpp/string/byte/strcmp.html) для сравнения строк. Ведь строки, как и массивы, [нельзя](/courses/cpp/chapters/cpp_chapter_0132/#block-compare) сравнивать напрямую такими операторами как `==` или `>`.

```cpp  {.example_for_playground .example_for_playground_008}
// strcmp() возвращает:
//   0, если строки равны.
//   Отрицательное число, если первая строка лексикографически меньше второй.
//   Положительное число, если она больше.
// int strcmp( const char* lhs, const char* rhs );

int main()
{
    char proto1[] = "UART";
    char proto2[] = "I2C";
    std::println("{}", std::strcmp(proto1, proto2));
}
```
```
1
```

## Прибавление к указателю целого числа

Перебирать сишный массив можно не только через индексы, но и с помощью указателя. Для этого указатель увеличивается на требуемое количество элементов: `++p`, `p++`, `p += n` или `p = p + n`.

Когда к указателю прибавляется целое число, то адрес увеличивается на соответствующее значение, умноженное на размер типа данных. Если прибавить к указателю число, не превышающее длину массива, то он будет ссылаться на один из последующих элементов:

```cpp  {.example_for_playground .example_for_playground_009}
double thresholds[]{0.009, 0.01, 0.5, 1.5};
    
double * p = &thresholds[0];
std::println("{}", *p);  // 0.009

++p;
std::println("{}", *p);  // 0.01

p += 2;
std::println("{}", *p);  // 1.5

++p;                     // Выход за пределы массива
std::println("{}", *p);  // UB
```
```
0.009
0.01
1.5
??? UB
```


![Перемещение по массиву с помощью указателя](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-16/illustrations/cpp/pointer_to_array_element.jpg) {.illustration}


Важно следить, чтобы указатель не вышел за границы массива. Обращение по нему приведёт к UB.

Стандарт [определяет,](https://timsong-cpp.github.io/cppwp/std23/expr.sub#2) что запись `arr[i]` эквивалентна выражению `*(arr + i)`. Разберём по шагам, что в нём происходит:
1. Внутри круглых скобок массив неявно приводится к указателю.
2. К хранящемуся в указателе адресу прибавляется число `i`, умноженное на размер типа.
3. Затем к нему применяется разыменование `*`. Мы получаем значение массива по индексу `i`.

```cpp  {.example_for_playground .example_for_playground_010}
std::uint8_t cmyk_color[] = {73, 45, 0, 4};

std::println("cmyk_color[3] == {}. *(cmyk_color + 3) == {}", 
            cmyk_color[3],
            *(cmyk_color + 3));
```
```
cmyk_color[3] == 4. *(cmyk_color + 3) == 4
```

А теперь следите за руками. Оператор `+` [коммутативный:](https://ru.wikipedia.org/wiki/%D0%9A%D0%BE%D0%BC%D0%BC%D1%83%D1%82%D0%B0%D1%82%D0%B8%D0%B2%D0%BD%D0%BE%D1%81%D1%82%D1%8C) `a + b` и `b + a` — это одно и то же. То есть `*(arr + i)` можно записать как `*(i + arr)`. Это в свою очередь соответствует записи `i[arr]`. Скомпилируется ли код вида `i[arr]`? Да! Следует ли использовать такую форму записи в промышленном коде? Категорически нет! Она эзотерическая и сбивающая с толку. Зачем тогда о ней знать? Исключительно потому что о ней любят спрашивать на собеседованиях.

Что выведет этот код? Напишите `err` в случае ошибки или `ub` в случае неопределённого поведения. {.task_text}

```cpp  {.example_for_playground .example_for_playground_011}
std::uint8_t rgb_color[] = {66, 135, 245};

std::println("{}", 1[rgb_color]);
```

```consoleoutput {.task_source #cpp_chapter_0152_task_0020}
```
Запись `1[rgb_color]` равносильна `rgb_color[1]`. {.task_hint}
```cpp {.task_answer}
135
```

Итак, выражение `arr[i]` эквивалентно выражению `*(arr + i)`. Которое в свою очередь можно заменить на `*(ptr + i)` и даже `ptr[i]`, если `ptr` ссылается на нулевой элемент массива.

Эквивалентны ли выражения `&arr[i]` и `ptr + i`? `y/n` {.task_text}

```consoleoutput {.task_source #cpp_chapter_0152_task_0030}
```
Применение оператора взятия адреса в выражении `&arr[i]` применяется [после](https://en.cppreference.com/w/cpp/language/operator_precedence.html) оператора `[]`. {.task_hint}
```cpp {.task_answer}
y
```

Обойдём массив с помощью указателей:

```cpp  {.example_for_playground .example_for_playground_012}
const std::size_t n = 3;
std::size_t sizes[n] = {16384, 32768, 65536};

std::size_t * ptr = sizes;

for (std::size_t i = 0; i < n; ++i)
{
    std::println("i={}. Addr={}. Val={}",
                  i, static_cast<void *>(ptr + i), *(ptr + i));
}
```
```
i=0. Addr=0x7fffc75c9740. Val=16384
i=1. Addr=0x7fffc75c9748. Val=32768
i=2. Addr=0x7fffc75c9750. Val=65536
```

В стандартной библиотеке есть функция [std::strchr()](https://en.cppreference.com/w/cpp/string/byte/strchr.html), которая принимает сишную строку и символ. Она возвращает указатель на первое вхождение символа в строку. Завершающий ноль _участвует_ в поиске. Если символ не найден, функция возвращает `nullptr`. {.task_text}

Напишите свою реализацию функции под названием `find_char()`. Считаем, что `nullptr` в неё передаваться не будет. {.task_text}

```cpp {.task_source #cpp_chapter_0152_task_0040}
const char * find_char(const char * str, char c)
{

}
```
Организуйте цикл, в котором увеличивайте указатель, пока значение по указателю не станет равным искомому символу. Если же значение равно `'\0'`, верните `nullptr`. {.task_hint}
```cpp {.task_answer}
const char * find_char(const char * str, char c)
{
    while (*str != c)
    {
        if (*str == '\0')
            return nullptr;
        ++str;
    }

    return str;
}
```

Применим адресную арифметику, чтобы посмотреть, какие адреса в памяти занимают переменные. Заведём шаблонную функцию `show_used_memory()`, которая принимает указатель на переменную любого типа `T`. Внутри функции приведём тип указателя `const T *` к типу `const char *` с помощью [reinterpret_cast](https://en.cppreference.com/w/cpp/language/reinterpret_cast.html). Это нужно, чтобы при инкременте указателя адрес увеличивался ровно на 1 байт. Выражение `reinterpret_cast<T>(expr)` приводит тип `expr` к типу `T`.

```cpp  {.example_for_playground}
import std;

template<class T>
void show_used_memory(const T * ptr, const std::string & name)
{
    const char * address = reinterpret_cast<const char *>(ptr);
    
    std::println("\n|{:<10}|{:p}|",
                 name, static_cast<const void *>(address));
    
    for (auto i = 1; i < sizeof(T); ++i)
    {
        std::println("|{:<10}|{:p}|",
                     ' ', static_cast<const void *>(address + i));
    }
}

int main()
{
    int ret_code = -5;
    bool retry = false;
    bool * p = &retry;

    show_used_memory(&retry, "retry");
    show_used_memory(&ret_code, "ret_code");
    show_used_memory(&p, "p");
}
```
```
|retry     |7ffdb51deffb|

|ret_code  |7ffdb51deffc|
|          |7ffdb51deffd|
|          |7ffdb51deffe|
|          |7ffdb51defff|

|p         |7ffdb51df000|
|          |7ffdb51df001|
|          |7ffdb51df002|
|          |7ffdb51df003|
|          |7ffdb51df004|
|          |7ffdb51df005|
|          |7ffdb51df006|
|          |7ffdb51df007|
```

## Вычитание из указателя целого числа

Вычитание целых чисел из указателей работает по той же схеме, что и сложение: из адреса отнимается значение, умноженное на размер типа.

```cpp  {.example_for_playground .example_for_playground_013}
int random_numbers[] = {-67, 9, 22, 18};
    
int * p = &random_numbers[3];
std::println("Address: {}. Value: {:0x}", 
             static_cast<void *>(p), *p);

p--;
std::println("Address: {}. Value: {:0x}",
             static_cast<void *>(p), *p);

p -= 2;
std::println("Address: {}. Value: {:0x}",
             static_cast<void *>(p), *p);

--p;  // Выход за пределы массива
std::println("Address: {}. Value: {:0x}", 
             static_cast<void *>(p), *p);  // UB
```
```
Address: 0x7ffe027539dc. Value: 12
Address: 0x7ffe027539d8. Value: 16
Address: 0x7ffe027539d0. Value: -43
??? UB
```


![Перемещение по массиву с помощью указателя](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-16-2/illustrations/cpp/pointer_to_array_substraction.jpg) {.illustration}


Напишите функцию `reverse()`, которая принимает указатель на сишную строку. Она должна перевернуть строку, то есть расположить её символы в обратном порядке. Считаем, что в функцию не может быть передан `nullptr`. {.task_text}

```cpp {.task_source #cpp_chapter_0152_task_0050}
void reverse(char * str)
{

}
```
Заведите две переменных типа `char *`: `start` и `end`. Обе инициализируйте указателем на начало строки. Сдвиньте `end` так, чтобы он смотрел на последний значащий символ строки. Это символ, предшествующий `'\0'`. Затем в цикле пока `start` меньше `end` меняйте местами значения по этим указателям, увеличивайте `start` и уменьшайте `end`. {.task_hint}
```cpp {.task_answer}
void reverse(char * str)
{
    char * start = str;
    char * end = start;
    
    while (*end != '\0')
        ++end;
    
    --end;

    while (start < end)
    {
        std::swap(*start, *end);
        ++start;
        --end;
    }
}
```

## Вычитание указателей

Указатели можно вычитать один из другого. Для этого они должны иметь одинаковый тип и ссылаться на области непрерывного участка памяти. Например, указывать на элементы одного и того же массива. Разность между указателями равна количеству объектов заданного типа между ними, а вовсе не количеству байт! При вычитании указателей компилятор делит получившееся значение на размер типа данных. Поэтому через разность указателей на элементы массива можно определять расстояние между ними.

```cpp  {.example_for_playground .example_for_playground_014}
int http_non_retriable_errors[] = {
    400, // Bad Request
    401, // Unauthorized
    404, // Not found
    403, // Forbidden
    501, // Not implemented
    405, // Method not allowed
};

// Указатель на нулевой элемент массива:
int * p1 = http_non_retriable_errors;

int * p2 = &http_non_retriable_errors[5];

std::size_t dist = p2 - p1;

std::println("{}", dist);
```
```
5
```


![Вычитание указателей](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-16-2/illustrations/cpp/pointer_substraction.jpg) {.illustration}


Что выведет этот код, если размер `int` равен 4 байта? Напишите `err` в случае ошибки компиляции или `ub` в случае неопределённого поведения. {.task_text}

```cpp  {.example_for_playground}
import std;

std::size_t size_ptr(int * buf)
{
    return sizeof(buf);
}

int main()
{
    int raw_data[5] = {};
    std::size_t a = &raw_data[4] - raw_data;
    std::size_t b = sizeof(raw_data);

    std::println("{} {} {}",
                 a,
                 b,
                 b == size_ptr(raw_data));
}
```

```consoleoutput {.task_source #cpp_chapter_0152_task_0060}
```
Что возвращает `sizeof()` от указателя? {.task_hint}
```cpp {.task_answer}
4 20 false
```

В стандартной библиотеке есть функция `std::strlen()`, которая принимает сишную строку и возвращает её длину без учёта завершающего нулевого символа. {.task_text}

Напишите свою реализацию функции. Будем считать, что в неё не может попасть `nullptr`. {.task_text}

```cpp {.task_source #cpp_chapter_0152_task_0070}
std::size_t strlen(const char * str)
{

}
```
Заведите указатель на начало строки: `const char * end = str`. В цикле, пока значение по указателю не станет равным `'\0'`, увеличивайте указатель. Затем верните разность между указателем на `'\0'` и указателем на начало строки. В соответствии с правилами адресной арифметики получившееся значение равно количеству элементов между двумя указателями. {.task_hint}
```cpp {.task_answer}
std::size_t strlen(const char * str)
{
    const char * end = str;

    while (*end != '\0')
        ++end;

    return end - str;
}
```

Вы можете сравнить своё решение задачи с вариантом, предлагаемом [в разделе cppreference,](https://en.cppreference.com/w/cpp/string/byte/strlen.html) про `std::strlen()`.


----------

## Резюме
- Сишная строка (null-terminated string) — это массив `char`, завершённый символом `\0`. 
- Адресная арифметика заключается в применении к указателям операций сложения и вычитания, которые работают с учётом типа указателя.
- Прибавление к указателю целого числа увеличивает значение адреса на это число, домноженное на размер типа.
- Вычитание из указателя целого числа уменьшает значение адреса на число, домноженное на размер типа.
- Вычитание из указателя другого указателя имеет смысл, если оба указателя ссылаются на общий участок памяти и имеют одинаковый тип.
- Вычитание одного указателя из другого возвращает количество элементов типа `T` между ними.
