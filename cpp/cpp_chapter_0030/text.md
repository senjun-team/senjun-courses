# Глава 3. Управляющие конструкции

В этой главе вы познакомитесь с синтаксисом для условного выполнения кода и организации циклов.

## Условное выполнение кода

В C++ есть три варианта для условного выполнения кода:
- классический `if-else`,
- лаконичный тернарный оператор,
- сопоставление выражения с набором значений через `switch-case`.

### Условия if-else

Чтобы выполнять различные действия в зависимости от условия, используется конструкция `if-else`. Условие — это выражение, приводимое к `bool`:

```c++
if (condition) {
    // Обработка condition == true
} else {
    // Обработка condition == false
}
```

Пример:

```c++ {.example_for_playground .example_for_playground_001}
const std::string filename = "scale_2400.png";

if (filename.contains('.')) {
    std::println("Processing file: {}", filename);
} else {
    std::println("Please provide filename with extension");
    handle_user_input();
}
```

Ветка `else` опциональна:

```c++ {.example_for_playground .example_for_playground_002}
std::size_t cpu_count = 0;

if (cpu_count == 0) {
    std::println("Setting CPU count to default value");
    cpu_count = 4;
    return;
}
```

Дана сигнатура функции `vertical_flight_speed()`. Она вычисляет вертикальную скорость полета самолета. Для этого она принимает два показания высотомера и прошедшее между ними время. {.task_text}

Напишите тело этой функций, соответствующее многострочному комментарию в коде. Вам пригодятся: {.task_text}
- Константа `NAN` — not a number, [нечисло](https://en.cppreference.com/w/c/numeric/math/NAN).
- Константа `INFINITY` — [бесконечность](https://en.cppreference.com/w/cpp/numeric/math/INFINITY).
- Функция [std::abs()](https://en.cppreference.com/w/cpp/numeric/math/abs) для получения модуля числа.

```c++ {.task_source #cpp_chapter_0030_task_0010}
/* cur_height   — текущая высота, м.
   prev_height  — предыдущая высота, м.
   elapsed_time — время (с), за которое произошло изменение высоты.

   Возвращаемое значение — вертикальная скорость полета, м/c.
   Если значение одной из высот ниже -500 м, то возвращается NAN.
   Если время меньше либо равно 0, то возвращается INFINITY.
*/
double vertical_flight_speed(double cur_height,
                             double prev_height,
                             double elapsed_time) {

}
```
Пример раннего выхода из функции: `if (elapsed_time <= 0) return INFINITY;`. {.task_hint}
```c++ {.task_answer}
/* cur_height   — текущая высота, м.
   prev_height  — предыдущая высота, м.
   elapsed_time — время (с), за которое произошло изменение высоты..

   Возвращаемое значение — вертикальная скорость полета, м/с.
   Если значение одной из высот ниже -500 м, то возвращается NAN.
   Если время меньше либо равно 0, то возвращается INFINITY.
*/
double vertical_flight_speed(double cur_height,
                             double prev_height,
                             double elapsed_time) {
    const double min_height = -500.0;

    if (cur_height < min_height || prev_height < min_height) {
        return NAN;
    }

    if (elapsed_time <= 0) {
        return INFINITY;
    }

    return std::abs(cur_height - prev_height) / elapsed_time;
}
```

Фигурные скобки объединяют инструкции в блоки. Если требуется выполнить только одну инструкцию, их можно опустить:

```c++  {.example_for_playground .example_for_playground_003}
const std::string s = "Some user input";

if (s.empty())
    std::println("Empty input!");
else
    std::println("User entered: {}", s);
```

Это правило расстановки скобок действует и для циклов.

Если фигурные скобки нужны в одной из веток условия, то для единообразия лучше ставить их и в другой ветке.

Напишите функцию `is_valid()` для наивной валидации e-mail. Она должна принимать строку и возвращать `true`, если в строке соблюдены условия: есть символы `@` и `.`; после символа `@` есть `.`; `@` не идет первым. {.task_text}

Вам поможет метод строки [find().](https://en.cppreference.com/w/cpp/string/basic_string/find) Он принимает подстроку или символ и опциональный параметр - индекс, начиная с которого искать вхождение. По умолчанию это 0. Метод возвращает индекс первого вхождения либо константу `std::string::npos`, означающую, что подстрока не найдена. Тип возвращаемого значения — `std::size_t`. {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0020}

```
Пример проверки индекса символа: `email.find('.', i) != std::string::npos`. {.task_hint}
```c++ {.task_answer}
bool is_valid(std::string email) {
    const std::size_t i = email.find('@');
    if (i == std::string::npos || i == 0) {
        return false;
    }

    return email.find('.', i) != std::string::npos;
}
```

Условия `if-else` бывают вложенными.

Перед вами цепочка `if-else` для сравнения цвета из RGB-палитры с тремя значениями. Префикс `0x` нужен для обозначения шестнадцатеричных чисел:

```c++ {.example_for_playground .example_for_playground_004}
if (color_code == 0x80ED99) {
    std::println("Light green");
} else if (color_code == 0x22577A) {
    std::println("Lapis Lazuli");
} else if (color_code == 0xC7F9CC) {
    std::println("Tea green");
} else {
    std::println("Color not in this palette");
}
```

В длинных цепочках вложенных `if-else` легко допустить ошибку. И в некоторых случаях им на замену приходит конструкция `switch-case`, которую мы рассмотрим дальше.

### Тернарный оператор

Тернарный оператор (ternary operator) позволяет компактно записывать простые условия. А называется он так, потому что состоит из трех выражений: условия и веток выполнения.

```
(condition) ? expression_for_true_branch : expression_for_false_branch;
```

Например:

```c++ {.example_for_playground .example_for_playground_005}
std::println("{}", price < 250 ? "cheap" : "expensive");
```

Условие записывается до символа `?`. За ним следует выражение, выполняющееся, если условие истинно. И после `:` указывается выражение для случая, если условие ложно.

Перепишем этот `if-else` на тернарный оператор:

```c++
int status_code = 0;

if (request_body_len > max_len) {
    status_code = -1;
} else {
    status_code = handle_request();
}
```

Отказ от `if-else` позволяет сделать переменную `status_code` константой:

```c++
const int status_code = (request_body_len > max_len) ? -1 : handle_request();
```

Скомпилируется ли этот код? Зависит от того, какой тип возвращает `handle_request()`! Дело в том, что типы возвращаемых тернарным оператором значений должны быть приводимы один к другому. Поэтому если `handle_request()` возвращает `int` или `bool`, код скомпилируется. А если `void`, то нет:

```
error: right operand to ? is void, but left operand is of type 'int'
```

Перепишите функцию `max()` с использованием тернарного оператора. Тело функции должно состоять из единственной инструкции. {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0030}
int max(int a, int b) {
    if (a > b) {
        return a;
    }

    return b;
}
```
Оператор `return` должен вернуть результат применения тернарного оператора к условию `a > b`. {.task_hint}
```c++ {.task_answer}
bool max() {
    return a > b ? a : b;
}
```

Кстати, в стандартной библиотеке C++ есть функции [std::max()](https://en.cppreference.com/w/cpp/algorithm/max) и [std::min()](https://en.cppreference.com/w/cpp/algorithm/min). При работе над реальными проектами руководствуйтесь правилом: [используй готовое.](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-lib) То есть прежде чем писать функцию самостоятельно, проверьте — вдруг она уже реализована в стандартной библиотеке?

### Конструкция switch-case

Конструкция `switch-case` удобна, когда требуется сравнивать выражение с набором константных значений. Это [более читабельная](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-switch-if) замена вложенных `if-else`:

```c++
switch(expression) {
    case val_1: 
        instructions_1;
        break;
    case val_2:
        instructions_2;
        break;
    case val_n:
        instructions_n;
        break;
    default:
        instructions;
}
```

Но у `switch-case` есть ограничения. Выражение для `switch` должно быть:
- целочисленным (например, `int`),
- символьного типа (например, `char`),
- либо перечислением `enum` (о котором вы скоро узнаете).

То есть сравнивать строку `std::string` с использованием `switch` не получится.

Так выглядит `switch-case` для сопоставления символа со значениями:

```c++  {.example_for_playground .example_for_playground_006}
char user_input = 'y';

switch(user_input) {
    case '\n': 
        std::println("User pressed enter. Repeating question.");
        show_question();
        break;
    
    case 'y':
        std::println("Yes");
        break;
    
    case 'n':
        std::println("No");
        break;
    
    default:
        std::println("Invalid input");
}
```
```
Yes
```

Тело `switch` обрамляется фигурными скобками. Вокруг инструкций в блоках `case` и `default` скобки не ставятся.

Блок `default` опциональный. Он срабатывает, если не подошел ни один из блоков `case`.

В большинстве случаев после выполнения `case` требуется выйти из `switch`. Для этого используется оператор `break`. Если его нет, то выполнение продолжится до следующего `break` или до самого конца `switch`:

```c++   {.example_for_playground .example_for_playground_007}
const std::size_t number_system = 10;

switch(number_system) {
    case 2: 
        std::println("Binary");
    
    case 10:
        std::println("Decimal");
    
    case 16:
        std::println("Hexadecimal");
    
    default:
        std::println("Other");
}
```
```
Decimal
Hexadecimal
Other
```

Забытый `break` — это _самая распространенная_ ошибка при использовании `switch-case`.

Что выведется в консоль? {.task_text}

```c++ {.example_for_playground .example_for_playground_008}
const std::size_t mark = 3;

switch(mark) {
    case 1:
        std::print("e");
    case 2:
        std::print("d");
    case 3:
        std::print("c");
    case 4:
        std::print("b");
    case 5:
        std::print("a");
}
```

```consoleoutput {.task_source #cpp_chapter_0030_task_0040}
```
Условие попадает под `case 3`, поэтому выполнится его блок. Так как в блоке отсутствует `break`, выполнятся следующие блоки `case`. {.task_hint}
```cpp {.task_answer}
cba
```

Бывают и случаи, когда `break` _специально_ не ставится. Если несколько разных значений нужно обработать одинаково, то соответствующие им блоки `case` идут рядом. Первые остаются пустыми, а в последний размещается необходимая обработка.

Что выведется в консоль? {.task_text}

```c++  {.example_for_playground .example_for_playground_009}
const std::size_t mark = 1;

switch(mark) {
    case 1:
    case 2:
    case 3:
        std::println("bad");
        break;
    case 4:
        std::print("good");
        break;
    case 5:
        std::print("excellent");
        break;
    default:
        std::print("-");
}
```

```consoleoutput {.task_source #cpp_chapter_0030_task_0050}
```
Условие попадает под `case 1`. Это пустой блок, в котором нет `break`. Поэтому выполнится следующий блок `case `, а затем и `case 3`. В `case 3` есть `break`, прерывающий `switch`.  {.task_hint}
```cpp {.task_answer}
bad
```

Перепишите эту функцию с применением `switch-case`. {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0060}
void log_state(int code) {
    std::string state = "";

    if (code == 0) {
        state = "Operation succeded";
    } else if (code == 1) {
        state = "Still in progress";
    } else if (code == 2) {
        state = "Aborted";
    } else {
        state = "Invalid state";
    }

    std::println("State of user's operation: {}", state);
}
```
Не забудьте про `break` и `default`. {.task_hint}
```c++ {.task_answer}
void log_state(int code) {
    std::string state = "";

    switch(code) {
        case 0:
            state = "Operation succeded";
            break;
        case 1:
            state = "Still in progress";
            break;
        case 2:
            state = "Aborted";
            break;
        default:
            state = "Invalid state";
    }

    std::println("State of user's operation: {}", state);
}
```

## Циклы

В C++ вам на выбор доступно три вида циклов: `while`, `do-while` и `for`. Причем у цикла `for` есть два варианта.

Для досрочного выхода из цикла применяется оператор `break`. А для прерывания текущей итерации и перехода на следующую — оператор `continue`.

### Цикл while

Тело цикла `while` выполняется, пока справедливо условие:

```c++
while(condition) {
    // Тело цикла
}
```

Его используют, если в теле цикла требуется изменять его условие. Либо если количество итераций не известно заранее:

```c++
while (!user_pressed_stop && !end_of_playlist) {
    play_next_song();
}
```

Еще один распространенный сценарий применения `while` — вечный цикл:

```c++
while(true) {
    cmd = read_console_command();

    if (!is_valid(cmd)) {
        continue;
    }

    if (is_exit(cmd)) {
        break;
    }

    handle_command(cmd);
}
```

Напишите функцию `gcd()`, которая находит наибольший общий делитель (GCD, greatest common divisor) чисел `a` и `b` типа `std::size_t`. Функция не должна быть рекурсивной. {.task_text}

Например, `gcd(25, 15)` равен 5, а `gcd(8, 3)` равен 1.  {.task_text}

Реализуйте алгоритм Евклида. Он заключается в следующем: {.task_text}
- GCD равен `a`, если `a` и `b` совпадают.
- GCD равен GCD от `a - b` и `b`, если `a` больше `b`.
- GCD равен GCD от `a` и `b - a`, если `a` меньше `b`.

```c++ {.task_source #cpp_chapter_0030_task_0070}
```
В цикле `while` проверяйте условие `a != b`. Внутри цикла изменяйте значения `a` и `b`. После выхода из цикла `a` будет равен наибольшему общему делителю. {.task_hint}
```c++ {.task_answer}
std::size_t gcd(std::size_t a, std::size_t b) {
    while (a != b) {
        if (a > b) {
            a -= b;
        } else {
            b -= a;
        }
    }

    return a;
}
```

К слову, в стандартной библиотеке C++ есть функция [std::gcd()](https://en.cppreference.com/w/cpp/numeric/gcd).

Для обхода элементов контейнера идиоматичнее использовать цикл `for`. Но никто не может запретить вам организовать обход через `while`:

```c++ {.example_for_playground .example_for_playground_010}
std::string s = "341453TNY";
std::size_t i = 0;

while (i < s.size() && std::isdigit(s[i])) {
    s[i] = 'X';
    ++i;
}

std::println("{}", s);
```
```
XXXXXXTNY
```

### Цикл do-while

Цикл `do-while` отличается от `while` порядком действий: сначала исполняется тело цикла, а затем проверяется условие. Это означает, что вне зависимости от условия тело выполнится хотя бы один раз.

```c++
do {
    // Тело цикла
} while(condition);
```

К циклу `do-while` часто прибегают при чтении данных из внешнего источника. Первое чтение произойдет в любом случае, а последующие — если останутся данные:

```c++
int data_size = 0;

do {
    data_size = read_chunked_data();
} while (data_size > 0);
```

### Цикл for

Цикл `for` удобен для обхода элементов контейнера и для выполнения действий заранее известное количество раз. Он применяется, если в теле цикла не требуется изменять его условие.

Вместо формального определения начнем с примера. Перепишем этот `while` на `for`:

```c++  {.example_for_playground .example_for_playground_011}
int i = 5;

while(i <= 25) {
    std::print("{} ", i);
    i += 5;
}
```
```
5 10 15 20 25
```

Так выглядит цикл `for`, делающий то же самое:

```c++  {.example_for_playground .example_for_playground_012}
for (int i = 5; i <= 25; i += 5) {
    std::print("{} ", i);
}
```

Внутри круглых скобок цикла `for` точкой с запятой разделены три выражения:
- Инициализация. Срабатывает один раз в начале цикла. Мы создали счетчик `i` и присвоили ему стартовое значение 5.
- Условие. Проверяется перед каждым выполнением тела цикла. Как только условие возвращает `false`, цикл прерывается. В нашем случае условие — это `i <= 25`.
- Итерация. Это действие, совершаемое после завершения каждого витка цикла для перехода на новый. Здесь мы увеличиваем счетчик: `i += 5`.

Так цикл `for` выглядит в общем виде:

```c++
for (initialization; condition; iteration) {
    // Тело цикла
}
```

Любое из этих выражений может отсутствовать. Если опустить все три, то получится вечный цикл:

```c++
for(;;) {
    // Бесконечный цикл
}
```

Напишите функцию `common_prefix_len()`, которая принимает две строки и возвращает `std::size_t` — длину их общего префикса. {.task_text}

Например, для строк `"sort"`, `"something"` функция вернет 2, потому что общий префикс равен `"so"`. А для строк `"test"`, `"cow"` функция вернет 0. {.task_text}

В своем решении используйте цикл `for`.  {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0080}
```
В инициализаторе цикла заведите счетчик. В условии цикла проверьте, что он меньше длин обеих строк. В теле цикла сравните символы строк по этому индексу. {.task_hint}
```c++ {.task_answer}
std::size_t common_prefix_len(std::string s1, std::string s2) {
    std::size_t len = 0;

    for (std::size_t i = 0; i < std::min(s1.size(), s2.size()); ++i) {
        if (s1[i] != s2[i]) {
            break;
        }

        ++len;
    }

    return len;
}
```

Напишите функцию, которая выводит в консоль таблицу умножения чисел от 1 до `n` в вертикальном представлении. {.task_text}

Если `n` меньше 1, функция не должна ничего выводить. {.task_text}

Для вывода текста с переносом строки есть функция `std::println()` (вызов от пустой строки `""` приведет к переносу строки). Для вывода без переноса есть функция `std::print()`. {.task_text}

Пример вывода для `n=3`: {.task_text}

```
1*1=1 2*1=2 3*1=3
1*2=2 2*2=4 3*2=6
1*3=3 2*3=6 3*3=9
```

```c++ {.task_source #cpp_chapter_0030_task_0090}
void show_multiplication_table(int n) {

}
```
Организуйте вложенный цикл. {.task_hint}
```c++ {.task_answer}
void show_multiplication_table(int n) {
    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= n; ++j) {
            const std::string trailing_symbol = (j == n) ? "" : " ";
            std::print("{}*{}={}{}", j, i, j*i, trailing_symbol);
        }

        std::println();
    }
}
```

### Цикл range-for

С помощью `for` можно организовать еще один вариант циклов. Он известен как `range-for` и применяется для итерации по диапазону значений. Например, по контейнеру.

```c++  {.example_for_playground .example_for_playground_013}
std::string s = "MAX";

for (char c: s) {
    if (std::tolower(c) == 'a') {
        std::println("Found char!");
    }
}
```
```
M
A
X
```

Общий вид цикла `range-for`:

```c++
for(item-initialization: range-initialization) {
    // Тело цикла
}
```

В круглых скобках цикла заводится переменная того же типа, что и элементы диапазона. Затем после двоеточия указывается сам диапазон.

Напишите функцию `count_letter()`, которая принимает два аргумента: строку и символ. Она должна вернуть значение типа `std::size_t` — количество вхождений данного символа в строку. {.task_text}

В своем решении воспользуйтесь циклом range-for. {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0100}

```
Пример цикла: `for (char letter: s)`. {.task_hint}
```c++ {.task_answer}
std::size_t count_letter(std::string s, char c) {
    std::size_t count = 0;

    for (char letter: s) {
        if (letter == c) {
            ++count;
        }
    }

    return count;
}
```

## Резюме
- Для условного выполнения кода предусмотрены конструкции: `if-else`, тернарный оператор и `switch-case`.
- Внутри блоков `switch-case` не забывайте ставить `break`.
- Организовать цикл можно с помощью конструкций: `while`, `do-while`, `for`.
- Оператор `break` нужен для досрочного выхода из цикла.
- Оператор `continue` прерывает итерацию цикла и запускает следующую.
