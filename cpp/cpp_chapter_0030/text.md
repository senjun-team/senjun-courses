# Глава 3. Условия

Вы познакомитесь с тремя вариантами условного выполнения кода:
- классическим `if-else`,
- лаконичным тернарным оператором,
- сопоставлением выражения с набором значений через `switch-case`.

## Условия if-else

Чтобы выполнять различные действия в зависимости от условия, используется конструкция `if-else`. Условием может быть любое выражение, приводимое к `bool`:

```c++
if (условное выражение)
    Инструкция для true
else
    Инструкция для false
```

Ветка `else` может отсутствовать.

Каждая ветка условия состоит строго из одной инструкции (statement). Как вы [помните,](/courses/cpp/chapters/cpp_chapter_0020/#block-statements) инструкции (statements) — это фрагменты кода, выполняемые последовательно. И они бывают трех видов: простые, составные и управляющие.

С простыми инструкциями вы уже знакомы. Они оканчиваются точкой с запятой:

```c++
return 0;
```

```c++
char quit = 'Q';
```

```c++
std::println("{}", a > b);
```

Составная инструкция (compound statement) — это обернутые фигурными скобками простые инструкции. Составные инструкции называют блоками.

В этом примере каждая ветка условия — это блок:

```c++ {.example_for_playground .example_for_playground_001}
const std::string filename = "scale_2400.png";

if (filename.contains('.'))
{
    std::println("Processing file: {}", filename);
}
else
{
    std::println("Please provide filename with extension");
    handle_user_input();
}
```

И третий вид инструкций — управляющие. К ним, например, относятся условия и циклы. В данном примере `if-else` — это одна управляющая инструкция, ветки которой содержат по блоку:

```c++
if (a > b)
{
    // ...
}
else
{
    // ...
}
```

### Когда фигурные скобки обязательны, а когда — опциональны? {#block-braces}

Каждая ветка условия содержит _только одну_ инструкцию, будь то простая, составная или управляющая инструкция. Отсюда следуют правила расстановки фигурных скобок:
- Если в ветке условия требуется выполнить больше одной инструкции, то такие инструкции объединяются в блок с помощью фигурных скобок. То есть превращаются в составную инструкцию.
- Если же требуется выполнить только одну инструкцию, фигурные скобки **можно** опустить.

Если фигурные скобки нужны только в одной из веток условия, то для единообразия лучше ставить их и в другой ветке.

Эти же правила расстановки фигурных скобок действуют и для тела циклов.

В данном случае скобки обязательны:

```c++ {.example_for_playground .example_for_playground_002}
std::size_t cpu_count = 0;

if (cpu_count == 0)
{
    std::println("Setting CPU count to default value");
    cpu_count = 4;
}
```

А здесь скобки можно опустить:

```c++  {.example_for_playground .example_for_playground_003}
const std::string s = "Some user input";

if (s.empty())
    std::println("Empty input!");
else
    std::println("User entered: {}", s);
```

Чему равно значение `a`? {.task_text}

```c++
int a = 3;

if (++a < 4)
    a++;
    a *= 4;
```

```consoleoutput {.task_source #cpp_chapter_0030_task_0070}
```
Префиксный инкремент сначала увеличивает `a` на 1, а потом возвращает значение. Поэтому условие в `if` не выполняется: `a` равно 4. Тело `if` состоит из единственной инструкции `a++`. Вторая инструкция выполнится безусловно. {.task_hint}
```cpp {.task_answer}
16
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
                             double elapsed_time)
{

}
```
Пример раннего выхода из функции: `if (elapsed_time <= 0) return INFINITY;`. {.task_hint}
```c++ {.task_answer}
/* cur_height   — текущая высота, м.
   prev_height  — предыдущая высота, м.
   elapsed_time — время (с), за которое произошло изменение высоты.

   Возвращаемое значение — вертикальная скорость полета, м/с.
   Если значение одной из высот ниже -500 м, то возвращается NAN.
   Если время меньше либо равно 0, то возвращается INFINITY.
*/
double vertical_flight_speed(double cur_height,
                             double prev_height,
                             double elapsed_time)
{
    const double min_height = -500.0;

    if (cur_height < min_height || prev_height < min_height)
    {
        return NAN;
    }

    if (elapsed_time <= 0)
    {
        return INFINITY;
    }

    return std::abs(cur_height - prev_height) / elapsed_time;
}
```

Напишите функцию `is_valid()` для наивной валидации e-mail. Она должна принимать строку и возвращать `true`, если в строке соблюдены условия: есть символы `@` и `.`; после символа `@` есть `.`; `@` не идет первым. {.task_text}

Вам поможет метод строки [find().](https://en.cppreference.com/w/cpp/string/basic_string/find) Он принимает подстроку или символ и опциональный параметр — индекс, начиная с которого искать вхождение. По умолчанию это 0. Метод возвращает индекс первого вхождения либо константу `std::string::npos`, означающую, что подстрока не найдена. Тип возвращаемого значения — `std::size_t`. {.task_text}

```c++ {.task_source #cpp_chapter_0030_task_0020}

```
Пример проверки индекса символа: `email.find('.', i) != std::string::npos`. {.task_hint}
```c++ {.task_answer}
bool is_valid(std::string email)
{
    const std::size_t i = email.find('@');
    if (i == std::string::npos || i == 0)
    {
        return false;
    }

    return email.find('.', i) != std::string::npos;
}
```

### Вложенные условия

В C++ нет конструкций наподобии `if-elif-else` как в Python, позволяющих внутри одного условия организовать неограниченное количество проверок. На помощь приходят вложенные условия.

Перед вами цепочка `if-else` для сравнения цвета из RGB-палитры с тремя значениями. Префикс `0x` нужен для обозначения шестнадцатеричных чисел:

```c++
if (color_code == 0x80ED99)
{
    std::println("Light green");
}
else
{
    if (color_code == 0x22577A)
    {
        std::println("Lapis Lazuli");
    }
    else
    {
        if (color_code == 0xC7F9CC)
        {
            std::println("Tea green");
        }
        else
        {
            std::println("Color not in this palette");
        }
    }
}
```

Этот код «лесенкой» выглядит ужасно. Поэтому подобные вложенные условия лучше форматировать без отступов и избыточных фигурных скобок. Ведь `if-else` — это инструкция, а вокруг единственной инструкции скобки можно не ставить. Такой код проще воспринимать:

```c++ {.example_for_playground .example_for_playground_004}
if (color_code == 0x80ED99)
{
    std::println("Light green");
} else if (color_code == 0x22577A)
{
    std::println("Lapis Lazuli");
} else if (color_code == 0xC7F9CC)
{
    std::println("Tea green");
} else
{
    std::println("Color not in this palette");
}
```

Но в длинных цепочках вложенных `if-else` все равно легко допустить ошибку. И в некоторых случаях им на замену приходит конструкция `switch-case`, которую мы рассмотрим дальше.

## Тернарный оператор

Тернарный оператор (ternary operator) позволяет компактно записывать простые условия. А называется он так, потому что состоит из трех выражений: условия и веток выполнения.

```
(условное выражение) ? выражение для true : выражение для false;
```

Например:

```c++ {.example_for_playground .example_for_playground_005}
std::println("{}", price < 250 ? "cheap" : "expensive");
```

Условие записывается до символа `?`. За ним следует выражение, выполняющееся, если условие истинно. И после `:` указывается выражение для случая, если условие ложно.

Перепишем этот `if-else` на тернарный оператор:

```c++
int status_code = 0;

if (request_body_len > max_len)
{
    status_code = -1;
}
else
{
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
int max(int a, int b)
{
    if (a > b)
    {
        return a;
    }

    return b;
}
```
Оператор `return` должен вернуть результат применения тернарного оператора к условию `a > b`. {.task_hint}
```c++ {.task_answer}
int max(int a, int b)
{
    return a > b ? a : b;
}
```

Кстати, в стандартной библиотеке C++ есть функции [std::max()](https://en.cppreference.com/w/cpp/algorithm/max) и [std::min()](https://en.cppreference.com/w/cpp/algorithm/min). При работе над реальными проектами руководствуйтесь правилом: [используй готовое.](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-lib) То есть прежде чем писать функцию самостоятельно, проверьте — вдруг она уже реализована в стандартной библиотеке?

## Конструкция switch-case

Конструкция `switch-case` удобна, когда требуется сравнивать выражение с набором константных значений. Это [более читабельная](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-switch-if) замена вложенных `if-else`:

```c++
switch(выражение)
{
    case значение_1: 
        инструкции_1;
        break;
    case значение_2:
        инструкции_2;
        break;
    case значение_n:
        инструкции_n;
        break;
    default:
        инструкции;
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

switch(user_input)
{
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

Тело `switch` **обязательно** обрамляется фигурными скобками. Вокруг инструкций в блоках `case` и `default` скобки не ставятся.

Блок `default` опциональный. Он срабатывает, если не подошел ни один из блоков `case`.

### Оператор break

В большинстве случаев после выполнения `case` требуется выйти из `switch`. Для этого используется оператор `break`. Если его нет, то выполнение **продолжится** до следующего `break` или до самого конца `switch`:

```c++   {.example_for_playground .example_for_playground_007}
const std::size_t number_system = 10;

switch(number_system)
{
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

switch(mark)
{
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

switch(mark)
{
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
void log_state(int code)
{
    std::string state = "";

    if (code == 0)
    {
        state = "Operation succeded";
    } else if (code == 1)
    {
        state = "Still in progress";
    } else if (code == 2)
    {
        state = "Aborted";
    } else {
        state = "Invalid state";
    }

    std::println("State of user's operation: {}", state);
}
```
Не забудьте про `break` и `default`. {.task_hint}
```c++ {.task_answer}
void log_state(int code)
{
    std::string state = "";

    switch(code)
    {
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

----------

## Резюме

- Инструкции (statements) — это фрагменты кода, выполняемые последовательно.
- Инструкции бывают простыми, составными и управляющими.
- Составные инструкции — это инструкции, объединенные в блок с помощью фигурных скобок.
- Для условного выполнения кода предусмотрены конструкции: `if-else`, тернарный оператор и `switch-case`.
- Конструкции `if-else` и `switch-case` относятся к управляющим инструкциям.
- Внутри блоков `switch-case` не забывайте ставить оператор `break`.
