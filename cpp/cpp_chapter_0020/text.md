# Глава 2. Простые функции

Так выглядит минимальная программа на C++, которая ничего не делает:

```c++
int main() { }
```

Она состоит из единственной функции.

## Точка входа в программу

Функция с именем `main` — это точка входа в программу (entry point). Ее наличие обязательно: после запуска программы управление передается именно ей.

Функция `main()` возвращает [целое число](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Rf-main) вызвавшему программу окружению. Это статус завершения:
- 0 в случае успеха,
- другое значение в случае ошибки.

В нашем примере тело функции пустое: `{ }`. Но как же тогда формируется статус завершения? Функция `main()` — особая: при отсутствии явно возвращаемого значения она возвращает 0. Для наглядности мы можем вернуть его явно:

```c++
int main() { 
    return 0;
}
```

## Сигнатура и тело функции

При объявлении функции после ее сигнатуры указывается тело, обрамленное фигурными скобками:

![Функции в C++](https://raw.githubusercontent.com/senjun-team/senjun-courses/cpp_course_chapter_func/illustrations/cpp/function.jpg) {.illustration}

Напомним, что **параметр** — это имя в определении функции. А **аргумент** — это фактическое значение, переданное функции при вызове.

Например, функция `is_error()` принимает параметр `http_code` типа `int`, а внутри `main()` вызывается с аргументом 404:

```c++
import std;

bool is_error(int http_code) {
    return http_code >= 300;
}

int main() { 
    const bool res = is_error(404);
    std::println("404 is error code? {}", res);
    return 0;
}
```
```
404 is error code? true
```

Обратите внимание на [ключевое слово](https://en.cppreference.com/w/cpp/language/cv) `const` возле типа переменной `res`. Это квалификатор типа, означающий, что  переменная не может быть изменена, то есть является **константой.** 

Мы могли бы обойтись и без него: `bool res = ...`. Но делать константами все переменные, которые не требуется изменять — это [отличная практика.](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Rconst-immutable) Она предотвращает неприятные ошибки. Например, случайную перезапись переменной, которую не планировалось изменять. Придерживайтесь этой практики и не забывайте использовать `const`.

Для вывода `res` в консоль мы сделали две вещи:
- Импортировали стандартную библиотеку `std`. В ней содержится [функция](https://en.cppreference.com/w/cpp/io/println) `println()`, отвечающая за форматированный вывод.
- Вызвали `println()`. Она находится в пространстве имен (namespace) `std`, и мы указали его при вызове: `std::println()`.

Напишите функцию `to_fahrenheit()`. Она принимает вещественное число — температуру в градусах по Цельсию, и возвращает градусы по шкале Фаренгейта. {.task_text}

Формула: `°F = (°C × 9.0/5.0) + 32.0`. {.task_text}

```c++ {.task_source #cpp_chapter_0020_task_0020}
import std;

// Your code here
```
Возвращаемое функцией значение, если параметр называется `celsius`: `celsius * 9.0 / 5.0 + 32.0`. {.task_hint}
```c++ {.task_answer}
double to_fahrenheit(double celsius) {
    return celsius * 9.0 / 5.0 + 32.0;
}
```

## Возвращаемое значение

Для возврата из функции значения используется оператор `return`:

```c++
return 128;
```

Если функция ничего не возвращает, то возвращаемым типом указывается `void`:

```c++
void process_request();
```

Вызывать `return` в конце такой функции не обязательно. Но его можно использовать для раннего выхода (early exit):

```c++
if (!is_connection_opened) {
    return;
}
```

Функция `solve_quadratic_equation()` принимает коэффициенты `a`, `b` и `c` квадратного уравнения вида `a * x * x + b * x + c = 0`. {.task_text}

В функции не обработаны два случая. Исправьте это: {.task_text}
- Если `a` равен 0 (`a == 0`), функция должна выводить текст `"The equation is linear, not quadratic"`.
- Если дискриминант меньше нуля, функция должна выводить `"The equation doesn't have real solutions"`.

```c++ {.task_source #cpp_chapter_0020_task_0040}
import std;

void solve_quadratic_equation(int a, int b, int c) {
    const double d = b * b - 4 * a * c;
    const double x1 = (-b + std::sqrt(d)) / (2 * a);
    const double x2 = (-b - std::sqrt(d)) / (2 * a);

    std::println("Equation: {}x^2 + {}x + {} = 0", a, b, c);
    std::println("Solutions: {}, {}", x1, x2);
}
```
Добавьте ранний выход в самом начале функции и после расчета дискриминанта `d`. {.task_hint}
```c++ {.task_answer}
void solve_quadratic_equation(int a, int b, int c) {
    if (a == 0) {
        std::println("The equation is linear, not quadratic");
        return;
    }
    
    const double d = b * b - 4 * a * c;
    if (d < 0) {
        std::println("The equation doesn't have real solutions");
        return;
    }
    
    const double x1 = (-b + std::sqrt(d)) / (2 * a);
    const double x2 = (-b - std::sqrt(d)) / (2 * a);

    std::println("Equation: {}x^2 + {}x + {} = 0", a, b, c);
    std::println("Solutions: {}, {}", x1, x2);
}
```

## Аргументы

В функцию можно передавать аргументы несколькими способами. В этой главе мы рассмотрим передачу по значению (by value) и по константному значению. 

### Передача аргументов по значению

При передаче аргумента по значению функция работает с его копией. И все изменения объекта внутри функции никак не отражаются снаружи.

Мы изменяем значение `n` внутри функции `show_reversed_number()`. Однако за ее пределами значение остается прежним:

```c++
import std;

void show_reversed_number(int n) {
    if (n == 0) {
        std::println(n);
        return;
    }

    while (n != 0) {
       // Оператор % - это остаток от деления:
       std::print("{}", n % 10);

       n = n / 10;
    }

  std::println();
}

int main() {
    int val = 1024;

    std::println("Before function call: {}", val);
    show_reversed_number(val);
    std::println("After function call: {}", val);
}
```
```
Before function call: 1024
4201
After function call: 1024
```

Напишите функцию `gcd()`, которая находит наибольший общий делитель (GCD, greatest common divisor) целых чисел `a` и `b`. Функция должна быть рекурсивной. {.task_text}

Например, `gcd(25, 15)` должен быть равен 5, а `gcd(8, 3)` должен быть равен 1.  {.task_text}

Используйте алгоритм Евклида. Он заключается в следующем: {.task_text}
- GCD равен `a`, если `a` и `b` совпадают.
- GCD равен GCD от `a - b` и `b`, если `a` больше `b`.
- GCD равен GCD от `a` и `b - a`, если `a` меньше `b`.

```c++ {.task_source #cpp_chapter_0020_task_0030}
```
В теле функции рекурсивно вызовите `gcd()` для случаев, если `a > b` и `a < b`. Для случая `a == b` просто верните `a`. {.task_hint}
```c++ {.task_answer}
import std;

int gcd(int a, int b) {
    if (a == b) {
        return a;
    }

    if (a > b) {
        return gcd(a - b, b);
    }

    return gcd(a, b - a);
}
```

Кстати, в стандартной библиотеке C++ есть [функция](https://en.cppreference.com/w/cpp/numeric/gcd) `std::gcd()`, которая написана более эффективно. Самостоятельно реализовывать функции из стандартной библиотеки полезно для получения опыта. Но при работе над реальными проектами руководствуйтесь [правилом:](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Res-lib) прежде чем писать функцию самому, нужно убедиться, что ее нет в стандартной библиотеке. [Cppreference](https://en.cppreference.com) в помощь.

### Передача аргументов по константному значению

Параметр функции можно сделать константным:

```c++
int draw_progress_bar(const int pct_complete);
```

Так поступают, если требуется подчеркнуть, что параметр не должен изменяться внутри функции. Однако эта практика не очень распространена.

## Объявления и определения

Функция становится видна компилятору только после точки своего объявления. Если вызвать ее _до_ места в коде, где она объявлена, компилятор выдаст ошибку.

Убедимся в этом на примере, в котором есть _взаимная рекурсия_ – вызов функциями друг друга. Пример реализует [гипотезу Коллатца:](https://ru.wikipedia.org/wiki/%D0%93%D0%B8%D0%BF%D0%BE%D1%82%D0%B5%D0%B7%D0%B0_%D0%9A%D0%BE%D0%BB%D0%BB%D0%B0%D1%82%D1%86%D0%B0) какое бы натуральное число `n` мы ни взяли, рано или поздно мы получим единицу, если будем совершать действия:
- Если `n` четное, делить его на 2.
- Если `n` нечетное, умножать на 3 и прибавлять 1. 

В `main()` мы заводим натуральное число `n`. И в цикле пока `n` не станет равно 1, применяем к нему взаимно рекурсивные функции `collatz_multiply()` и `collatz_divide()`:

```c++
import std;

int collatz_multiply(int x) {
    if (x % 2 > 0) {
        return 3 * x+ 1;
    }
    return collatz_divide(x);
}

int collatz_divide(int x) {
    if (x % 2 == 0) {
        return x / 2;
    }
    return collatz_multiply(x);
}

int main() {
    int n = 17;
    std::println("Checking Collatz conjecture for {}", n);
    
    while (n > 1) {
        n = collatz_multiply(n);
        std::print("{} ", n);
    }
}
```
```
main.cpp:7:12: error: use of undeclared identifier 'collatz_divide'
    7 |     return collatz_divide(x);
      |            ^
```

Этот код не компилируется: компилятор не находит функцию, использованную раньше своего объявления. И мы не можем поправить это простым переносом функции выше места ее вызова, ведь `collatz_multiply()` вызывает `collatz_divide()` и наоборот!

Кроме того, код может быть достаточно сложным, чтобы подходящего места для размещения всех функций _до_ их вызовов просто бы не нашлось.

На помощь приходят **объявления** (declarations). Объявление делает функцию видимой для компилятора. Объявление — это только сигнатура функции без реализации:

```c++
int collatz_multiply(int x);
```

А все примеры функций, которые вы успели увидеть в этой главе, являются **определениями** (definitions). Определение — это реализация функции: ее сигнатура и тело. Любое определение также является и объявлением.

Чтобы исправить наш пример кода, разместим объявления функций до их использования:
```c++
import std;

int collatz_multiply(int x);
int collatz_divide(int x);

int main() {
    int n = 17;
    std::println("Checking Collatz conjecture for {}", n);
    
    while (n > 1) {
        n = collatz_multiply(n);
        std::print("{} ", n);
    }
}

int collatz_multiply(int x) {
    if (x % 2 > 0) {
        return 3 * x+ 1;
    }
    return collatz_divide(x);
}

int collatz_divide(int x) {
    if (x % 2 == 0) {
        return x / 2;
    }
    return collatz_multiply(x);
}
```
```
Checking Collatz conjecture for 17
52 26 13 40 20 10 5 16 8 4 2 1 
```

Перед вами объявление функции `vertical_flight_speed()`. Она вычисляет вертикальную скорость полета самолета. Для этого она принимает два показания высотомера и прошедшее между ними время. {.task_text}

Напишите определение этой функций, соответствующее многострочному комментарию в коде. Вам пригодятся: {.task_text}
- Константа `NAN` — not a number, [нечисло](https://en.cppreference.com/w/c/numeric/math/NAN).
- Константа `INFINITY` — [бесконечность](https://en.cppreference.com/w/cpp/numeric/math/INFINITY). Обе константы объявлены в хедере `cmath`.
- Оператор `||` (логическое «ИЛИ») для проверки нескольких условий в рамках одной конструкции `if`.
- [Функция](https://en.cppreference.com/w/cpp/numeric/math/abs) `std::abs()` для получения модуля числа.

```c++ {.task_source #cpp_chapter_0030_task_0010}
#include <cmath>

import std;

/* cur_height   — текущая высота, м.
   prev_height  — предыдущая высота, м.
   elapsed_time — время (с), за которое произошло изменение высоты.

   Возвращаемое значение — вертикальная скорость полета, м/c.
   Если значение одной из высот ниже 0, то возвращается NAN.
   Если время меньше либо равно 0, то возвращается INFINITY.
*/
double vertical_flight_speed(double cur_height,
                             double prev_height,
                             double elapsed_time);
```
Пример раннего выхода из функции: `if (elapsed_time <= 0) return INFINITY;`. {.task_hint}
```c++ {.task_answer}
#include <cmath>

import std;

/* cur_height   — текущая высота, м.
   prev_height  — предыдущая высота, м.
   elapsed_time — время (с), за которое произошло изменение высоты..

   Возвращаемое значение — вертикальная скорость полета, м/с.
   Если значение одной из высот ниже 0, то возвращается NAN.
   Если время меньше либо равно 0, то возвращается INFINITY.
*/
double vertical_flight_speed(double cur_height,
                             double prev_height,
                             double elapsed_time) {
    if (cur_height < 0 || prev_height < 0) {
        return NAN;
    }

    if (elapsed_time <= 0) {
        return INFINITY;
    }

    return std::abs(cur_height - prev_height) / elapsed_time;
}
```

Объявлений одной и той же функции в программе может быть сколь угодно много. Но определение должно быть только одно. Так гласит один из пунктов [правила одного определения](https://en.cppreference.com/w/cpp/language/definition) (ODR, one definition rule). Нарушение ODR приведет к ошибке компиляции.

Что выведет этот код? {.task_text}

В случае ошибки напишите `err`. {.task_text}

```c++ {.example_for_playground .example_for_playground_003}
import std;

int main() {
    std::println("{}", to_miles(0.0));
}

double to_miles(double km) {
    return km * 0.62;
}
```

```consoleoutput {.task_source #cpp_chapter_0020_task_0050}
```
Функция `to_miles()` объявлена после ее вызова.  {.task_hint}
```cpp {.task_answer}
err
```

## Оформление кода

Вы успели поработать с кодом на C++ и наверняка задались вопросом: как его лучше форматировать? Единственно-правильного ответа вы не найдете. Более того, любая программа на C++ может быть записана в одну строку, оставаясь при этом корректной.

Правила расстановки фигурных скобок, использования пробелов либо табуляции для отступов, именования переменных — это и многое другое фиксируется в код-стайле. Самые популярные стили:
- [Google,](https://google.github.io/styleguide/cppguide.html)
- [Microsoft,](https://learn.microsoft.com/en-us/windows/win32/stg/coding-style-conventions)
- [LLVM,](https://llvm.org/docs/CodingStandards.html)
- [Chromium,](https://www.chromium.org/developers/coding-style/)
- [Mozilla,](https://firefox-source-docs.mozilla.org/code-quality/coding-style/coding_style_cpp.html)
- [WebKit,](https://webkit.org/code-style-guidelines/)
- [GNU.](https://gcc.gnu.org/wiki/CppConventions)

Каждый из этих семи стилей известен форматтеру кода `clang-foramt`:

```shell
clang-format -i --style=LLVM main.cpp
```

Придерживаться единого код-стайла во всем проекте полезно: одинаково оформленный код проще читать.

У функции может быть сколько угодно определений, но только одно объявление. {.task_text}

Введите `y`, если согласны, и `n`, если нет. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0020_task_0060}
```
Вспомните, что такое ODR. {.task_hint}
```cpp {.task_answer}
n
```

Определение также является и объявлением. {.task_text}

Введите `y`, если согласны, и `n`, если нет. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0020_task_0070}
```
И объявление, и определение объявляют (делают видимой) сущность для компилятора. {.task_hint}
```cpp {.task_answer}
y
```

## Домашнее задание

1. Установите форматтер кода.
2. Сохраните локально код из 1-ой задачи.
3. Отформатируйте файл, применив один из распространенных код-стайлов.

----------

## Резюме

- Сигнатура функции состоит из типа возвращаемого значения, имени функции, типов параметров.
- Объявление — это только сигнатура функции без реализации. Оно делает функцию видимой для компилятора.
- Определение — это реализация функции: ее сигнатура и тело. Определение также является объявлением.
- Согласно правилу одного определения (ODR) у функции может быть только одно определение.
- Если функция ничего не возвращает, то возвращаемый тип — `void`.
- Функция `main()` — точка входа в программу.
