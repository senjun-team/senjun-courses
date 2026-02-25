# Глава 3.2. Конструкция switch-case

Конструкция `switch-case` удобна, когда требуется сравнивать выражение с набором константных значений. Это [более читабельная](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#res-switch-if) замена вложенных `if-else`:

```cpp
switch (выражение)
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
- либо перечислением `enum`, о котором вы скоро [узнаете.](/courses/cpp/chapters/cpp_chapter_0054/)

То есть сравнивать строку `std::string` с использованием `switch` не получится.

## Как выглядит switch-case

Так выглядит `switch-case` для сопоставления символа со значениями:

```cpp  {.example_for_playground .example_for_playground_006}
char user_input = 'y';

switch (user_input)
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

Тело `switch` **обязательно** обрамляется фигурными скобками. Вокруг инструкций в блоках `case` и `default` скобки можно не ставить.

Блок `default` опциональный. Он срабатывает, если не подошёл ни один из блоков `case`.

### Оператор break

В большинстве случаев после выполнения `case` требуется выйти из `switch`. Для этого используется оператор `break`. Если его нет, то выполнение **продолжится** до следующего `break` или до самого конца `switch`:

```cpp   {.example_for_playground .example_for_playground_007}
const std::size_t number_system = 10;

switch (number_system)
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

```cpp {.example_for_playground .example_for_playground_008}
const std::size_t mark = 3;

switch (mark)
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

```consoleoutput {.task_source #cpp_chapter_0032_task_0040}
```
Условие попадает под `case 3`, поэтому выполнится его блок. Так как в блоке отсутствует `break`, выполнятся следующие блоки `case`. {.task_hint}
```cpp {.task_answer}
cba
```

Бывают и случаи, когда `break` _специально_ не ставится. Если несколько разных значений нужно обработать одинаково, то соответствующие им блоки `case` идут рядом. Первые остаются пустыми, а в последний размещается необходимая обработка.

Что выведется в консоль? {.task_text}

```cpp  {.example_for_playground .example_for_playground_009}
const std::size_t mark = 1;

switch (mark)
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

```consoleoutput {.task_source #cpp_chapter_0032_task_0050}
```
Условие попадает под `case 1`. Это пустой блок, в котором нет `break`. Поэтому выполнится следующий блок `case `, а затем и `case 3`. В `case 3` есть `break`, прерывающий `switch`.  {.task_hint}
```cpp {.task_answer}
bad
```

Перепишите эту функцию с применением `switch-case`. {.task_text}

```cpp {.task_source #cpp_chapter_0032_task_0060}
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
```cpp {.task_answer}
void log_state(int code)
{
    std::string state = "";

    switch (code)
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

- Cопоставление выражения с набором значений через `switch-case` — это удобная замена вложенным `if-else`.
- Выражение для `switch` должно быть либо целочисленным, либо символьного типа, либо перечислением `enum`.
- Внутри блоков `switch-case` не забывайте ставить оператор `break`.
- В `switch-case` есть опциональный блок `default`. Он срабатывает, если не подошёл ни один из блоков `case`.
