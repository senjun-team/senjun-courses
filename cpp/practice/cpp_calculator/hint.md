## Подход к выполнению практики

Вычисление алгебраического выражения состоит из трех этапов: токенизация, перевод в постфиксную форму, вычисление выражения в постфиксной форме.

## Представление токенов

Каждый токен можно представить в виде структуры с двумя полями: `tok_type` — тип токена (число, скобка и т.д.); `value` — значение токена:

```cpp
enum class TokenType : std::uint8_t
{
    Number = 0,
    Operator,
    Parenthesis,
    Invalid
};

struct Token
{
    TokenType tok_type = TokenType::Invalid;
    std::string value;
};
```

Тогда алгебраическое выражение представляется в виде вектора токенов `std::vector<Token>`.

Например, строка с выражением `"9.5-(1+3)"` превращается в вектор из 7-ми элементов:

```
{
    {
        .tok_type = TokenType::Number,
        .value = "9.5"
    },
    {
        .tok_type = TokenType::Operator,
        .value = "-"
    },
    {
        .tok_type = TokenType::Parenthesis,
        .value = "("
    },
    {
        .tok_type = TokenType::Number,
        .value = "1"
    },
    {
        .tok_type = TokenType::Operator,
        .value = "+"
    },
    {
        .tok_type = TokenType::Number,
        .value = "3"
    },
    {
        .tok_type = TokenType::Parenthesis,
        .value = ")"
    }
}
```

Для того чтобы по исходному выражению получить вектор токенов, необходимо составить конечный автомат.

## Представление конечного автомата

КА для парсинга выражения на токены можно представить в виде словаря словарей.

Ключи словаря верхнего уровня — обозначения для классов (категорий) символов:

```cpp
enum class SymbolType : std::uint8_t
{
    Digit = 0,
    Point,
    Operator,
    Parenthesis,
    Other
};
```

Значение по данному ключу — тоже словарь.

Ключи в этом вложенном словаре  — обозначения вариантов текущего состояния:

```cpp
enum class State : std::uint8_t
{
    NewToken = 0,
    NumberIntegerPart,
    NumberFractionalPart,
    Error
};
```

А значения — структуры с двумя полями для обозначения нового состояния и функции, которую требуется выполнить при переходе между состояниями. С помощью обертки `std::function` мы можем присвоить функцию полю структуры:

```cpp
struct Transition
{
    State new_state;
    std::function<void(char, Token &, std::vector<Token> &)> action;
};
```

Вот как может выглядеть объявление КА в виде словаря словарей:

```cpp
using Row = std::map<State, Transition>;

// Конечный автомат (finite-state machine) для парсинга строки
// с алгебраическим выражением
const std::map<SymbolType, Row> kFSM = {
    // ...
};
```

А вот пример объявления КА и инициализации его значениями:

```cpp
const std::map<SymbolType, Row> kFSM = {
    // Символ
    { SymbolType::Digit,
        {
            { State::NewToken, // Текущее состояние
            // Новое состояние                       Действие
            { .new_state = State::NumberIntegerPart, .action = start_accumulating_number} },

            { State::NumberIntegerPart,
            { .new_state = State::NumberIntegerPart, .action = accumulate_number} },

            { State::NumberFractionalPart,
            { .new_state = State::NumberFractionalPart, .action = accumulate_number} },
        }
    },
    // ...
};
```

Здесь `start_accumulating_number()` и `accumulate_number()` — функции, вызываемые *при переходе* к накоплению числа и *в процессе* накопления числа. Их тип должен совпадать с типом, переданным в `std::function` в объявлении поля `action`:

```cpp
void(char, Token &, std::vector<Token> &)
```

Например:

```cpp
void start_accumulating_number(char symbol, Token & token, std::vector<Token> & tokens)
{
    // ...
}
```

Допустим, конечный автомат представлен в виде словаря с именем `kFSM`. Тогда переключение между состояниями `state` и `new_state` будет выглядеть так:

```cpp
const Transition transition = kFSM.at(symbol_type).at(state);

if (transition.new_state == State::Error)
    throw std::invalid_argument("Couldn't split expression to tokens");

state = transition.new_state;
transition.action(sybmol, token, tokens);
```

Здесь `symbol_type` — категория обрабатываемого символа. Для ее получения можно завести отдельную функцию, которая по символу возвращает значение перечисления `SymbolType`.

Если описание КА через словарь словарей кажется вам слишком сложным, вы можете воспользоваться альтернативным подходом. Заведите функцию, внутри которой по `switch-case` обрабатывайте каждый символ и меняйте состояние КА.
