# Глава 5.4. Перечисления

Перечисления (enumerations) облегчают работу с данными, которые принимают фиксированное и заранее известное множество значений.

## Перечисления enum {#block-enum}

[Перечисление](https://en.cppreference.com/w/c/language/enum) `enum` — это тип, значения которого ограничены набором именованных констант. Во многих код-стайлах имена констант задаются заглавными буквами:

```cpp   {.example_for_playground .example_for_playground_004}
// Создаем новый тип
enum HashType
{
    MD5,
    SHA1,
    SHA2
};

// Создаем объект этого типа
HashType hash_type = SHA2;
```

Мы завели тип `HashType`. Переменные этого типа принимают одно из 3-х значений: `MD5`, `SHA1`, `SHA2`. Под капотом каждая из этих констант равна какому-то целому числу. За счёт этого компилятор может выполнить неявное преобразование: привести значение типа `HashType` к целочисленному типу.

```cpp  {.example_for_playground .example_for_playground_005}
// Оба варианта обращения к константам корректны:
int k = HashType::MD5;
int m = SHA1;

HashType hash_type = SHA2;
int n = hash_type;

std::println("{} {} {}", k, m, n);
```
```
0 1 2
```

Взгляните ещё раз на то, как выглядит создание перечисления. Создадим новый тип `Compression`, переменные которого могут принимать два значения — `QuickLZ` и `LZ4`:

```cpp   {.example_for_playground .example_for_playground_006}
enum Compression
{
    QuickLZ,
    LZ4,
};
```

После объявления перечисления ставится оператор `;`. Такой синтаксис — очередное наследие Си, используемое и для классов со структурами. Он был введён, чтобы сразу после определения типа создавать его объект.

Так выглядит одновременное создание нового типа `Compression` и переменной этого типа `compression_method`:

```cpp   {.example_for_playground .example_for_playground_007}
enum Compression
{
    QuickLZ,
    LZ4,
} compression_method;

// ...

compression_method = LZ4;
```

В современном C++ коде такая практика практически не встречается.

Значение константы внутри перечисления можно определить явно после оператора `=`. Если этого не сделано, константа равна значению предыдущей, увеличенному на 1. Первая константа по умолчанию равна нулю. {#block-enum-const-values}

Каковы целочисленные значения, принимаемые перечислением `State`? Укажите их через пробел. {.task_text}

```cpp
enum State
{
    Initialized,
    Paused,
    Done = 6,
    Reset
};
```

```consoleoutput {.task_source #cpp_chapter_0054_task_0040}
```
`Initialized` — первая константа, значит, она равна нулю. Значение следующей константы равно значению предыдущей, увеличенному на 1. Значит, `Paused` равна 1. `Done` равна 6. И поэтому `Reset` равна 7. {.task_hint}
```cpp {.task_answer}
0 1 6 7
```

## Перечисления enum class

Как вы только что узнали, у перечислений `enum` есть две особенности:
- Имя константы доступно и без указания имени перечисления. Вместо `State::Reset` допустимо написать просто `Reset`. Это может привести к конфликту имён, если существует другая константа с таким же именем.
- Значения перечислений неявно преобразуются к целочисленным типам и другим перечислениям.

Такое поведение порождает массу ошибок. Чтобы их избежать, в C++11 был введён `enum class`. Он ничем не отличается от обычного `enum` кроме запрета неявных преобразований и безопасной работы с именами констант. [Старайтесь всегда использовать](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#renum-class) `enum class` вместо `enum`.

```cpp   {.example_for_playground .example_for_playground_008}
enum class ByteOrder
{
    Native       = 1,
    LittleEndian = 2,
    BigEndian    = 3
};

ByteOrder bo1 = Native;            // Ошибка
ByteOrder bo2 = ByteOrder::Native; // ОК
int n = ByteOrder::Native;         // Ошибка
```

Заведите `enum class` `Rarity` со значениями `Common`, `Rare`, `Legendary`. {.task_text}

Напишите функцию `to_string()`, которая принимает значение типа `Rarity` и возвращает соответствующую ему строку: `"Common"`, `"Rare"`, `"Legendary"`. {.task_text}

В теле функции используйте `switch-case`. {.task_text}

```cpp {.task_source #cpp_chapter_0054_task_0050}
// Перечисление Rarity

// Функция to_string()
```
После объявления `enum class` не забудьте точку с запятой. Объявление `Rarity` должно идти до объявления функции `to_string()`. Все возвращаемые функцией строки начинаются с заглавной буквы. {.task_hint}
```cpp {.task_answer}
enum class Rarity
{
    Common,
    Rare,
    Legendary
};

std::string to_string(Rarity rarity)
{
    switch(rarity)
    {
        case Rarity::Common:
            return "Common";
        case Rarity::Rare:
            return "Rare";
        case Rarity::Legendary:
            return "Legendary";
    }
}
```

----------

## Резюме

- Перечисление — это тип, значения которого ограничены набором именованных констант.
