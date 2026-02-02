# Глава 9.2. Ограничение времени жизни и области видимости

В этой главе мы разберем, могут ли в программе сосуществовать переменные с одинаковыми именами. А также какие есть приемы для ограничения времени жизни переменной.

## Затенение имён {#block-variable-shadowing}

Стандарт C++ позволяет заводить во вложенных областях видимости переменные с одинаковыми именами. При обращении к такой переменной происходит [затенение](https://en.wikipedia.org/wiki/Variable_shadowing) (перекрытие) имён: приоритет отдаётся переменной вложенного блока. 

Выбрать глобальную переменную можно, написав перед ней оператор разрешения имён `::` без указания имени пространства имён. Это означает обращение к глобальному пространству имён. Это безымянное пространство имён, представляющее собой глобальную область видимости. {#block-global-namespace}

В этом примере есть 3 переменные с именем `max_speed`: глобальная, локальная для функции `main()` и локальная во вложенном блоке.

```cpp {.example_for_playground}
import std;

double max_speed = 60.0;

int main()
{
    double max_speed = 90.0;

    {
        double max_speed = 120.0;
        std::println("{} {}", max_speed, ::max_speed);
    }

    std::println("{} {}", max_speed, ::max_speed);
}
```
```
120 60
90 60
```

Затенение имён приводит к массе непреднамеренных ошибок. Старайтесь его избегать.

## Вложенные блоки кода и RAII

Как вы [помните,](/courses/cpp/chapters/cpp_chapter_0050/#block-raii) идиома RAII применяется для автоматического управления ресурсами, требующими парных действий. Например, для открытия и закрытия сетевого соединения. Чтобы реализовать RAII-класс, в его конструкторе описывают захват ресурса, а в деструкторе — освобождение. И в некоторых случаях освободить ресурс требуется ещё до выхода из функции, заранее.

В C++ распространена практика создания вложенного блока кода специально для контроля времени жизни переменной. Это выглядит как пара фигурных скобок, не относящихся к функции или управляющей конструкции:

```cpp
int main()
{
    // ...

    {
        int n = 1'000;
        // ...

    }   // Здесь n разрушается

    // ...
}
```

Рассмотрим типичный сценарий использования такого подхода. Он возникает при параллельной работе с переменной из нескольких потоков. Доступ к ней синхронизируется [мьютексом](https://ru.wikipedia.org/wiki/%D0%9C%D1%8C%D1%8E%D1%82%D0%B5%D0%BA%D1%81) `std::mutex`. Он гарантирует, что в каждый момент переменную читает или записывает максимум один поток. Но каждый раз вручную захватывать мьютекс _до обращения_ к переменной и отпускать его _после этого_ неудобно. Поэтому пользуются RAII-классом [std::unique_lock](https://en.cppreference.com/w/cpp/thread/unique_lock.html). В конструкторе он захватывает мьютекс, а в деструкторе отпускает. Для контроля времени жизни объекта `std::unique_lock` используется вложенный блок.

Допустим, у нас есть класс `TaskRunner`, объекты которого из нескольких потоков работают с очередью задач `task_queue`. Она защищена мьютексом `task_queue_mutex`. Тогда метод для выполнения задачи мог бы выглядеть так:

```cpp
void TaskRunner::exec_task()
{
    Task task;

    {
        std::unique_lock<std::mutex> lock(task_queue_mutex); // блокируем мьютекс
        if (task_queue.empty())
            return;
        task = task_queue.pop();
    }                                                        // разблокируем

    exec(task);
}
```

Перед вами код для замера времени выполнения алгоритма `std::sort()`. Воспользуйтесь им, чтобы написать RAII-класс `MeasureTime`. В конструкторе он сохраняет текущее время. А в деструкторе выводит в консоль разницу между настоящим моментом и сохранённым. Чтобы увидеть пример использования класса, откройте задачу в песочнице. {.task_text}

```cpp
std::vector<int> numbers = random_vector(1e6);

// https://en.cppreference.com/w/cpp/chrono/high_resolution_clock/now
auto start = std::chrono::high_resolution_clock::now();

std::sort(numbers);

auto finish = std::chrono::high_resolution_clock::now();

auto delta = std::chrono::duration_cast<std::chrono::milliseconds>
             (finish-start).count();

std::println("Duration: {} ms", delta);
```

```cpp {.task_source #cpp_chapter_0092_task_0040}
class MeasureTime
{

};
```
В приватной секции класса заведите поле типа `std::chrono::time_point<std::chrono::high_resolution_clock>`. В конструкторе присвойте этому полю результат вызова `std::chrono::high_resolution_clock::now()`. В деструкторе заведите переменную, равную текущему значению времени. {.task_hint}
```cpp {.task_answer}
class MeasureTime
{
public:
    MeasureTime()
    {
        start = std::chrono::high_resolution_clock::now();
    }

    ~MeasureTime()
    {
        auto finish = std::chrono::high_resolution_clock::now();

        auto delta = std::chrono::duration_cast<std::chrono::milliseconds>
             (finish-start).count();

        std::println("Duration: {} ms", delta);
    }

private:
    std::chrono::time_point<std::chrono::high_resolution_clock> start;
};
```

## Инициализаторы в if и switch

Инициализатор нужен, чтобы создать переменную и присвоить ей значение до выражения внутри круглых скобок управляющей конструкции. Вы уже [работали](/courses/cpp/chapters/cpp_chapter_0040/#block-for-explanation) с инициализатором в цикле `for`: 

```cpp
//   инициализатор
//   |--------|
for (int i = 0; i < n; ++i) { /* ... */ }
```

А в C++17 появилась возможность задавать инициализаторы в управляющих конструкциях `if` и `switch`: 

```cpp
if (init-statement; condition)  { /* ... */ }

switch (init-statement; condition)  { /* ... */ }
```

Какую проблему решают инициализаторы? Разберем это на примере кода, который обрабатывает введённую пользователем команду: 

```cpp   {.example_for_playground .example_for_playground_003}
std::string cmd = read_user_input();

if (cmd == "q")
{
    std::println("Quitting...");
}
else
{
    std::println("Handling command {}", cmd);
    run_command(cmd);
}

// ...
```

У этого кода есть недостаток: область видимости переменной `cmd` больше, чем требуется. Переменная нужна только для условия, но объявлена _до_ него и доступна _после_ него.

Перепишем пример выше с использованием инициализатора:

```cpp    {.example_for_playground .example_for_playground_004}
if (std::string cmd = read_user_input(); cmd == "q")
{
    std::println("Quitting...");
}
else
{
    std::println("Handling command {}", cmd);
    run_command(cmd);
}

// ...
```

Теперь область видимости и время жизни `cmd` ограничены условием, в котором эта переменная используется. Вне его она недоступна.

Инициализаторы в управляющих конструкциях — всего лишь синтаксический сахар над созданием новой области видимости с помощью вложенного блока:

```cpp  {.example_for_playground .example_for_playground_005}
// ...

{
    std::string cmd = read_user_input();

    if (cmd == "q")
    {
        std::println("Quitting...");
    }
    else
    {
        std::println("Handling command {}", cmd);
        run_command(cmd);
    }
}

// ...
```

В заголовках инструкций `if` и `while` можно использовать инициализацию, совмещённую с проверкой, если инициализируемая переменная приводится к `bool.` Вот пара примеров:

```cpp
if (int return_code = command())
{
    log_error(return_code);
}
```

```cpp
while (int len = read_chunk())
{
    process_chunk();
    std::println("{} bytes read", len);
}
```

[Старайтесь минимизировать](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#res-scope) область видимости переменных: это делает код более надёжным и лаконичным. [Используйте инициализаторы](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#es6-declare-names-in-for-statement-initializers-and-conditions-to-limit-scope) там, где они вам в этом помогут.

Разумеется, инициализаторы опциональны: до этого момента вы использовали `if` и `switch` без них.

Перед вами функция `add_alias()` для добавления псевдонима консольной команды и функция `get_description()`, по команде или псевдониму возвращающая описание. Функции плохо спроектированы: они завязаны на глобальные переменные. Кроме того, в коде допущена ошибка, приводящая к накапливанию пустых описаний команд. {.task_text}

Перенесите логику из этого кода в класс `Commands`. Метод `get_description()` должен кидать исключение `std::out_of_range`, если команда не найдена. В условии `if` используйте инициализатор. {.task_text}

```cpp
std::unordered_map<std::string, std::string> cmd_aliases = 
{
    {"rd", "rmdir"},
    {"o", "less"}
};

std::unordered_map<std::string, std::string> cmd_descriptions =
{
    {"rmdir", "remove empty directories"},
    {"less", "display the contents of a file"},
    {"sed", "stream editor for transforming text"}
};

void add_alias(std::string alias, std::string cmd)
{
    cmd_aliases[alias] = cmd;
}

std::string get_description(std::string cmd)
{
    auto it = cmd_aliases.find(cmd);

    if (it != cmd_aliases.end())
        return cmd_descriptions[it->second];

    return cmd_descriptions[cmd];
}
```

```cpp {.task_source #cpp_chapter_0092_task_0050}
class Commands
{
public:
    void add_alias(std::string alias, std::string cmd)
    {

    }

    void add_command(std::string cmd, std::string description)
    {

    }

    std::string get_description(std::string cmd)
    {

    }

private:

};
```
Вы можете освежить в памяти [варианты вставки](/courses/cpp/chapters/cpp_chapter_0073/#block-insert) элементов в ассоциативный контейнер. {.task_hint}
```cpp {.task_answer}
class Commands
{
public:
    void add_alias(std::string alias, std::string cmd)
    {
        cmd_aliases[alias] = cmd;
    }

    void add_command(std::string cmd, std::string description)
    {
        cmd_descriptions.emplace(cmd, description);
    }

    std::string get_description(std::string cmd)
    {
        if (auto it = cmd_aliases.find(cmd); it != cmd_aliases.end())
            return cmd_descriptions.at(it->second);

        return cmd_descriptions.at(cmd);
    }

private:
    std::unordered_map<std::string, std::string> cmd_aliases;
    std::unordered_map<std::string, std::string> cmd_descriptions;
};
```

----------

## Резюме

- Время жизни переменной бывает автоматическим, статическим, динамическим и локальным для потока.
- Статическое время жизни бывает у глобальных и статических переменных.
- Автоматическое время жизни бывает у локальных переменных и значений параметров функций.
- Временем жизни автоматических переменных управляет компилятор. Он вызывает деструктор, когда объект покидает свою область видимости. За счёт этого в C++ работают RAII-классы.
- Статической можно сделать и локальную, и глобальную переменную.
- Блок кода создаёт область видимости.
- Для ограничения области видимости в конструкциях `for`, `if` и `switch` используется опциональный инициализатор, идущий до `;`.
- Для той же цели в условии управляющих конструкций, в том числе `while`, можно просто объявить и проинициализировать переменную, и она неявно приведётся к `bool`.
- При затенении имён приоритет отдаётся локальной переменной.
- Избегайте в своём коде глобальных переменных и затенения имён.
- Для доступа к глобальной области видимости используется оператор `::`.
