# Глава 10.1. Объявления и определения

Проект на C++ может быть сколь угодно сложным и содержащим произвольную иерархию директорий. Он почти всегда включает специфичные для системы сборки скрипты, конфиги и другие вспомогательные файлы. Но нас интересуют только файлы с кодом на C++. Они содержат объявления и определения. Мы разберем, что это такое, а потом обсудим, как организуются проекты без модулей и с модулями.

## Видимость объявлений для компилятора

Функции, классы, переменные и другие сущности в программе становятся видны компилятору после точки своего объявления. Если использовать их до места, где они объявлены, то компилятор выдаст ошибку.

Убедимся в этом на примере, в котором есть [взаимная рекурсия.](https://ru.wikipedia.org/wiki/%D0%92%D0%B7%D0%B0%D0%B8%D0%BC%D0%BD%D0%B0%D1%8F_%D1%80%D0%B5%D0%BA%D1%83%D1%80%D1%81%D0%B8%D1%8F) Пример реализует [гипотезу Коллатца:](https://ru.wikipedia.org/wiki/%D0%93%D0%B8%D0%BF%D0%BE%D1%82%D0%B5%D0%B7%D0%B0_%D0%9A%D0%BE%D0%BB%D0%BB%D0%B0%D1%82%D1%86%D0%B0) какое бы натуральное число `n` мы ни взяли, рано или поздно мы получим единицу, если будем совершать действия:
- Если `n` чётное, делить его на 2.
- Если `n` нечётное, умножать на 3 и прибавлять 1. 

```cpp  {.example_for_playground}
import std;

int collatz_multiply(int x) 
{
    return (x % 2 > 0) ? 3 * x + 1 : collatz_divide(x);
}

int collatz_divide(int x) 
{
    return  (x % 2 == 0) ? x / 2 : collatz_multiply(x);
}

int main()
{
    int n = 17;
    std::println("Checking Collatz conjecture for {}", n);
    
    while (n > 1)
    {
        n = collatz_multiply(n);
        std::print("{} ", n);
    }
}
```
```
main.cpp:5:36: error: use of undeclared identifier 'collatz_divide'
    5 |     return x % 2 > 0 ? 3 * x + 1 : collatz_divide(x);
      |                                    ^
```

Компилятор не нашёл функцию, вызванную до своего объявления. Поправить это переносом функции выше места её вызова не получится. Ведь `collatz_multiply()` вызывает `collatz_divide()` и наоборот! Кроме того, код может быть достаточно сложным, чтобы подходящего места для размещения всех функций _до_ их вызовов просто бы не нашлось. Как же решить эту проблему?

## Что такое объявление и определение {#block-declaration-definition}

Все функции, классы и другие сущности, с которыми вы работали в прошлых главах, это не просто объявления, а заодно и _определения._ Единственное исключение — объявление функции `read_file()` в файле `main.cpp` [предыдущей практики](/courses/cpp/practice/cpp_brainfuck_interpreter/) «Интерпретатор Brainfuck».

[Определение](https://en.cppreference.com/w/cpp/language/definition.html) (definition) — это _объявление_ вместе с информацией, которой достаточно для использования сущности в коде. Определение функции включает и её тело, то есть реализацию. Любое определение также является и объявлением. {#block-definitions}

Как же выглядят объявления, которые не считаются определениями?

[Объявление](https://en.cppreference.com/w/cpp/language/declarations.html) (declaration) делает сущность видимой для компилятора. Объявление функции состоит из возвращаемого типа, имени функции и параметров. После объявления ставится символ `;`. {#block-declarations}

```cpp
int collatz_multiply(int x);
```

Чтобы исправить пример кода с гипотезой Коллатца, разместим объявления функций до их использования:

```cpp  {.example_for_playground}
import std;

int collatz_multiply(int x); // объявление

int collatz_divide(int x);   // объявление

int main()
{
    int n = 17;
    std::println("Checking Collatz conjecture for {}", n);
    
    while (n > 1)
    {
        n = collatz_multiply(n);
        std::print("{} ", n);
    }
}

int collatz_multiply(int x)  // определение
{
    return (x % 2 > 0) ? 3 * x + 1 : collatz_divide(x);
}

int collatz_divide(int x)   // определение
{
    return  (x % 2 == 0) ? x / 2 : collatz_multiply(x);
}
```
```
Checking Collatz conjecture for 17
52 26 13 40 20 10 5 16 8 4 2 1 
```

## Правило одного определения

Объявлений одной и той же сущности в программе может быть сколь угодно много. Но определение должно быть единственным. Так гласит важный пункт из [правила одного определения](https://en.cppreference.com/w/cpp/language/definition) (ODR, one definition rule). Нарушение ODR приведёт к ошибке компиляции. {#block-odr}

Что выведет этот код? {.task_text}

В случае ошибки напишите `err`. {.task_text}

```cpp {.example_for_playground}
import std;

int main()
{
    std::println("{}", to_miles(0.0));
}

double to_miles(double km)
{
    return km * 0.62;
}
```

```consoleoutput {.task_source #cpp_chapter_0101_task_0010}
```
Функция `to_miles()` объявлена после её вызова. {.task_hint}
```cpp {.task_answer}
err
```

## Объявление и определение класса

Объявление класса не включает реализацию его методов:

```cpp {.example_for_playground .example_for_playground_001}
class Message
{
public:
    Message(std::string raw_text);

    std::string get_message();
    std::time_t get_timestamp();

private:
    std::string msg;
    std::time_t ts;
};
```

Объявление класса в связке с реализацией методов считается определением класса. При реализации метода _вне_ тела класса перед методом указывается имя класса, отделённое от метода оператором разрешения области видимости `::`.

```cpp  {.example_for_playground .example_for_playground_002}
Message::Message(std::string raw_text)
{
    msg = parse_message(raw_text);
    ts = parse_time(raw_text);
}

std::string Message::get_message()
{
    return msg;
}

std::time_t Message::get_timestamp()
{
    return ts;
}
```

Приемная комиссия колледжа принимает заявления на поступление вместе с результатами теста. Члены комиссии должны отслеживать `n`-ый наивысший балл по тестам. Это нужно делать в потоковом режиме, по мере того, как поступают новые заявления. Отслеживание `n`-ного наивысшего балла помогает определить пороговое значение, необходимое для зачисления `n` учеников. {.task_text}

Перед вами объявление класса `NthLargest`. Реализуйте определения методов. Они должны идти после объявления класса. {.task_text}

Конструктор принимает число `n` и значение по умолчанию, которое нужно возвращать, пока не накопится `n` результатов теста. {.task_text}

Метод `add()` добавляет новый результат теста и возвращает обновлённый `n`-ый наивысший балл. Пока `n` результатов не накопилось, метод возвращает `default_val`. {.task_text}

В реализации используется [очередь с приоритетами.](/courses/cpp/chapters/cpp_chapter_0075/#block-priority-queue) {.task_text}

```cpp {.task_source #cpp_chapter_0101_task_0020}
class NthLargest
{
public:
    NthLargest(std::size_t n, std::size_t default_val);
    std::size_t add(std::size_t val);

private:
    std::size_t m_n;
    std::size_t m_default;
    // https://en.cppreference.com/w/cpp/container/priority_queue
    std::priority_queue<std::size_t,              // тип элемента
                        std::vector<std::size_t>, // контейнер для адаптера
                        std::greater<std::size_t> // компаратор
                        > m_pq;
};
```
Очередь с приоритетами реализует структуру данных [куча](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) (heap). По умолчанию элемент с наибольшим значением ключа находится на вершине кучи. Чтобы на вершине оказался наименьший элемент, шаблонный класс `std::priority_queue` был инстанцирован компаратором `std::greater` вместо `std::less`. Вам осталось контроллировать количество элементов кучи. Оно не должно превосходить `n`. {.task_hint}
```cpp {.task_answer}
class NthLargest
{
public:
    NthLargest(std::size_t n, std::size_t default_val);
    std::size_t add(std::size_t val);

private:
    std::size_t m_n;
    std::size_t m_default;
    // https://en.cppreference.com/w/cpp/container/priority_queue
    std::priority_queue<std::size_t,              // тип элемента
                        std::vector<std::size_t>, // контейнер для адаптера
                        std::greater<std::size_t> // компаратор
                        > m_pq;
};

NthLargest::NthLargest(std::size_t n, std::size_t default_val)
{
    m_n = n;
    m_default = default_val;
}

std::size_t NthLargest::add(std::size_t val)
{
    if (m_pq.size() < m_n)
    {
        m_pq.push(val);
        return (m_pq.size() < m_n) ? m_default : m_pq.top();
    }
    
    if (std::size_t cur_nth_largest = m_pq.top(); cur_nth_largest < val)
    {
        m_pq.push(val);
        if (m_pq.size() > m_n)
            m_pq.pop();
    }
    
    return m_pq.top();
}
```

----------

## Резюме {#block-summary}

- Объявление (declaration) делает сущность видимой для компилятора, а определение (definition) реализует её.
- Любое определение является объявлением.
- Объявлений одной и той же сущности в проекте может быть несколько. Но определение должно быть единственным. Это важный пункт ODR (one definition rule).
