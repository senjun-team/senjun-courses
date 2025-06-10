# Глава 8. Обзор алгоритмов

Алгоритмы стандартной библиотеки C++ — это шаблонные функции для поиска, сортировки, слияния и других типовых действий над диапазонами и отдельными значениями.

## Мотивация применять алгоритмы

Умение своевременно использовать алгоритмы отличает опытного C++ разработчика от новичка. Например, так выглядит наивная реализация поиска максимального элемента вектора:

```c++  {.example_for_playground .example_for_playground_001}
std::vector<double> temperatures = read_temperatures();

if (temperatures.empty())
{
    std::println("Waiting for new data...");
}
else
{
    double max = temperatures.front();

    for(double t: temperatures)
    {
        if (max < t)
            max = t;
    }

    std::println("Max temperature: {}", max);
}
```

Перепишем этот пример с использованием алгоритма [std::max_element()](https://en.cppreference.com/w/cpp/algorithm/max_element):

```c++  {.example_for_playground .example_for_playground_002}
std::vector<double> temperatures = read_temperatures();

const auto it_max = std::max_element(temperatures.begin(), temperatures.end());

if (it_max == temperatures.end())
    std::println("Waiting for new data...");
else 
    std::println("Max temperature: {}", *it_max);
```

Этот код делает то же самое, но выглядит короче и понятнее. Похожий пример мы [приводили,](/courses/cpp/chapters/cpp_chapter_0010/#block-zero-overhead) когда описывали одно из достоинств C++ — абстракции с нулевой стоимостью.

Всегда предпочитайте стандартные алгоритмы и их комбинацию наивным самописным решениям. Вы получите преимущества:
- Более выразительный и понятый код. Алгоритмы стандартной библиотеки можно сравнить со словарным запасом, которым владеют разработчики на C++.
- Повышение уровня абстракции без потери производительности.
- Предотвращение типичных проблем: избыточной алгоритмической сложности, ошибок в граничных условиях.

Если вам нужно выполнить над контейнером какое-то типовое действие, то скорее всего оно реализовано в стандартной библиотеке. Просто пройдитесь по [списку алгоритмов](https://en.cppreference.com/w/cpp/algorithm) и выберите подходящий.

## Какие алгоритмы есть в стандартной библиотеке

Стандарт языка включает [сотни алгоритмов.](https://en.cppreference.com/w/cpp/algorithm) В прошлых главах вы уже познакомились с [std::for_each()](/courses/cpp/chapters/cpp_chapter_0010/#block-for-each), [std::clamp()](/courses/cpp/chapters/cpp_chapter_0010/#block-clamp), [std::min(), std::max()](/courses/cpp/chapters/cpp_chapter_0030/#block-min-max), [std::reverse()](/courses/cpp/chapters/cpp_chapter_0060/#block-vector), [std::find(), std::find_if()](/courses/cpp/chapters/cpp_chapter_0060/#block-find-if) и [std::sort()](/courses/cpp/chapters/cpp_chapter_0070/#block-sort).

Перечислим самые популярные классы задач, решаемые с помощью алгоритмов. Для каждого из них приведем в пример по паре функций.
- Изменение элементов диапазона.
    - [std::for_each()](https://en.cppreference.com/w/cpp/algorithm/for_each), [std::replace](https://en.cppreference.com/w/cpp/algorithm/replace).
- Поиск и подсчет.
    - [std::find()](https://en.cppreference.com/w/cpp/algorithm/find), [std::count()](https://en.cppreference.com/w/cpp/algorithm/count).
- Бинарный поиск в отсортированном диапазоне.
    - [std::lower_bound()](https://en.cppreference.com/w/cpp/algorithm/lower_bound), [std::equal_range()](https://en.cppreference.com/w/cpp/algorithm/equal_range).
- Копирование.
    - [std::copy()](https://en.cppreference.com/w/cpp/algorithm/copy), [std::copy_backward](https://en.cppreference.com/w/cpp/algorithm/copy_backward).
- Присваивание элементам значений по некоторым правилам.
    - [std::fill()](https://en.cppreference.com/w/cpp/algorithm/fill), [std::generate()](https://en.cppreference.com/w/cpp/algorithm/generate).
- Удаление элементов.
    - [std::remove()](https://en.cppreference.com/w/cpp/algorithm/remove), [std::unique()](https://en.cppreference.com/w/cpp/algorithm/unique). Оба алгоритма используются в связке с методом контейнера `erase()` или функцией [std::erase()](https://en.cppreference.com/w/cpp/container/vector/erase2).
- Изменение порядка элементов.
    - [std::reverse()](https://en.cppreference.com/w/cpp/algorithm/reverse), [std::shuffle()](https://en.cppreference.com/w/cpp/algorithm/random_shuffle).
- Сортировка.
    - [std::sort()](https://en.cppreference.com/w/cpp/algorithm/sort), [std::nth_element()](https://en.cppreference.com/w/cpp/algorithm/nth_element).
- Операции над множествами.
    - [std::includes()](https://en.cppreference.com/w/cpp/algorithm/includes), [std::set_intersection()](https://en.cppreference.com/w/cpp/algorithm/set_intersection).
- Слияние отсортированных диапазонов.
    - [std::merge()](https://en.cppreference.com/w/cpp/algorithm/merge), [std::inplace_merge()](https://en.cppreference.com/w/cpp/algorithm/inplace_merge).
- Операции над структурой данных [куча](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) (heap).
    - [std::push_heap()](https://en.cppreference.com/w/cpp/algorithm/push_heap), [std::make_heap()](https://en.cppreference.com/w/cpp/algorithm/make_heap).
- Расчет максимума и минимума.
    - [std::minmax()](https://en.cppreference.com/w/cpp/algorithm/minmax), [std::clamp()](https://en.cppreference.com/w/cpp/algorithm/clamp).
- Получение из диапазона некоего значения. Например, суммы элементов.
    - [std::accumulate()](https://en.cppreference.com/w/cpp/algorithm/accumulate), [std::reduce()](https://en.cppreference.com/w/cpp/algorithm/reduce).
- Все остальное.

Алгоритмы имеют перегрузки, принимающие разный набор параметров. Например, одним из параметров может выступать [политика выполнения](https://en.cppreference.com/w/cpp/algorithm/execution_policy_tag_t) (execution policy). Она появилась в C++17 и позволяет распараллеливать работу алгоритма. Это открывает возможности многопоточности и векторизации без единой дополнительной строки кода. С ней вы поработаете в следующих главах.

Многие алгоритмы принимают итераторы на диапазон внутри контейнера. Диапазон включает итератор на начало и _не_ включает итератор на конец:

```c++ {.example_for_playground .example_for_playground_003}
std::vector<int> vals = {1, 2, 3, 4, 5, 6};

// Переворачиваем последовательность элементов
// с индексами 1, 2.
std::reverse(vals.begin() + 1, vals.begin() + 3);

std::println("{}", vals);
```
```
[1, 3, 2, 4, 5, 6]
```

В этой главе мы рассмотрим только алгоритмы, которые пригодятся вам в первую очередь.

## Поиск

Алгоритм `std::find()` нужен для поиска элемента в коллекции. Как и некоторые другие алгоритмы, он обладает вариациями с суффиксами `_if` и `_if_not` в имени. Рассмотрим, чем они отличаются.

Функция [std::find()](https://en.cppreference.com/w/cpp/algorithm/find) принимает итераторы на диапазон и искомое значение. И возвращает итератор на первый элемент диапазона, равный значению.

В этом примере мы получаем итератор на искомый элемент, чтобы заменить его значение:

```c++ {.example_for_playground .example_for_playground_004}
std::array<int, 5> raw_data = {9, -1, 2, 2, 3};

auto it = std::find(raw_data.begin(), raw_data.end(), -1);

if (it != raw_data.end())
    *it = 0;

std::println("{}", raw_data);
```
```
[9, 0, 2, 2, 3]
```

Чему равна переменная `i`? {.task_text}

С функцией `std::distance()` вы [познакомились](/courses/cpp/chapters/cpp_chapter_0060/#block-distance) в главе про итераторы. {.task_text}

```c++ {.example_for_playground}
import std;

int main()
{
    std::vector<int> ids = {568, 91, 55, 91, 104};

    auto rit = std::find(ids.rbegin(), ids.rend(), 91);

    std::size_t i =  rit == ids.rend() ?
                            0 : std::distance(ids.begin(), (rit + 1).base());
}
```

```consoleoutput {.task_source #cpp_chapter_0080_task_0010}
```
В алгоритм `std::find()` передаются обратные итераторы, значит поиск начинается с конца. Функция `std::find()` возвращает обратный итератор `rit`, и выражение `(rit + 1).base()` позволяет получить из обратного итератора обычный. Затем расчитывается расстояние между первым элементом вектора и элементом, на который указывает итератор. {.task_hint}
```cpp {.task_answer}
3
```

Функция [std::find_if()](https://en.cppreference.com/w/cpp/algorithm/find) вместо значения принимает [предикат](/courses/cpp/chapters/cpp_chapter_0050/#block-predicate) и применяет его к элементам диапазона. И возвращает итератор на первый элемент, для которого предикат вернул `true`.

Функция [std::find_if_not()](https://en.cppreference.com/w/cpp/algorithm/find) имеет единственное отличие от `std::find_if()`. Она возвращает итератор на элемент, для которого предикат вернул `false`.

Реализуйте функцию `accepts_gzip()`. Она принимает вектор из HTTP-заголовков. Каждый заголовок — это пара, в которой поле `first` хранит имя заголовка, а поле `second` — его значение. Функция должна вернуть `true`, если среди заголовков содержится `"Accept-Encoding"`, значение которого содержит подстроку `"gzip"`. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0020}
bool accepts_gzip(std::vector<std::pair<std::string, std::string>> headers)
{

}
```
Вам потребуется завести вспомогательную функцию-предикат, которая сравнивает поле `first` объекта `std::pair` с нужным заголовком. И в случае успеха ищет в поле `second` подстроку `"gzip"`. Вам пригодится метод строки `find()`, с которым вы [уже работали.](/courses/cpp/chapters/cpp_chapter_0030/#block-string-find) {.task_hint}
```c++ {.task_answer}
bool accepts(std::pair<std::string, std::string> header)
{
    if (header.first != "Accept-Encoding")
        return false;

    return (header.second.find("gzip") != std::string::npos);
}

bool accepts_gzip(std::vector<std::pair<std::string, std::string>> headers)
{
    return std::find_if(headers.begin(), headers.end(), accepts) 
           != headers.end();
}
```

Чтобы искать элементы начиная с конца диапазона, в алгоритм поиска передаются [обратные итераторы:](/courses/cpp/chapters/cpp_chapter_0060/#block-reverse-iterators)

```c++ {.example_for_playground .example_for_playground_005}
std::deque<int> d = {5, 6, 10, 10, 1};

auto rit = std::find(d.rbegin(), d.rend(), 10);
std::println("{}", std::distance(d.begin(), (rit + 1).base()));
```
```
3
```

Функция `std::find()` и ее вариации нужны для поиска одного элемента. Они работают за линейное время `O(N)`, где `N` — длина диапазона.

Для поиска одного диапазона внутри другого используется алгоритм [std::search()](https://en.cppreference.com/w/cpp/algorithm/search). Он работает за время `O(N * M)`, где `N` и `M` — длины диапазонов.

Для определения, соответствуют ли элементы диапазона предикату, есть алгоритмы [std::all_of(), std::any_of() и std::none_of()](https://en.cppreference.com/w/cpp/algorithm/all_any_none_of). Их алгоритмическая сложность — `O(N)`.

## Бинарный поиск

Если элементы диапазона отсортированы, то поиск по ним можно выполнить быстрее, чем за `O(N)`. Для этого вместо полного перебора применяется [бинарный поиск.](https://ru.wikipedia.org/wiki/%D0%94%D0%B2%D0%BE%D0%B8%D1%87%D0%BD%D1%8B%D0%B9_%D0%BF%D0%BE%D0%B8%D1%81%D0%BA)

Бинарный поиск выглядит предельно просто. Допустим, мы ищем в отсортированном диапазоне элемент, равный `X`.
1. Сравниваем с `X` элемент в середине диапазона.
2. Если его значение меньше `X`, то продолжаем поиск в первой половине диапазона. Иначе — во второй.
3. Сравниваем с `X` элемент в середине выбранной половины.
4. Выполняем шаги 2 и 3, пока не найдем `X`, либо пока не получим пустой диапазон. Это будет означать отсутствие искомого элемента.

На каждой итерации бинарного поиска рассматриваемый диапазон сокращается в 2 раза. Значит, алгоритм работает за `O(log(N))`.

Так выглядит бинарный поиск на примере небольшого диапазона:

![Бинарный поиск](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-8/illustrations/cpp/binary_search.jpg) {.illustration}

Понять алгоритм бинарного поиска просто, но совершить ошибку в его реализации — еще проще. Чаще всего ошибаются при определении середины и границ диапазона. Это приводит к вечным циклам, пропуску элементов и выходу за границы диапазона. Поэтому реализация бинарного поиска — задача, которая была и остается одной из популярнейших задач на собеседованиях.

Напишите шаблонную функцию `b_search()`, которая принимает итераторы на отсортированный диапазон: `first` — итератор на первый элемент, `last` — итератор, указывающий за последний элемент. Также функция принимает искомое значение `x`. Для простоты будем считать, что `x` встречается в диапазоне не более 1 раза. {.task_text}

Функция должна вернуть итератор на элемент, равный `x`, либо итератор `last`, если элемент не обнаружен. {.task_text}

Например, для диапазона `[2, 5, 8, 9, 11, 12, 16]` и значения `x=11` функция должна вернуть итератор на 4-ый элемент, считая с нуля. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0030}
template<class It, class Val>
It b_search(It first, It last, Val x)
{

}
```
Определите длину диапазона через `std::distance()`. Середина диапазона находится на половине длины от его начала. {.task_hint}
```c++ {.task_answer}
template<class It, class Val>
It b_search(It first, It last, Val x)
{
    std::size_t len = std::distance(first, last);

    while (len > 0)
	{
        const std::size_t half = len / 2;
        It middle = first + half;
        
        if (*middle < x)
        {
            first = middle;
            ++first;
            len -= half - 1;
        }
        else
        {
            len = half;
        }
	}
    
    return first;
}
```

В стандартной библиотеке есть алгоритм [std::binary_search()](https://en.cppreference.com/w/cpp/algorithm/binary_search). Но он лишь проверяет, что диапазон содержит искомый элемент. Эта функция возвращает `true` либо `false`:

```c++ {.example_for_playground .example_for_playground_006}
std::string s = "abcdefg";
std::println("{}", std::binary_search(s.begin() + 2, s.end() - 1, 'e'));
```
```
true
```

Зато алгоритмы [std::lower_bound()](https://en.cppreference.com/w/cpp/algorithm/lower_bound) и [std::upper_bound()](https://en.cppreference.com/w/cpp/algorithm/upper_bound) возвращают итератор. Оба принимают итераторы на упорядоченный диапазон и искомое значение:
- `std::lower_bound()` возвращает итератор на первый элемент диапазона, который _не меньше,_ чем значение.
- `std::upper_bound()` возвращает итератор на первый элемент диапазона, который `_больше_, чем значение.

Если искомый элемент присутствует в диапазоне, функции возвращают разный результат:

```c++ {.example_for_playground .example_for_playground_007}
std::vector<int> v = {3, 5, 7, 8, 8, 8, 10};
auto it_l = std::lower_bound(v.begin(), v.end(), 8);
std::println("lower bound. index = {}. value = {}",
             std::distance(v.begin(), it_l), *it_l);

auto it_u = std::upper_bound(v.begin(), v.end(), 8);
std::println("upper bound. index = {}. value = {}",
             std::distance(v.begin(), it_u), *it_u);
```
```
lower bound. index = 3. value = 8
upper bound. index = 6. value = 10
```

А при поиске отсутствующего числа эти функции возвращают одинаковый результат:

```c++  {.example_for_playground .example_for_playground_008}
std::vector<int> v = {2, 5, 9, 11};
auto it_l = std::lower_bound(v.begin(), v.end(), 6);
std::println("lower bound. index = {}. value = {}",
             std::distance(v.begin(), it_l), *it_l);

auto it_u = std::upper_bound(v.begin(), v.end(), 6);
std::println("upper bound. index = {}. value = {}",
             std::distance(v.begin(), it_u), *it_u);
```
```
lower bound. index = 2. value = 9
upper bound. index = 2. value = 9
```

Иными словами, итераторы от `std::lower_bound()` и `std::upper_bound()` _ограничивают_ диапазон элементов, равных искомому значению. Чтобы получить этот диапазон, можно воспользоваться всего одной функцией. Она называется [std::equal_range()](https://en.cppreference.com/w/cpp/algorithm/equal_range) и возвращает пару итераторов.

Напишите функцию `count_vals()`. Она принимает отсортированный вектор и искомое значение. Функция должна вернуть, сколько раз искомое значение встречается в векторе. {.task_text}

Например, для вектора `[6, 6, 8, 8, 9]` и значения 8 функция должна вернуть 2. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0040}
std::size_t count_vals(std::vector<int> data, int val)
{

}
```
Для получения диапазона элементов, равных `val`, воспользуйтесь [std::equal_range()](https://en.cppreference.com/w/cpp/algorithm/equal_range). Функция возвращает пару итераторов. Верните расстояние между ними, вызвав [std::distance()](https://en.cppreference.com/w/cpp/iterator/distance). {.task_hint}
```c++ {.task_answer}
std::size_t count_vals(std::vector<int> data, int val)
{
    const auto it_pair = std::equal_range(data.begin(), data.end(), val);
    return std::distance(it_pair.first, it_pair.second);
}
```

## Сортировка

Функция [std::is_sorted()](https://en.cppreference.com/w/cpp/algorithm/is_sorted) проверяет, отсортирован ли диапазон. А [std::sort()](https://en.cppreference.com/w/cpp/algorithm/sort) сортирует его. Обе функции работают с сортировкой по возрастанию. Для строк это называется [лексикографическим порядком.](https://ru.wikipedia.org/wiki/%D0%9B%D0%B5%D0%BA%D1%81%D0%B8%D0%BA%D0%BE%D0%B3%D1%80%D0%B0%D1%84%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D0%B9_%D0%BF%D0%BE%D1%80%D1%8F%D0%B4%D0%BE%D0%BA) Для сортировки по возрастанию используется сравнение через оператор `<`.

```c++   {.example_for_playground .example_for_playground_009}
std::deque<double> d = {3.1, 8.0, 56.5};
std::println("{}", std::is_sorted(d.begin(), d.end()));

d.push_front(100.0);
std::println("{}", std::is_sorted(d.begin(), d.end()));

std::sort(d.begin(), d.end());
std::println("{}", d);
```
```
true
false
[3.1, 8, 56.5, 100]
```

Для сортировки в другом порядке у алгоритмов есть перегрузки, принимающие _компаратор._ Это функция-предикат от двух параметров. Она возвращает `true`, если первый параметр меньше, чем второй. Чтобы сортировать по убыванию, а не по возрастанию, достаточно передать [std::greater<T>()](https://en.cppreference.com/w/cpp/utility/functional/greater), где `T` — тип элементов диапазона.

```c++
bool is_sorted = std::is_sorted(days.begin(),
                                days.end(),
                                std::greater<DayOfWeek>());
```

Функция [std::less<T>()](https://en.cppreference.com/w/cpp/utility/functional/less) тоже реализована в стандартной библиотеке.

Чтобы сортировать данные по более сложным правилам, приходится писать свой компаратор. Для этого важно помнить требования к компараторам. Они общие для реализации компаратора на любом языке программирования. В них нет ничего специфичного для C++. Если компаратор нарушает хотя бы одно из требований, то в работе использующего компаратор алгоритма произойдет ошибка. Например, зацикливание, аварийное завершение или получение некорректного результата.

Допустим, наш компаратор называется `is_less()`. Для него должны выполняться условия:
- [Строгий частичный порядок](https://ru.wikipedia.org/wiki/%D0%9E%D1%82%D0%BD%D0%BE%D1%88%D0%B5%D0%BD%D0%B8%D0%B5_%D0%BF%D0%BE%D1%80%D1%8F%D0%B4%D0%BA%D0%B0#%D0%9E%D0%BF%D1%80%D0%B5%D0%B4%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D1%8F).
    - `!is_less(x, x)`. Антирефлексивность: значение `x` не может быть меньше самого себя.
    - Если `is_less(x, y) && is_less(y, z)`, то `is_less(x, z)`. Транзитивность: если `x` меньше `y`, а `y` меньше `z`, то `x` меньше `z`.
    - Если `is_less(x, y)`, то `!is_less(y, x)`. Антисимметричность. Она следует из двух предыдущих аксиом.
- Транзитивность эквивалентности.
    - Если `!is_less(x, y) && !is_less(y, x)`, то `x` и `y` эквивалентны: с точки зрения компаратора между ними нет разницы. Транзитивность эквивалентности означает, что если `x` эквивалентен `y`, а `y` эквивалентен `z`, то `x` эквивалентен `z`.

Если компаратор удовлетворяет строгому частичному порядку и транзитивности эквивалентности, то говорят, что он реализует строгий слабый порядок (strict weak ordering). Стандарт C++ требует, чтобы все передаваемые в алгоритмы компараторы поддерживали строгий слабый порядок. Вызов алгоритма с некорректным компаратором приводит к UB.

При написании собственных компараторов наиболее частая ошибка — это нарушение антисимметричности. Она допущена и в коде компаратора `has_higher_priority()`. Он нужен для сортировки товаров в первую очередь по убыванию популярности и во вторую — по убыванию рейтинга. {.task_text}

Найдите ошибку в компараторе и исправьте ее. Чтобы посмотреть, как компаратор применяется в коде, откройте эту задачу в плэйграунде. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0050}
struct Product
{
    int id = 0;
    int popularity = 0;
    int rating = 0; 
};

bool has_higher_priority(Product a, Product b)
{
    if (a.popularity > b.popularity)
        return true;
    
    if (a.rating > b.rating)
        return true;

    return false;
}
```

Допустим, в функцию переданы аргументы с полями `popularity=1000, rating=4` и `popularity=100, rating=5`. В зависимости от того, в каком порядке эти аргументы попадут в функцию, `has_higher_priority()` вернет разный результат! Это означает нарушение аксиомы антисимметричности. Чтобы исправить это, нужно добавить дополнительную проверку для сравнений поля `rating`. Условие `a.rating < b.rating` нужно дополнить: `a.popularity == b.popularity && a.rating < b.rating`. {.task_hint}
```c++ {.task_answer}
struct Product
{
    int id = 0;
    int popularity = 0;
    int rating = 0; 
};

bool has_higher_priority(Product a, Product b)
{
    if (a.popularity > b.popularity)
        return true;
    
    if (a.popularity == b.popularity && a.rating > b.rating)
        return true;

    return false;
}
```

До C++11 для реализации `std::sort()` подходил алгоритм быстрой сортировки [quicksort](https://ru.wikipedia.org/wiki/%D0%91%D1%8B%D1%81%D1%82%D1%80%D0%B0%D1%8F_%D1%81%D0%BE%D1%80%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%BA%D0%B0). Но начиная с C++11 стандарт гарантирует для этой функции алгоритмическую сложность `O(N * log(N))`. Quicksort в худшем случае имеет квадратичную сложность `O(N^2)` и  не удовлетворяет этому ограничению. 

Так какой же алгоритм заложен в `std::sort()`? Зависит от реализации.

В некоторых реализациях [используется](https://github.com/gcc-mirror/gcc/blob/releases/gcc-15/libstdc%2B%2B-v3/include/bits/stl_algo.h#L1874) интроспективная сортировка [introsort](https://en.wikipedia.org/wiki/Introsort). Это гибрид из трех способов сортировки. Вначале запускается quicksort. Если глубина рекурсии превышает некий порог, алгоритм переключается на пирамидальную сортировку [heapsort](https://ru.wikipedia.org/wiki/%D0%9F%D0%B8%D1%80%D0%B0%D0%BC%D0%B8%D0%B4%D0%B0%D0%BB%D1%8C%D0%BD%D0%B0%D1%8F_%D1%81%D0%BE%D1%80%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%BA%D0%B0). Ближе к концу, когда остаются небольшие интервалы, в игру вступает сортировка вставками [insertion sort](https://en.wikipedia.org/wiki/Insertion_sort).

Другие реализации `std::sort()` еще [более сложные](https://danlark.org/2022/04/20/changing-stdsort-at-googles-scale-and-beyond/). Они [включают](https://github.com/llvm-mirror/libcxx/blob/master/include/algorithm#L3899) эвристики и оптимизации в зависимости от длины интервалов и особенностей типа элементов.

Напишите функцию `contains_duplicate()`. Она принимает вектор целых чисел и возвращает `true`, если в нем есть дубликаты. {.task_text}

Например, для вектора `[3, 2, 3, 1]` функция вернет `true`. А для `[9, 2]`  — `false`. {.task_text}

Функция должна отработать _быстрее,_ чем за квадратичное время `O(N^2)`. Исходный массив можно изменять. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0060}
bool contains_duplicate(std::vector<int> v)
{

}
```
Сначала отсоритруйте вектор. Сложность сортировки — `O(N * log(N))` Затем пройдитесь по вектору, чтобы сравнить соседние элементы за `O(N)`. {.task_hint}
```c++ {.task_answer}
bool contains_duplicate(std::vector<int> v)
{
    if (v.empty())
        return false;
    
    std::sort(v.begin(), v.end());
    
    for (std::size_t i = 0; i < v.size() - 1; ++i)
    {
        if (v[i] == v[i + 1])
            return true;
    }

    return false;
}
```

## Копирование

Добавить элементы одного контейнера в конец другого не так просто, как хотелось бы. Например, вектор, в отличие от строк, не поддерживает конкатенацию через оператор `+=`: `s1 += s2`. Вместо нее используется алгоритм [std::copy](https://en.cppreference.com/w/cpp/algorithm/copy). Он принимает итераторы на входной диапазон и итератор на начало выходного диапазона:

```c++
template<class InputIt, class OutputIt>
OutputIt copy(InputIt first, InputIt last, OutputIt d_first)
{  /* Реализация */  }
```

Функция возвращает итератор _за_ последний элемент выходного диапазона.

Будьте внимательны: для использования `std::copy()` в выходном диапазоне должно быть не меньшее количество элементов, чем во входном диапазоне. Копирование в диапазон меньшего размера приведет к выходу за его границу. А значит, к [UB.](/courses/cpp/chapters/cpp_chapter_0050/#block-ub)

Например, UB есть в этом коде. Мы копируем 3 элемента из `source` по итератору _за_ последний элемент `destination`:

```c++  {.example_for_playground .example_for_playground_010}
std::vector<int> source = {6, 8, 2};
std::vector<int> destination = {1, 1, 1};

std::copy(source.begin(), source.end(), destination.end()); // UB
```

Чтобы избавиться от UB, заранее обеспечьте выходной диапазон необходимого размера. Сделаем это с помощью метода вектора [resize()](https://en.cppreference.com/w/cpp/container/vector/resize):

```c++ {.example_for_playground .example_for_playground_011}
std::vector<int> source = {6, 8, 2};
std::vector<int> destination = {1, 1, 1};

destination.resize(destination.size() + source.size());

std::copy(source.begin(), source.end(), destination.end() - source.size());

std::println("{}", destination);
```
```
[1, 1, 1, 6, 8, 2]
```

Удобен ли такой способ? Он подталкивает к ошибкам при определении длин диапазонов. Из-за них вы получите UB либо лишние элементы в конце выходного диапазона.

Поэтому если производительность при копировании не критична, вместо связки `resize()` и `std::copy()` применяется другой подход. В `std::copy()` вместо итератора на выходной диапазон передается специальная функция, которая возвращает _итератор-адаптер._

Итератор-адаптер — это не настоящий итератор, а класс, ведущий себя как итератор. Так, [std::back_insert_iterator](https://en.cppreference.com/w/cpp/iterator/back_insert_iterator) умеет только добавлять элементы в конец контейнера. Он вызывает метод контейнера `push_back()`, когда к элементу, на который указывает итератор-адаптер, присваивается значение оператором `=`.

Чтобы получить этот итератор-адаптер для контейнера, предусмотрена функция [std::back_inserter](https://en.cppreference.com/w/cpp/iterator/back_inserter). Она принимает контейнер и возвращает `std::back_insert_iterator`. 

```c++  {.example_for_playground .example_for_playground_012}
std::vector<int> source = {6, 8, 2};
std::vector<int> destination = {1, 1, 1};

std::copy(source.begin(), source.end(), std::back_inserter(destination));

std::println("{}", destination);
```
```
[1, 1, 1, 6, 8, 2]
```

Для вставки в начало контейнера есть итератор-адаптер [std::front_insert_iterator](https://en.cppreference.com/w/cpp/iterator/front_insert_iterator). Под капотом он вызывает метод `push_front()` контейнера. Чтобы получить этот итератор-адаптер, есть функция [std::front_inserter](https://en.cppreference.com/w/cpp/iterator/front_inserter)

```c++  {.example_for_playground .example_for_playground_013}
std::list<int> source = {6, 8, 2};
std::list<int> destination = {1, 1, 1};

std::copy(source.begin(), source.end(), std::front_inserter(destination));

std::println("{}", destination);
```
```
[2, 8, 6, 1, 1, 1]
```

Для вставки на произвольную позицию используется итератор-адаптер [std::insert_iterator](https://en.cppreference.com/w/cpp/iterator/insert_iterator), вызывающий метод контейнера `insert()`. Чтобы получить его для контейнера, нужно вызвать функцию [std::inserter](https://en.cppreference.com/w/cpp/iterator/inserter). Ознакомьтесь с ее документацией и потренируйтесь использовать ее в следующей задаче.

Необходимо реализовать функцию `insert_ad()`. Она принимает аудиопоток `stream`, метку для вставки рекламы `tag` и аудиопоток рекламы `ad` (advertisement). Функция возвращает аудиопоток со вставленной рекламой. {.task_text}

В аудиопотоке нужно найти все парные метки, то есть последовательность `tag`, идущую два раза подряд. Рекламу нужно вставить между этими метками. Удалять их из потока не надо. Считаем, что аудиопоток корректный: если он содержит метки, то они обязательно парные. {.task_text}

Пример: {.task_text}
- Аудиопоток: `[0x2a, 0x17, 0x14, 0x17, 0x01, 0x01, 0x01, 0x01, 0x55, 0x7c, 0x20]`.
- Метка: `[0x01, 0x01]`.
- Реклама: `[0x0b, 0x0a, 0x0d]`.
- Результат после вставки рекламы: `[0x2a, 0x17, 0x14, 0x17, 0x01, 0x01, 0x0b, 0x0a, 0x0d, 0x01, 0x01, 0x55, 0x7c, 0x20]`.

```c++ {.task_source #cpp_chapter_0080_task_0070}
std::vector<char> insert_ad(
    std::vector<char> stream, // аудиопоток
    std::vector<char> tag,    // метка для вставки рекламы
    std::vector<char> ad)     // реклама
{
    
}
```
Для поиска последовательности внутри другой последовательности воспользуйтесь [std::search()](https://en.cppreference.com/w/cpp/algorithm/search). Для определения смещения при последующем поиске пригодится [std::distance()](https://en.cppreference.com/w/cpp/iterator/distance). Для копирования будет нужна связка [std::copy()](https://en.cppreference.com/w/cpp/algorithm/copy) и [std::inserter()](https://en.cppreference.com/w/cpp/iterator/inserter). {.task_hint}
```c++ {.task_answer}
std::vector<char> insert_ad(
    std::vector<char> stream, // аудиопоток
    std::vector<char> tag,    // метка для вставки рекламы
    std::vector<char> ad)     // реклама
{
    std::size_t offset = 0;
    
    while(true)
    {
        auto it = std::search(stream.begin() + offset,
                              stream.end(),
                              tag.begin(),
                              tag.end());
        
        if (it == stream.end())
            break;

        it += tag.size();
        offset += std::distance(stream.begin(), it);
        std::copy(ad.begin(), ad.end(), std::inserter(stream, it));
        offset += ad.size() + tag.size();
    }

    return stream;
}
```

На примере `std::copy()` скажем про важную, но неочевидную особенность работы с алгоритмами. Если алгоритм не предназначен для модификации входного диапазона, но технически это допускает, будьте осторожны. Функция не должна изменять значения внутри входного диапазона или инвалидировать итераторы. Это приведет к UB. В случае с `std::copy()` это происходит, если входной и выходной диапазоны пересекаются:

```c++
std::vector<int> source = {6, 8, 2};

auto it = source.begin() + 1;
std::copy(source.begin(), source.end(), it); // UB
```

Поэтому при возникновении сомнений всегда сверяйтесь с документацией алгоритма на [cppreference.](https://en.cppreference.com/w/cpp/algorithm.html)

## Удаление и идиома erase-remove

Для удаления элементов из диапазона предназначены алгоритмы [std::remove и std::remove_if](https://en.cppreference.com/w/cpp/algorithm/remove). Но будьте бдительны: они ничего не удаляют.

```c++  {.example_for_playground .example_for_playground_014}
std::vector<int> v = {1, 2, 3, 3, 2, 1};

auto it = std::remove(v.begin(), v.end(), 2);

std::println("vector={}", v);
std::println("range without removed elements ends at {}'th element from 0",
             std::distance(v.begin(), it - 1));
```
```
vector=[1, 3, 3, 1, 2, 1]
range without removed elements ends at 3'th element from 0
```

Эти функции лишь _сдвигают_ в начало диапазона все элементы, которые не нужно удалять. И возвращают итератор на конец получившегося диапазона. Практически всегда следующий за этим итератором хвост из ненужных элементов требуется стереть. Он может содержать любой мусор. Для этого вызывается метод контейнера `erase()` либо функция стандартной библиотеки [std::erase()](https://en.cppreference.com/w/cpp/container/vector/erase2), появившаяся в C++20.

Использование `std::remove()` и `erase()` в связке известно как [идиома erase-remove.](https://en.wikipedia.org/wiki/Erase%E2%80%93remove_idiom)

```c++  {.example_for_playground .example_for_playground_015}
std::vector<int> v = {1, 2, 3, 3, 2, 1};

auto it = std::remove(v.begin(), v.end(), 2);
v.erase(it, v.end());

std::println("vector={}", v);
```
```
vector=[1, 3, 3, 1]
```

Чаще всего вызовы `std::remove()` и `erase()` записываются в одну строку:

```c++
v.erase(std::remove(v.begin(), v.end(), 2), v.end());
```

Как вы считаете, почему функция `std::remove()` не удаляет элементы по-настоящему? Она, как и любой алгоритм стандартной библиотеки, работает с итераторами, а не с контейнером напрямую. Итератор — это объект, который указывает на конкретный элемент контейнера. Ему известно расположение элемента в памяти и его тип. Поэтому единственное, что можно сделать с элементом через итератор — это изменить значение элемента. Чтобы передвинуть элемент или поменять элементы местами, нужны два итератора: изменение порядка элементов достигается за счет обмена значениями пары итераторов. А вот для _удаления_ элемента требуется доступ непосредственно к контейнеру. Поэтому алгоритм технически не может удалить что-то из контейнера.

## Подсчет

Иногда требуется подсчитать количество элементов диапазона, удовлетворяющих некоторому критерию. Для этого есть алгоритмы [std::count() и std::count_if()](https://en.cppreference.com/w/cpp/algorithm/count): 
- `std::count()` принимает итераторы на диапазон и значение. И возвращает количество элементов диапазона, равных значению.
- `std::count_if()` вместо значения принимает предикат.

```c++ {.example_for_playground}
import std;

int main()
{
    std::forward_list<std::string> blocks = {"a", "b", "b", "c"};

    // В не требующие изменения элементов алгоритмы можно передавать
    // обычные итераторы, а можно — константные
    const std::size_t n = std::count(blocks.cbegin(), blocks.cend(), "b");
    std::println("{}", n);
}
```
```
2
```

Чему равно значение переменной `n`? {.task_text}

```c++   {.example_for_playground}
import std;

bool is_neg(int val)
{
    return val < 0;
}

int main()
{
    std::vector<int> measurements = {-10, 5, 10, -2, -2};
    std::size_t n = std::count_if(measurements.begin() + 1,
                                  measurements.end() - 1,
                                  is_neg);
}
```

```consoleoutput {.task_source #cpp_chapter_0080_task_0080}
```
Подсчет отрицательных элементов осуществляется начиная с индекса 1 и до индекса 3 включительно. То есть среди элементов `5, 10, -2`. {.task_hint}
```cpp {.task_answer}
1
```

Еще одна распространенная задача — найти сумму или произведение элементов диапазона. Ее можно решить с помощью алгоритмов [std::accumulate()](https://en.cppreference.com/w/cpp/algorithm/accumulate) и [std::reduce()](https://en.cppreference.com/w/cpp/algorithm/reduce).

Функция `std::accumulate()` принимает итераторы на диапазон и начальное значение. И возвращает сумму начального значения с элементами диапазона:

```c++   {.example_for_playground .example_for_playground_016}
std::vector<double> distances = {34.3, 18.0, 51.5};

double total_distance = std::accumulate(distances.cbegin(),
                                        distances.cend(),
                                        0.0);

std::println("{}", total_distance);
```
```
103.8
```

Функция `get_average()` принимает вектор чисел и возвращает их среднее арифметическое. В ней допущена ошибка. Исправьте ее и перепишите код с использованием `std::accumulate()`. {.task_text}

```c++ {.task_source #cpp_chapter_0080_task_0090}
double get_average(std::vector<int> values)
{
    double sum = 0.0;

    for (double val: values)
        sum += val;

    return sum / values.size();
}
```
В исходной реализации функции если вектор `values` пуст, происходит деление на ноль. {.task_hint}
```c++ {.task_answer}
double get_average(std::vector<int> values)
{
    if (values.empty())
        return 0.0;

    return std::accumulate(values.begin(),
                           values.end(),
                           0.0) / values.size();
}
```

У `std::accumulate()` есть перегрузка, дополнительно принимающая функцию с двумя параметрами. Она нужна для расчета произведения или любого другого значения.

Что будет выведено в консоль? {.task_text}

Функция [std::to_string()](https://en.cppreference.com/w/cpp/string/basic_string/to_string) переводит число в строку.  {.task_text}

```c++
import std;

std::string dot_fold(std::string left, int right)
{
    return left + "." + std::to_string(right);
}

int main()
{
    std::vector<int> v = {5, 2, 0};
    std::println("{}", std::accumulate(v.cbegin() + 1,
                                       v.cend(),
                                       std::to_string(v.front()),
                                       dot_fold));
}
```

```consoleoutput {.task_source #cpp_chapter_0080_task_0100}
```
В данном случае `std::accumulate()` накапливает результат, начиная с нулевого элемента вектора, сконвертированного в строку. К этой строке через точку прибавляются последующие элементы вектора. {.task_hint}
```cpp {.task_answer}
5.2.0
```

## Когда использовать алгоритмы, а когда — методы класса?

Если вы проходите этот курс достаточно внимательно, то наверняка обратили внимание на интересную деталь. У классов ассоциативных контейнеров есть метод `find()`. Зачем он нужен, если в стандартной библиотеке существует алгоритм с таким же названием?

Функции стандартной библиотеки ничего не знают об устройстве контейнеров. Они реализуют обобщенные алгоритмы, работающие с итераторами. А методы класса, напротив, обладают полной информацией о его организации. За счет этого они эффективнее:

```c++   {.example_for_playground .example_for_playground_017}
std::unordered_map<int, std::string> warning_codes =
{
    {52, "API is deprecated"},
    {103, "API key is not provided"},
    // ...
};

// Метод применяет хеш-функцию к ключу и
// сразу получает элемент хеш-таблицы.
// Это O(1) в среднем и O(N) в худшем случае.
auto it_m = warning_codes.find(52);

// Функция просто перебирает все элементы
// от begin() до end() не включительно.
// Это O(N).
auto it_f = std::find(warning_codes.begin(), warning_codes.end(), 52);
```

Метод `find()` ассоциативного контейнера отработает быстрее, чем алгоритм `std::find()`.

Второй пример пересекающейся функциональности у алгоритма и метода — это удаление элементов. У списка `std::list` есть методы [remove() и remove_if()](https://en.cppreference.com/w/cpp/container/list/remove). Алгоритмы стандартной библиотеки с этими названиями применительно к спискам неэффективны. А методы класса имеют возможность _действительно_ удалить элементы списка, причем быстро.

Когда выбирать алгоритмы, а когда — методы класса? Если в приоритете скорость, то используйте методы. А если вы пишете обобщенный код, который должен работать с произвольными контейнерами, выбирайте функции стандартной библиотеки.

## Оптимальны ли алгоритмы и контейнеры

Классы и функции стандартной библиотеки разрабатываются с упором на три плохо совместимых качества:
- Универсальность.
- Эффективность.
- Обратную совместимость.

Это значит, что _в общем случае_ они показывают хорошие результаты. И уж точно превосходят простые самописные решения. Но [внимание к обратной совместимости](/courses/cpp/chapters/cpp_chapter_0010/#block-backward-compatibility) бьет по эффективности. То есть даже для усредненного сценария возможна более удачная реализация многих контейнеров и алгоритмов.

Возникает вопрос: в каких случаях стоит подбирать им замену?

Как всегда, руководствуйтесь здравым смыслом и фактами. По умолчанию выбирайте стандартную библиотеку. Это не требует от вас лишних действий. И даже при появлении проблем со скоростью или потреблением памяти не спешите мигрировать на стороннюю библиотеку. Принимайте решение, опираясь на профилирование производительности, бенчмарки и анализ потребления памяти.

_В большинстве случаев_ стандартная библиотека оказывается ни при чем, а проблемы лежат в совершенно другой плоскости. Например, упираются в неудачную архитектуру или синхронную обработку данных там, где нужна асинхронность.

Если же контейнер или алгоритм действительно стал бутылочным горлышком проекта, поэкспериментируйте с альтернативами: [Abseil от Google](https://github.com/abseil/abseil-cpp), [Folly от Meta](https://github.com/facebook/folly), уже [знакомым](/courses/cpp/chapters/cpp_chapter_0010/#block-boost) вам [Boost](https://www.boost.org/) и другими библиотеками с открытым исходным кодом.

## Домашнее задание

Самостоятельно разберитесь, в чем разница между алгоритмами:
- [std::accumulate()](https://en.cppreference.com/w/cpp/algorithm/accumulate) и [std::reduce()](https://en.cppreference.com/w/cpp/algorithm/reduce).
- [std::for_each()](https://en.cppreference.com/w/cpp/algorithm/for_each) и [std::transform()](https://en.cppreference.com/w/cpp/algorithm/transform).

----------

## Резюме

- Всегда предпочитайте алгоритмы стандартной библиотеки наивным самописным реализациям.
- Алгоритмы стандартной библиотеки — это шаблонные функции для поиска, сортировки, копирования диапазонов и других полезных операций.
- Для использования в некоторых алгоритмах пригодятся итераторы-адаптеры и возвращающие их функции.
- У некоторых контейнеров есть методы с такими же названиями, как у алгоритмов стандартной библиотеки. Они более эффективны.
- Идиома erase-remove заключается в применении алгоритма `std::remove()` или `std::remove_if()` с последующим вызовом метода `erase()` контейнера.
- Старайтесь периодически просматривать, [какие алгоритмы есть](https://en.cppreference.com/w/cpp/algorithm) в стандартной библиотеке. Применяйте их в своем коде.
