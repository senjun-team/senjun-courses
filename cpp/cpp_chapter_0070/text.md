# Глава 7. Обзор контейнеров

Контейнер — это коллекция элементов, реализованная как шаблон класса или структуры. Но в C++ не каждый шаблонный класс для хранения элементов может считаться контейнером. Контейнеры — это конкретные классы и структуры стандартной библиотеки. Их выбор разнообразен: есть массивы, хеш-таблицы, очереди и многое другое.

Шаблоны разных контейнеров имеют разный набор параметров. Но среди них всегда есть тип элемента.

Вам не обязательно досконально изучать интерфейс каждого из классов контейнеров, ведь всегда можно обратиться к [cppreference.](https://en.cppreference.com/w/cpp/container) Важнее уметь подбирать контейнер под конкретную задачу. А для этого полезно помнить:
- Под какие сценарии оптимизирован контейнер.
- Какова алгоритмическая сложность поддерживаемых им операций.
- Какие структуры данных у него _скорее всего_ под капотом.

Знание подкапотного устройства и алгоритмической сложности поможет находить ответы на вопросы:
- Насколько возрастет время доступа к элементу, если размер контейнера увеличится с сотни до десятков миллионов элементов?
- Почему в каких-то случаях вставка элемента работает быстро, а в каких-то — медленно?
- Какие контейнеры дружелюбны к кешу процессора ([cache-friendly](https://web.cecs.pdx.edu/~jrb/cs201/lectures/cache.friendly.code.pdf)), а какие — нет?

Стандарт C++ _не определяет_ внутреннюю реализацию контейнеров, зато фиксирует публичный интерфейс, ограничения на алгоритмическую сложность операций и некоторые свойства. Например, в какой последовательности хранятся элементы. У стандартной библиотеки C++ больше одной реализации. Но многие реализации контейнеров схожи из-за налагаемых стандартом требований.

## Классификация контейнеров

Контейнеры делятся на три категории, по одной на каждый из основных сценариев использования.

![Классификация контейнеров](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers.jpg) {.illustration}

В **последовательных** (sequence) контейнерах элементы хранятся линейно. Место элемента зависит от того, когда и на какую позицию он был добавлен. И не зависит от значения самого элемента. Вы уже [познакомились](/courses/cpp/chapters/cpp_chapter_0060/#block-vector) с типичным представителем последовательных контейнеров — динамическим массивом `std::vector`.

Поверх последовательных контейнеров реализованы **адаптеры.** Это классы, внутри использующие контейнеры, но предоставляющие над ними другой интерфейс. Например, поверх вектора организованы такие полезные структуры данных как [стек,](https://ru.wikipedia.org/wiki/%D0%A1%D1%82%D0%B5%D0%BA) [куча](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) и [очереди.](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5))

В **ассоциативных** (associative) контейнерах позиция элемента зависит от его ключа. Эти контейнеры предназначены для хранения пар «ключ-значение» или только ключей. Сопоставление ключу значения называется отображением (mapping) или ассоциацией (association).

Ассоциативные контейнеры бывают упорядоченными и неупорядоченными.

**Упорядоченные** (ordered) ассоциативные контейнеры часто называют просто ассоциативными контейнерами. Их элементы всегда отсортированы, а вставка, удаление и поиск элемента осуществляются за `O(log(N))`.

В **неупорядоченных** (unordered) ассоциативных контейнерах элементы, напротив, не отсортированы. Любое действие с элементом выполняется за `O(1)` в среднем и `O(N)` в худшем случае.

Класс `std::string`, [как вы помните,](/courses/cpp/chapters/cpp_chapter_0060/#block-string) не считается контейнером. Причина тому — ограничения на типы данных, которые позволяет хранить строка. Да, она может состоять не только из символов `char`! Но об этом позже. Внутри строка организована примерно так же, как `std::vector`, о котором вы сейчас узнаете.

Также контейнерами _не_ являются [std::bitset](https://en.cppreference.com/w/cpp/utility/bitset) и [std::valarray](https://en.cppreference.com/w/cpp/numeric/valarray). Класс `std::bitset` — это статический массив битов. А `std::valaray` — динамический массив, заточенный под математические вычисления. Этот класс не пользуется популярностью: он появился в C++98 и не отличается удобным интерфейсом. Вместо него для математических расчетов обычно подключают классы из специализированных библиотек, таких как [Eigen](https://eigen.tuxfamily.org/index.php?title=Main_Page) и [Armadillo](https://arma.sourceforge.net/).

## Последовательные контейнеры

Последовательные контейнеры представлены массивами, деком и списками.

![Последовательные контейнеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_sequential.jpg) {.illustration}

### Класс vector

Класс [std::vector](https://en.cppreference.com/w/cpp/container/vector) — ваш [выбор по умолчанию](https://timsong-cpp.github.io/cppwp/n4950/sequence.reqmts#2) для хранения последовательностей элементов. Это массив с динамически изменяемым размером. В нем есть методы для добавления и удаления элементов на любой позиции.

Тип элемента — это первый параметр шаблона класса `std::vector`. У шаблона есть и другие параметры, но мы не будем их задавать, то есть воспользуемся значениями по умолчанию.

```c++
std::vector<std::string> directions = {"left", "backward", "right"};
```

Для инициализации вектора `directions` мы использовали фигурные скобки, в которых перечислили значения элементов. Это так называемая списочная инициализация ([list-initialization](https://en.cppreference.com/w/cpp/language/list_initialization)). Она появилась в C++11 и позволяет заполнять контейнер в момент создания.

Для доступа к элементам по индексу используется оператор `[]`:

```c++ {.example_for_playground .example_for_playground_001}
std::vector<double> fractions = {0.05, 0.20, 0.80, 0.95};

double a = fractions[3]; // ok

// 3 - максимальный индекс. Что произойдет на этой строке?
double b = fractions[4];
```

Стандарт C++ относит чтение или запись элемента по несуществующему индексу к неопределенному поведению (UB, undefined behaviour). {#block-ub}

Есть ли в этом коде UB? `Y/N`. {.task_text}

```c++
std::vector<char> buffer = {'0', '0', '0', '0', '0'};

for (std::size_t i = 0; i <= buffer.size(); ++i)
    buffer[i] = '*';
```

```consoleoutput {.task_source #cpp_chapter_0070_task_0010}
```
Обратите внимание на максимальное значение, которое индекс `i` принимает в цикле. {.task_hint}
```cpp {.task_answer}
Y
```

Чтобы не писать проверок, но при этом избежать UB, предусмотрен метод `at()`. Если индекс выходит за границы массива, метод бросает исключение `std::out_of_range`:

```c++ {.example_for_playground .example_for_playground_002}
const std::vector<std::string> headers = {"Accept", "Cookie", "Expires"};

try
{
    const std::string header = headers.at(10);
    // ...
}
catch(const std::out_of_range & e)
{
    std::println("Exception in {}", e.what());
}
```

В классе `std::vector` реализовано множество других полезных методов. В первую очередь вам потребуются:
- `size()` — получение длины массива.
- `empty()` — проверка, является ли массив пустым. Хорошим правилом считается использовать проверку `if (v.empty())` вместо `if (v.size() == 0)`.
- `front()` — возвращает первый элемент.
- `back()` — возвращает последний элемент.
- `push_back()` — добавляет элемент в конец. Например, `v.push_back(1)`.
- `insert()` — добавляет элемент перед итератором. Вызов `v.insert(v.begin(), 1)` добавит элемент 1 в начало контейнера. У метода есть [несколько перегрузок.](https://en.cppreference.com/w/cpp/container/vector/insert) Метод возвращает итератор на вставленный элемент.
- `pop_back()` — удаляет последний элемент.
- `erase()` — удаляет один или несколько элементов. Вызов `v.erase(it)` удалит элемент, на который указывает итератор `it`. А `v.erase(it_start, it_end)` удалит элементы в диапазоне. У метода несколько [перегрузок.](https://en.cppreference.com/w/cpp/container/vector/erase) Метод возвращает итератор за последним удаленным элементом.
- `clear()` — удаляет все элементы.

```c++  {.example_for_playground .example_for_playground_003}
std::vector<std::string> column_names = {"id", "username", "date_joined"};

column_names.push_back("is_superuser");

column_names.insert(column_names.begin() + 1, "email");

for (std::string name: column_names)
    std::print("{} ", name);
```
```
id email username date_joined is_superuser 
```

Если говорить о внутренней реализации, то для вектора выделяется единая область памяти. В ней друг за другом хранятся элементы. При добавлении новых элементов этой памяти может не хватить. Тогда резервируется область большего объема, и в нее переносятся все элементы. Чаще всего объем удваивается, но это зависит от реализации.

Метод `capacity()` показывает емкость вектора: сколько элементов в него вмещается, прежде чем требуется реаллокация.

Выделение нового блока памяти, перенос в него элементов и освобождение старого блока — это медленные операции. Их можно избежать, если максимальное количество элементов известно заранее. Для этого используется метод `reserve()`. Он принимает количество элементов и сразу выделяет память нужного объема.

```c++ {.example_for_playground .example_for_playground_004}
std::vector<int> data;
std::println("size: {}, capacity: {}", data.size(), data.capacity());

data = {10, 20, 30, 40, 50};
std::println("size: {}, capacity: {}", data.size(), data.capacity());

data.reserve(1'000);
std::println("size: {}, capacity: {}", data.size(), data.capacity());
```
```
size: 0, capacity: 0
size: 5, capacity: 5
size: 5, capacity: 1000
```

Реализуйте функцию `merge()`, которая принимает два отсортированных по возрастанию вектора. Функция возвращает вектор, содержащий элементы первого и второго векторов с сохранением отсортированности. {.task_text}

Например, для векторов `1, 3, 3, 7` и `0, 1, 3, 4` функция должна вернуть вектор `0, 1, 1, 3, 3, 3, 4, 7`. {.task_text}

Если вам нужно уточнить информацию о методах контейнера, воспользуйтесь [cppreference.](https://en.cppreference.com/w/cpp/container/vector) Не стесняйтесь подглядывать туда при решении задач. {.task_text}

```c++ {.task_source #cpp_chapter_0070_task_0020}
std::vector<int> merge(std::vector<int> v_left, std::vector<int> v_right)
{

}
```
Перемещайтесь по обоим векторам и в каждый момент времени сравнивайте пару элементов из `v_left` и `v_right`. Если элемент из `v_left` меньше, добавляйте его в результирующий вектор и сдвигайте итератор по `v_left` вперед. Иначе добавляйте в результирующий вектор значение из `v_right` и перемещайте вперед итератор по этому вектору. {.task_hint}
```c++ {.task_answer}
std::vector<int> merge(std::vector<int> v_left, std::vector<int> v_right)
{
    std::vector<int> res;
    
    auto it_l = v_left.cbegin();
    auto it_r = v_right.cbegin();

    while (it_l != v_left.cend() || it_r != v_right.cend())
    {
        if (it_r == v_right.cend() || (it_l != v_left.cend() && *it_l < *it_r))
        {
            res.push_back(*it_l);
            ++it_l;
        }
        else if (it_r != v_right.cend())
        {
            res.push_back(*it_r);
            ++it_r;
        }
    }

    return res;
}
```

### Статический массив, дек и списки

Итак, для большинства задач подходит динамический массив [std::vector](https://en.cppreference.com/w/cpp/container/vector). Но если количество элементов известно и неизменно, выбирайте статический массив [std::array](https://en.cppreference.com/w/cpp/container/array). Он занимает меньше места, чем вектор. 

Тип и количество элементов — это параметры шаблона массива.

```c++
template<class T, std::size_t N>
struct array
{
    // Реализация
};
```

Создание массива из 4-х элементов типа `double`:

```c++
std::array<double, 4> interpolated_data = {77.0, 77.2, 76.99, 77.3};
```

Если ожидаются частые вставки в начало и конец контейнера, присмотритесь к классу [std::deque](https://en.cppreference.com/w/cpp/container/deque) (double-ended queue, двунаправленная очередь). Он реализует [структуру данных дек.](https://en.wikipedia.org/wiki/Double-ended_queue)

Дек поддерживает обращение к элементам по индексу. Этим он схож с массивами `std::vector` и `std::array`. Но у него есть важное отличие. Оно заключается в способе размещения элементов в памяти. Элементы массивов хранятся в памяти непрерывно, а элементы дека — кусочно-непрерывно.

В большинстве реализаций под элементы дека выделяется _набор_ массивов фиксированной длины. Они могут располагаться в совершенно разных участках памяти. Поэтому заводится дополнительный контейнер, хранящий адрес каждого из массивов. Зная индекс элемента в деке и длину массива, легко вычислить, где именно этот элемент расположен.

Заведем дек `d` и поработаем с ним:

```c++  {.example_for_playground .example_for_playground_005}
std::deque<int> d = {7, 6, 2, 0, 3, 9, 0, 1, 5, 3, 5, 5};
std::println("   {}", d); // Пробелы для отступа слева

d.push_front(4);
d.push_back(8);
d.push_back(3);

std::println("{}", d);
```
```
   [7, 6, 2, 0, 3, 9, 0, 1, 5, 3, 5, 5]
[4, 7, 6, 2, 0, 3, 9, 0, 1, 5, 3, 5, 5, 8, 3]
```

Если допустить, что выделяемые под дек массивы содержат по 6 элементов, дек `d` можно представить так:

![Дек](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/deque.jpg) {.illustration}

Доступ к элементам по индексу за `O(1)` — это ключевое свойство классов `std::array`, `std::vector` и `std::deque`. Но вставка и удаление по произвольному индексу в них работают медленно: за `O(N)`. Ведь для этого нужно сдвинуть все элементы с бОльшим индексом. После чего итераторы на сдвинутые элементы инвалидируются. А добавление нового элемента в вектор может привести к перевыделению памяти и инвалидации всех итераторов.

Списки `std::list` и `std::forward_list` устроены иначе. Их элементы находятся в разных участках памяти. При добавлении или удалении любого из них итераторы на остальные элементы остаются валидными. А вставка или удаление произвольного элемента работают быстро: за `O(1)`. При условии, что вы уже получили итератор на нужную позицию.

Итак, списки хороши для частых вставок и удалений из середины. Но они не поддерживают обращение по индексу: для получения N-ного элемента придется перебрать все элементы перед ним.

Класс [std::list](https://en.cppreference.com/w/cpp/container/list) реализует [структуру данных «двусвязный список»](https://ru.wikipedia.org/wiki/%D0%A1%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA#%D0%94%D0%B2%D1%83%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_(%D0%B4%D0%B2%D1%83%D0%BD%D0%B0%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9_%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA)) и поддерживает обход контейнера в обе стороны.

Класс [std::forward_list](https://en.cppreference.com/w/cpp/container/forward_list) реализует [односвязный список](https://ru.wikipedia.org/wiki/%D0%A1%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA#%D0%9E%D0%B4%D0%BD%D0%BE%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_(%D0%BE%D0%B4%D0%BD%D0%BE%D0%BD%D0%B0%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D0%B5%D0%BD%D0%BD%D1%8B%D0%B9_%D1%81%D0%B2%D1%8F%D0%B7%D0%BD%D1%8B%D0%B9_%D1%81%D0%BF%D0%B8%D1%81%D0%BE%D0%BA)) и поддерживает обход только от начала к концу.

В прошлой главе вы [узнали,](/courses/cpp/chapters/cpp_chapter_0060/#block-iterator-categories) что итераторы в C++ делятся на несколько категорий. {.task_text}

Как считаете, к какой из них относятся итераторы по `std::forward_list`? Напишите `random access`, `bidirectional` или `forward`. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0070_task_0090}
```
В односвязном списке можно перебирать элементы только от начала к концу. {.task_hint}
```cpp {.task_answer}
forward
```

Реализуйте функцию `get_equator()`. Она принимает не пустой односвязный список и возвращает значение элемента, стоящего на экваторе. {.task_text}

Экватор — это последний элемент в списке, сумма элементов _до_ которого меньше суммы элементов _после._ Значение самого элемента в обеих суммах не учитывается. Для первого элемента сумму элементов _до_ считаем равной 0. Для последнего элемента сумму элементов _после_ тоже считаем равной 0. {.task_text}

Примеры: {.task_text}
- Список: `5 -> 2 -> 4 -> 8 -> 1`. Результат: 4. Сумма элементов `5, 2` меньше суммы элементов `8, 1`. Сумма `5, 2, 4` уже больше значения `1`.
- Список: `100 -> 50 -> 25 -> 12`. Результат: 100. Это единственный элемент, для которого сумма _до_ меньше суммы _после._

```c++ {.task_source #cpp_chapter_0070_task_0030}
int get_equator(std::forward_list<int> lst)
{

}
```
Заведите вектор и заполните его в процессе итерирования по списку `lst`. Сохраняйте в вектор сумму элементов, включая данный. Длина вектора будет совпадать с длиной списка. Например, для списка `5 -> 2 -> 4 -> 8 -> 1` вы получите вектор `5, 7, 11, 19, 20`. В этом же проходе по списку сохраните в переменную сумму всех элементов. Затем проитеритуйтесь по вектору. Зная общую сумму элементов списка и частичную сумму, хранящуюся в элементе вектора, легко вычислить сумму элементов до данного и после данного. {.task_hint}
```c++ {.task_answer}
int get_equator(std::forward_list<int> lst)
{
    std::vector<int> sums;
    int sum = 0;

    for (auto it = lst.cbegin(); it != lst.cend(); ++it)
    {
        sum += *it;
        sums.push_back(sum);
    }

    int sum_left = 0;
    int equator = 0;
    
    for (std::size_t i = 0; i < sums.size(); ++i)
    {
        const int sum_right = sum - sums[i];
        
        if (sum_left < sum_right)
            equator = sum - sum_right - sum_left;
        
        sum_left = sums[i];
    }

    return equator;
}
```

### Алгоритмическая сложность работы с последовательными контейнерами

![Алгоритмическая сложность работы с последовательными контейнерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_sequential_algo_complexity.jpg) {.illustration}

Кстати, алгоритмическая сложность операций над строкой `std::string` такая же, как над вектором: их внутреннее устройство схоже.

Реализуйте шаблонную функцию `is_diagonal()`. Она принимает квадратную матрицу, реализованную как массив массивов. {.task_text}

Функция возвращает `true`, если матрица является [диагональной.](https://ru.wikipedia.org/wiki/%D0%94%D0%B8%D0%B0%D0%B3%D0%BE%D0%BD%D0%B0%D0%BB%D1%8C%D0%BD%D0%B0%D1%8F_%D0%BC%D0%B0%D1%82%D1%80%D0%B8%D1%86%D0%B0) В такой матрице все элементы вне главной диагонали равны нулю. Например: {.task_text}

```
5  0  0
0 -1  0
0  0  2
```

```c++ {.task_source #cpp_chapter_0070_task_0060}
template<std::size_t N>
bool is_diagonal(std::array<std::array<int, N>, N> matrix)
{

}
```
Главная диагональ матрицы состоит из элементов с одинаковыми индексами: `matrix[0][0]`, `matrix[1][1]` и так далее. {.task_hint}
```c++ {.task_answer}
template<std::size_t N>
bool is_diagonal(std::array<std::array<int, N>, N> matrix)
{
    for(std::size_t i = 0; i < matrix.size(); ++i)
    {
        for (std::size_t j = 0; j < matrix[i].size(); ++j)
        {
            if (i == j)
                continue;
            
            if (matrix[i][j] != 0)
                return false;
        }
    }

    return true;
}
```

## Упорядоченные ассоциативные контейнеры

Упорядоченные ассоциативные контейнеры хранят в отсортированном виде ключи или пары ключ-значение. Доступ к элементу осуществляется по ключу и работает за `O(log(N)`.

![Ассоциативные контейнеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_assotiative.jpg) {.illustration}

Такие контейнеры реализуются через [бинарные деревья поиска.](https://ru.wikipedia.org/wiki/%D0%94%D0%B2%D0%BE%D0%B8%D1%87%D0%BD%D0%BE%D0%B5_%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D0%BE_%D0%BF%D0%BE%D0%B8%D1%81%D0%BA%D0%B0) Чаще всего — через [красно-черные деревья.](https://ru.wikipedia.org/wiki/%D0%9A%D1%80%D0%B0%D1%81%D0%BD%D0%BE-%D1%87%D1%91%D1%80%D0%BD%D0%BE%D0%B5_%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D0%BE)

Деревья поиска сбалансированы: в них длина левого и правого поддерева отличается не более чем на 1 уровень. За счет этого поиск работает быстро. Но для поддержания сбалансированности при вставке или удалении иногда приходится поворачивать дерево (балансировать). Поворот дерева означает, что часть его узлов меняется местами. Следовательно, итераторы на некоторые элементы инвалидируются.

Чтобы тип данных мог быть ключом в упорядоченном ассоциативном контейнере, к нему должно быть применимо сравнение оператором `<`.

Познакомимся с классом `std::map`. Но сперва сделаем отступление про класс для хранения пар ключ-значение в `std::map` и подобных контейнерах.

### Класс pair

Шаблонный класс `std::pair` позволяет хранить пару объектов. Типы объектов задаются двумя параметрами шаблона и могут отличаться. А доступ к объектам осуществляется через поля `first` и `second`.

```c++  {.example_for_playground .example_for_playground_006}
std::pair<std::size_t, std::string> user = {507, "bot_master"};

std::size_t id = user.first;
std::string login = user.second;
```

### Класс map

Контейнер [std::map](https://ru.cppreference.com/w/cpp/container/map) предназначен для хранения пар ключ-значение с уникальным ключом. Элементы `std::map` — это объекты класса `std::pair`. Шаблон класса `std::map` принимает два параметра: тип ключа и тип значения. Ключи хранятся отсортированными по возрастанию.

```c++  {.example_for_playground .example_for_playground_007}
// Ключ - имя пользователя.
// Значение - Unix timestamp последнего логина
std::map<std::string, std::time_t> user_last_login = {
    {"Alice", 1153044881},
    {"Bob", 1743461707},
};

// Вывод отсортирован по имени пользователя
std::println("{}", user_last_login);
```
```
{"Alice": 1743461707, "Bob": 1153044881}
```

**Доступ к элементу** можно организовать несколькими способами: через оператор `[]`, методы `at()` и `find()`.

Обращение через `[]` _создает_ ключ, если он отсутствует, и к нему привязывается значение по умолчанию.

```c++  {.example_for_playground .example_for_playground_008}
// Ключ существует, получаем его значение
std::time_t login_time_a = user_last_login["Alice"];

// Ключа нет, он добавляется
std::time_t login_time_e = user_last_login["Eve"];

std::println("{}", user_last_login);
```
```
{"Alice": 1153044881, "Bob": 1743461707, "Eve": 0}
```

Добавление ключа в случае его отсутствия может быть нежелательным. Если по логике программы ключ _обязан_ присутствовать в контейнере, используйте метод `at()`. Он вернет значение либо в случае его отсутствия бросит исключение. 

```c++  {.example_for_playground .example_for_playground_009}
try
{
    std::time_t login_time_e = user_last_login.at("Eve");
    std::println("{}", login_time_e);
}
catch(const std::out_of_range & e)
{
    std::println("Not found");
}
```

На случаи, когда уверенности в присутствии ключа нет, предусмотрен метод `find()`. Он принимает ключ и возвращает итератор.

```c++  {.example_for_playground .example_for_playground_010}
auto it = user_last_login.find("Eve");

if (it == user_last_login.end())
{
    std::println("Not found");
}
else
{
    std::pair<const std::string, std::time_t> record = *it;
    std::println("Key: {}. Value: {}", record.first, record.second);
}
```

Оператор разыменования `*` применяется к итератору для получения элемента, на который он указывает. Обратите внимание, что в паре ключ-значение ключ константен: `std::pair<const std::string, std::time_t>`. Это говорит о том, что у элемента нельзя изменить ключ.

В реальном коде практически никогда не требуется явное получение из итератора переменной класса `std::pair`. Доступ к ключу и значению организуется проще:

```c++
std::println("Key: {}. Value: {}", (*it).first, (*it).second);
```

Разберем, что означают записи `(*it).first` и `(*it).second`. Для обращения к полю используется оператор доступа к элементу `.`. Но у операторов `*` и `.` [одинаковый приоритет.](https://en.cppreference.com/w/cpp/language/operator_precedence) Чтобы сначала получить пару, а после этого обратиться к ее полю, разыменование заключается в скобки: `(*it)`. И к полученному объекту пары применяется оператор доступа к элементу: `(*it).first`.

В C++ есть оператор `->`, позволяющий выразить то же самое, но короче. Эти две записи эквивалентны:

```c++
(*it).first
```

```c++
it->first
```

Перебор элементов `std::map` реализуется двумя способами: циклом по итераторам и циклом `range-for`.

Цикл по итераторам:

```c++  {.example_for_playground .example_for_playground_011}
for (auto it = user_last_login.begin(); it != user_last_login.end(); ++it)
    std::println("Key: {}. Value: {}", it->first, it->second);
```

Цикл `range-for`:

```c++  {.example_for_playground .example_for_playground_012}
for (std::pair<std::string, std::time_t> record: user_last_login)
    std::println("Key: {}. Value: {}", record.first, record.second);
```

При обходе контейнера через `range-for` мы работаем с парами ключ-значение. Вместо явного указания типа можно использовать `auto`:

```c++  {.example_for_playground .example_for_playground_013}
for (auto record: user_last_login)
    std::println("Key: {}. Value: {}", record.first, record.second);
```

Чтобы в цикле _изменять_ значения по ключу, используйте цикл с итераторами. В главе про ссылки вы узнаете, как это делать в цикле `range-for`.

```c++  {.example_for_playground .example_for_playground_014}
for (auto it = user_last_login.begin(); it != user_last_login.end(); ++it)
    it->second = 0;
```

Для **вставки элемента** в контейнер `std::map` предусмотрено несколько способов. Перечислим три из них: оператор `[]`, методы `try_emplace()` и `insert_or_assign()`. 

Оператор `[]` добавляет значение по ключу либо перезаписывает уже существующее. Используйте его, если вам не требуется отличать вставку от перезаписи:

```c++   {.example_for_playground .example_for_playground_015}
// Ключ - id сервера, значение - имя
std::map<std::size_t, std::string> server_names;

server_names[902] = "stage";
```

Метод [try_emplace()](https://en.cppreference.com/w/cpp/container/map/try_emplace) принимает ключ и значение. Он возвращает пару из итератора и флага. Флаг равен `true`, если вставка произошла, и `false`, если элемент по ключу уже существовал. Замены значения существующего элемента на новое не происходит. А возвращаемый итератор в любом случае указывает на элемент по ключу.

```c++   {.example_for_playground .example_for_playground_016}
std::map<std::size_t, std::string> server_names;

std::size_t server_id = 902;
std::string name = "stage";

auto [it, inserted] = server_names.try_emplace(server_id, name);

if (inserted)
    std::println("Inserted key {} with value {}", it->first, it->second);
else
    std::println("Key {} exists. Value: {}", it->first, it->second);
```

Чтобы в случае существования ключа обновить его значение, есть метод [insert_or_assign()](https://en.cppreference.com/w/cpp/container/map/insert_or_assign).

```c++ {.example_for_playground .example_for_playground_017}
auto [it, inserted] = server_names.insert_or_assign(server_id, name);

if (inserted)
    std::println("Inserted key {} with value {}", it->first, it->second);
else
    std::println("Updated key's {} value to {}", it->first, it->second);
```

Методы `try_emplace()` и `insert_or_assign()` появились в C++17. До них для вставки элементов использовался не такой удобный и эффективный [insert()](https://en.cppreference.com/w/cpp/container/map/insert).

Для **удаления** элемента предусмотрен метод [erase()](https://en.cppreference.com/w/cpp/container/map/erase). У него несколько перегрузок для удаления по ключу, итератору и диапазону итераторов. Перегрузка, принимающая ключ, возвращает количество удаленных элементов (0 или 1). А перегрузки с итераторами возвращают итератор на элемент после удаленного.

```c++
bool removed = server_names.erase("stage") > 0;
```

Реализуйте функцию `print_frequency()`. Она принимает вектор слов и выводит в консоль частоту, с которой встречается каждое слово. Вывод должен быть отсортирован по убыванию частоты. Слова с одинаковой частотой должны быть отсортированы по алфавиту. {.task_text}

В своем решении используйте `std::map`, `std::vector` и функцию для сортировки [std::sort()](https://en.cppreference.com/w/cpp/algorithm/sort), перегрузка которой принимает итераторы на диапазон и предикат. Функция сортирует элементы диапазона, попарно применяя предикат к его элементам: если предикат вернул `true` для элементов `a` и `b`, то в отсортированном диапазоне элемент `a` будет идти перед `b`. В качестве предиката используйте функцию `is_less()`. {.task_text #block-sort}

Пример консольного вывода функции `print_frequency()` для вектора `{"login", "register", "login", "start_course"}`: {.task_text}

```
2 login  
1 register
1 start_course
```

```c++ {.task_source #cpp_chapter_0070_task_0040}
bool is_less(std::pair<std::string, std::size_t> left,
             std::pair<std::string, std::size_t> right)
{
    if (left.second > right.second)
        return true;
    if (left.second < right.second)
        return false;
    
    return left.first < right.first;
}

void print_frequency(std::vector<std::string> words)
{

}
```
Заполните контейнер `std::map`, ключами которого будут слова, а значениями — их частоты. Затем заполните вектор, хранящий пары со словами и частотами. К вектору примените функцию сортировки с предикатом `is_less()`. Например: `std::sort(v.begin(), v.end(), is_less)`. {.task_hint}
```c++ {.task_answer}
bool is_less(std::pair<std::string, std::size_t> left,
             std::pair<std::string, std::size_t> right)
{
    if (left.second > right.second)
        return true;
    if (left.second < right.second)
        return false;
    
    return left.first < right.first;
}

void print_frequency(std::vector<std::string> words)
{
    std::map<std::string, std::size_t> words_count;

    for(std::string word: words)
    {
        auto [it, inserted] = words_count.try_emplace(word, 1);

        if (!inserted)
            ++it->second;    
    }

    std::vector<std::pair<std::string, std::size_t>> words_with_freq;
    words_with_freq.reserve(words_count.size());
    
    for (auto it = words_count.begin(); it != words_count.end(); ++it)
        words_with_freq.push_back(*it);

    std::sort(words_with_freq.begin(), words_with_freq.end(), is_less);

    for (auto & word_freq: words_with_freq)
        std::println("{} {}", word_freq.second, word_freq.first);
}
```

Для **поиска** элемента по ключу есть метод `find()`. Он возвращает итератор на нужный элемент. Если такового не нашлось, итератор указывает на позицию после последнего элемента.

### Класс set

Класс [std::set](https://en.cppreference.com/w/cpp/container/set) нужен, чтобы работать со множеством уникальных ключей. В отличие от `std::map`, значения к ним не привязаны. В целом эти контейнеры схожи за исключением некоторых нюансов. Например, У `std::set` нет метода `try_emplace()`, потому что он не имел бы особого смысла. Вместо него есть метод [emplace()](https://en.cppreference.com/w/cpp/container/set/emplace).

```c++ {.example_for_playground .example_for_playground_018}
std::set<std::string> words = {"a", "the"};

words.emplace("then");
words.emplace("a");

words.erase("the");

std::println("Key \"then\" exists: {}. Set items:", words.contains("then"));

for (std::string word: words)
    std::print("{} ", word);
```
```
Key "then" exists: true. Set items:
a then
```

Класс `IPv4Pool` — это пул свободных [IPv4](https://ru.wikipedia.org/wiki/IPv4) адресов. С помощью пула можно зарезервировать ip-адрес и освободить его после использования. Необходимо реализовать методы класса `IPv4Pool`. {.task_text}

`IPv4Pool(IPv4Range range)` — конструктор, принимающий диапазон доступных ip-адресов. Диапазон не может быть пустым. {.task_text}

`std::pair<IPv4, bool> reserve_ip()` — находит первый свободный `IPv4` адрес и резервирует его. Возвращает `true` в случае успеха. Если свободных адресов не осталось, то возвращает пару `{IPv4(), false}`.  {.task_text}

`bool release_ip(IPv4 ip4)` — освобождает зарезервированный ip-адрес и возвращает `true`. Если переданный адрес отсутствует в пуле, то метод возвращает `false`. {.task_text}

Классы `IPv4` и `IPv4Range` уже есть в проекте. `IPv4` реализует ip-адрес. Для его объектов доступны все виды сравнения и форматированный вывод. {.task_text}

Класс диапазона `IPv4Range` позволяет перечислить все входяшие в него ip-адреса. Он пригоден для обхода циклом [range-for](/courses/cpp/chapters/cpp_chapter_0040/#block-range-for). Также можно воспользоваться методами `cbegin()`, `cend()` и `find()`, которые возвращают `IPv4Range::const_iterator` на начало, конец и на указанный ip-адрес соответственно. Размер диапазона можно получить с помощью метода `size()`. {.task_text}

```c++ {.task_source #cpp_chapter_0070_task_0100}
class IPv4Pool
{
public:
    explicit IPv4Pool(IPv4Range range)
    {

    }

    std::pair<IPv4, bool> reserve_ip()
    {

    }

    bool release_ip(IPv4 ip4)
    {

    }

private:

};
```
Заведите множество `std::set` для хранения зарезервированных ip-адресов. Для поиска свободного ip-адреса обойдите диапазон, переданный в конструкторе пула. В цикле проверяйте наличие текущего адреса в множестве зарезервированных. Верните первый не найденный среди зарезервированных адресов. Для освобождения ip-адреса достаточно удалить его из множества зарезервированных. Предложенное решение неэффективно при резервировании большого количества ip-адресов сразу. Подумайте над более оптимальным вариантом. {.task_hint}
```c++ {.task_answer}
class IPv4Pool
{
public:
    explicit IPv4Pool(IPv4Range range)
    {
        m_range = range;
        m_cursor = m_range.cbegin();
    }

    std::pair<IPv4, bool> reserve_ip()
    {
        if (m_range.size() == m_reserved.size())
            return { IPv4(), false };

        const auto end = m_range.cend();

        while (true)
        {
            if (m_cursor == end)
                m_cursor = m_range.cbegin();

            auto emplaced = m_reserved.emplace(*m_cursor);

            if (emplaced.second)
                return { *m_cursor, true };

            ++m_cursor;
        }
    }

    bool release_ip(IPv4 ip4)
    {
        const bool released = m_reserved.erase(ip4) > 0;
        if (released)
            m_cursor = m_range.find(ip4);

        return released;
    }

private:
    IPv4Range m_range;
    std::set<IPv4> m_reserved;
    IPv4Range::const_iterator m_cursor;
};
```

В C++17 в `std::set` был добавлен метод `merge()`. Он нужен, чтобы объединять два множества:

```c++ {.example_for_playground .example_for_playground_019}
std::set<int> a = {1, 2, 3};
std::set<int> b = {0, 2, 4};

a.merge(b);

std::println("{}", a);
```
```
{0, 1, 2, 3, 4}
```

### Классы multimap и multiset

В классах [std::multimap](https://en.cppreference.com/w/cpp/container/multimap) и [std::multiset](https://en.cppreference.com/w/cpp/container/multiset) ключи могут повторяться. Допустим, у нас есть несколько серверов, на которых выполняются задания. Мы могли бы представить это как `std::map` с ключом — именем сервера и значением — вектором заданий:

```c++ {.example_for_playground .example_for_playground_020}
std::map<std::string, std::vector<std::string>> server_jobs;

auto [it, inserted] = server_jobs.try_emplace("stage", 
                                            std::vector<std::string>
                                            {"db_replication"}
                                            );
if (!inserted)
    it->second.push_back("db_replication"); // Вызываем push_back() у вектора
```

А можно использовать `std::multimap`. Его метод [equal_range()](https://en.cppreference.com/w/cpp/container/multimap/equal_range) принимает ключ и возвращает итераторы на начало и конец диапазона элементов с этим ключом. У `std::multimap` нет метода `try_emplace()`, потому что пара ключ-значение может быть добавлена, даже если ключ уже существует в контейнере. Вместо него есть метод [emplace()](https://en.cppreference.com/w/cpp/container/multimap/emplace).

```c++ {.example_for_playground .example_for_playground_021}
std::multimap<std::string, std::string> server_jobs;

server_jobs.emplace("stage", "db_replication");
server_jobs.emplace("stage", "file_storage_backup");
server_jobs.emplace("prod", "packages_update");

for (auto[it, it_end] = server_jobs.equal_range("stage"); it != it_end; ++it)
    std::println("Job {}", it->second);
```

### Алгоритмическая сложность работы с ассоциативными контейнерами

![Алгоритмическая сложность работы с упорядоченными ассоциативными контейнерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_associative_algo_complexity.jpg) {.illustration}

## Неупорядоченные ассоциативные контейнеры

Имена классов неупорядоченных ассоциативных контейнеров похожи на имена упорядоченных. Их отличает лишь префикс `unordered`.

![Неупорядоченные ассоциативные контейнеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_unordered_assotiative.jpg) {.illustration}

Но внутренней реализацией эти категории контейнеров принципиально отличаются. Элементы упорядоченных ассоциативных контейнеров всегда отсортированы, а элементы неупорядоченных хранятся в произвольном порядке. Упорядоченные ассоциативные контейнеры — это деревья поиска, а неупорядоченные — [хеш-таблицы.](https://ru.wikipedia.org/wiki/%D0%A5%D0%B5%D1%88-%D1%82%D0%B0%D0%B1%D0%BB%D0%B8%D1%86%D0%B0) 

Поиск, добавление и удаление элементов в хеш-таблице осуществляется по ключу. Для этого к ключу `key` применяется хеш-функция. Она преобразует ключ в целое число. Затем по этому числу высчитывается индекс `i` в массиве. Например, делением по модулю, равному длине `n` массива:

```
i = hash(key) % n
```

По этому индексу в массиве хранится привязанное к ключу значение.

Множество ключей потенциально бесконечно. А количество значений целого числа ограничено. Значит, неизбежны ситуации, когда для разных ключей хеш-функция вернет одно и то же значение. Это называется коллизией. Частота коллизий зависит от того, насколько удачно подобрана хеш-функция.

Для разрешения коллизий известно [несколько способов.](https://ru.wikipedia.org/wiki/%D0%A5%D0%B5%D1%88-%D1%82%D0%B0%D0%B1%D0%BB%D0%B8%D1%86%D0%B0#%D0%A0%D0%B0%D0%B7%D1%80%D0%B5%D1%88%D0%B5%D0%BD%D0%B8%D0%B5_%D0%BA%D0%BE%D0%BB%D0%BB%D0%B8%D0%B7%D0%B8%D0%B9) В реализациях стандартной библиотеки как правило используется метод цепочек. Иногда его называют методом списков, потому что каждый элемент массива указывает на односвязный список. Он содержит пары ключ-значение. Если коллизий по данному хеш-значению не было, список состоит из единственного элемента:

![Разрешение коллизий методом цепочек](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/hash_table_separate_chaining.jpg) {.illustration}

Чтобы тип данных мог быть ключом в неупорядоченном ассоциативном контейнере, должны выполняться условия:
- К нему применимо сравнение оператором `==`.
- Для него определена хеш-функция. По умолчанию используется шаблонная функция `std::hash<Key>()`.

### Классы unordered_* контейнеров

Интерфейсы упорядоченных и неупорядоченных ассоциативных контейнеров практически идентичны, поэтому подробно останавливаться на них не имеет смысла.

Множество ключей [std::unordered_set](https://en.cppreference.com/w/cpp/container/unordered_set) используется для быстрой проверки на уникальность и для подсчета количества уникальных объектов.

```c++ {.example_for_playground .example_for_playground_022}
std::unordered_set<std::string> bluetooth_protocols = {"BNEP"};

bluetooth_protocols.insert("RFCOMM");
bluetooth_protocols.insert("SDP");
bluetooth_protocols.insert("SDP");

bluetooth_protocols.erase("BNEP");

auto it = bluetooth_protocols.find("RFCOMM");

std::println("Key RFCOMM exists: {}", it != bluetooth_protocols.end());
std::println("Set: {} Size: {}", bluetooth_protocols, bluetooth_protocols.size());
```
```
Key RFCOMM exists: true
Set: {"SDP", "RFCOMM"} Size: 2
```

Класс [std::unordered_map](https://en.cppreference.com/w/cpp/container/unordered_map) позволяет хранить пары ключ-значение с уникальным ключом.

А классы [std::unordered_multimap](https://en.cppreference.com/w/cpp/container/unordered_multimap) и [std::unordered_multiset](https://en.cppreference.com/w/cpp/container/unordered_multiset) не требуют уникальности ключа. 

Реализуйте функцию `consists_of()`, которая принимает текст послания `message` и текст журнала `magazine`. Функция возвращает `true`, если текст послания может быть составлен из вырезанных из журнала букв. Все буквы только латинские в нижнем регистре. Пробелы учитывать не нужно. {.task_text}

Напимер, `consists_of("bab", "abbc")` вернет `true`, а `consists_of("bab", "abc")` — `false`. {.task_text}

```c++ {.task_source #cpp_chapter_0070_task_0050}
bool consists_of(std::string message, std::string magazine)
{

}
```
Заведите вспомогательную функцию, которая принимает строку и возвращает `std::map`, ключи которого — символы, а значения — их частота в строке. Это частотный словарь. Примените эту функцию к посланию и тексту журнала. Затем проитерируйтесь по частотному словарю послания и проверьте, что каждый ключ в нем содержится в словаре журнала. И значения по этому ключу в словаре послания больше или равны значениям в словаре журнала. {.task_hint}
```c++ {.task_answer}
std::unordered_map<char, std::size_t> to_dict(std::string text)
{
    std::unordered_map<char, std::size_t> dict;

    for(char c: text)
    {
        if (c == ' ')
            continue;

        auto [it, inserted] = dict.try_emplace(c, 1);

        if (!inserted)
            ++it->second;    
    }

    return dict;
}

bool consists_of(std::string message, std::string magazine)
{
    const auto dict_message = to_dict(message);
    const auto dict_magazine = to_dict(magazine);

    for (auto it = dict_message.cbegin(); it != dict_message.cend(); ++it)
    {
        const auto it_mag = dict_magazine.find(it->first);
        
        if (it_mag == dict_magazine.end() || it_mag->second < it->second)
            return false;
    }

    return true;
}
```

### Алгоритмическая сложность работы с неупорядоченными ассоциативными контейнерами

![Алгоритмическая сложность работы с неупорядоченными ассоциативными контейнерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_unordered_associative_algo_complexity.jpg) {.illustration}

## Адаптеры

Последовательные контейнеры реализуют плоские структуры данных, в которых нет иерархии или вложенности. Поверх плоских структур можно эффективно имплементировать несколько других структур данных. Это и есть адаптеры — обертки над последовательными контейнерами.

![Адаптеры](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_adapters.jpg) {.illustration}

### Очереди и стек

Класс [std::queue](https://en.cppreference.com/w/cpp/container/queue) — это структура данных [очередь](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)), в которой элементы добавляются с одного конца, а удаляются с другого.

```c++ {.example_for_playground .example_for_playground_023}
std::queue<int> orders;

orders.push(152);
orders.push(201);
orders.push(8);

orders.pop();

std::println("{} {}", orders.front(), orders.back());
```
```
201 8
```

Очередь может быть организована поверх нескольких последовательных контейнеров. По умолчанию она использует `std::deque`, но с помощью аргумента шаблона дек можно заменить на список:

```c++
std::queue<int, std::list<int>> orders;
```

Класс [std::priority_queue](https://en.cppreference.com/w/cpp/container/priority_queue) — это [очередь с приоритетами](https://ru.wikipedia.org/wiki/%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C_%D1%81_%D0%BF%D1%80%D0%B8%D0%BE%D1%80%D0%B8%D1%82%D0%B5%D1%82%D0%BE%D0%BC_(%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5)). Она поддерживает две операции:
- Добавить элемент.
- Извлечь элемент с максимальным приоритетом.

Приоритет элемента определяется оператором `<`, но это можно переопределить.

Очередь с приоритетами реализуется через [кучу (heap)](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) поверх массива.

Заведем очередь с приоритетами из задач на исполнение. Задача — это пара из приоритета типа `int` и названия типа `std::string`. Опертор `<` вначале сравнивает поля `first` двух объектов типа `std::pair`, затем — поля `second`. Для простой демонстрации работы контейнера нас это устраивает. 

```c++ {.example_for_playground}
import std;

void execute_task(std::pair<int, std::string> task)
{
    // https://en.cppreference.com/w/cpp/utility/format/spec
    std::println("[{:>5}] {}", task.first, task.second);
}

void process_tasks(std::vector<std::pair<int, std::string>> tasks)
{
    // Как и у всех контейнеров и адаптеров, у priority_queue
    // есть перегрузка конструктора, принимающая итераторы на
    // диапазон элементов нужного типа:
    std::priority_queue<std::pair<int, std::string>> queue = {
                                                    tasks.begin(),
                                                    tasks.end()
                                                    };
    while(queue.size() > 0)
    {
        execute_task(queue.top());
        queue.pop();
    }
}

int main()
{
    process_tasks({
        {-1, "free unused resources"},
        {100, "preprocess data 2"},
        {20, "merge results"},
        {40, "process data 2"},
        {-1000, "shutdown"},
        {-2, "free unused resources"},
        {41, "process data 1"},
        {1000, "initialize system"},
        {100, "preprocess data 1"},
    });
}
```
```
[ 1000] initialize system
[  100] preprocess data 2
[  100] preprocess data 1
[   41] process data 1
[   40] process data 2
[   20] merge results
[   -1] free unused resources
[   -2] free unused resources
[-1000] shutdown
```

Класс [std::stack](https://en.cppreference.com/w/cpp/container/stack) реализует [структуру данных стек](https://ru.wikipedia.org/wiki/%D0%A1%D1%82%D0%B5%D0%BA), в которой элементы добавляются и удаляются только с одного конца.

### flat-версии ассоциативных контейнеров

Упорядоченные ассоциативные контейнеры оптимизированы для смешанных операций вставки, поиска и удаления. Очередность операций произвольна: вслед за добавлением элемента может идти еще одно добавление, удаление или поиск. Так работает кеширование.

Однако распространен и другой сценарий:
1. Вначале контейнер заполняется элементами.
2. Затем по ним осуществляется поиск.

Например, на старте сервиса подгружаются данные, а затем по ним требуется только поиск. Чем более скученно данные лежат в памяти, тем быстрее по ним поиск, потому что кеш процессора задействуется эффективнее. Класс `std::vector` — самый дружелюбный к кешу динамически расширяемый контейнер. Под него выделяется единая область памяти. На втором месте — хранящийся в памяти кусочно-непрерывно `std::deque`. 

Поверх этих двух классов и реализованы `flat`-версии контейнеров `map` и `set`: [std::flat_map](https://en.cppreference.com/w/cpp/container/flat_map), [std::flat_set](https://en.cppreference.com/w/cpp/container/flat_set), [std::flat_multimap](https://en.cppreference.com/w/cpp/container/flat_multimap), [std::flat_multiset](https://en.cppreference.com/w/cpp/container/flat_multiset).

Данные в них хранятся отсортированными, как и в упорядоченных ассоциативных контейнерах. А поиск работает быстрее, хотя формально остается логарифмическим `O(log(N))`. Как и очередь с приоритетами, flat-контейнеры реализованы через [кучу](https://ru.wikipedia.org/wiki/%D0%9A%D1%83%D1%87%D0%B0_(%D1%81%D1%82%D1%80%D1%83%D0%BA%D1%82%D1%83%D1%80%D0%B0_%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85)) поверх массива.

Интерфейсы flat-версий контейнеров не отличаются от интерфейсов ассоциативных контейнеров.

### Алгоритмическая сложность работы с адаптерами

![Алгоритмическая сложность работы с адаптерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/container_adapters_algo_complexity.jpg) {.illustration}

Требуется добавлять и удалять элементы с обоих концов контейнера. Других операций не планируется. Какой контейнер или адаптер подойдет для этого лучше всего? {.task_text}

Напишите полное название вместе с неймспейсом. {.task_text}

Если вам интересен практический пример похожего сценария, то откройте подсказку. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0070_task_0070}
```
[Work stealing](https://en.wikipedia.org/wiki/Work_stealing) — это известная стратегия балансировки параллельных вычислений. Она описывает, как распределять потоки выполнения по ядрам процессора. Стратегия получила свое название из-за того, что простаивающие ядра перехватывают часть работы у загруженных. Каждому ядру соотносится дек потоков. Потоки могут удаляться с обоих концов дека. Стратегия work stealing реализована в асинхронном рантайме [Tokio](https://tokio.rs/) для Rust, фреймворке [Fork/Join](https://docs.oracle.com/javase/tutorial/essential/concurrency/forkjoin.html) для Java и библиотеке [Task Parallel Library](https://learn.microsoft.com/en-us/dotnet/standard/parallel-programming/task-parallel-library-tpl) для .NET. {.task_hint}
```cpp {.task_answer}
std::deque
```

Требуется хранить список задач для исполнения в тред-пуле. Когда поток заканчивает задачу, он переходит к следующей с максимальным приоритетом. Задачи попадают в этот список с уже указанным приоритетом. Какой контейнер или адаптер подойдет лучше всего? {.task_text}

Напишите полное название вместе с неймспейсом. {.task_text}

```consoleoutput {.task_source #cpp_chapter_0070_task_0080}
```
Лучше всего подойдет очередь с приоритетами. {.task_hint}
```cpp {.task_answer}
std::priority_queue
```

----------

## Резюме

В качестве резюме этой главы отлично подходит таблица контейнеров, адаптеров и их алгоритмической сложности. Держите ее под рукой, если будете готовиться к собеседованиям.

![Алгоритмическая сложность работы с контейнерами и адаптерами](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/containers_algo_complexity_senjun_ru.jpg) {.illustration}