# Подсказка

## Класс LRUCache

Реализуем класс `LRUCache` с помощью двусвязного списка `std::list` и хеш-таблицы `std::unordered_map`. 

Двусвязный список хранит пары из ключей и значений, которые добавляются в кеш методом `put()`. Пара - это `std::pair<int, std::string>`. Значит, объявление приватного поля класса для списка может выглядеть так:

```cpp
std::list<std::pair<int, std::string>> m_list;
```

Хеш-таблица хранит ключи элементов и итераторы на соответствующие им элементы списка. Итератор на элемент списка имеет тип `std::list<std::pair<int, std::string>>::iterator`. Значит, объявление приватного поля класса для хеш-таблицы может быть таким:

```cpp
std::unordered_map<int, std::list<std::pair<int, std::string>>::iterator> m_map;
```

Остается реализовать методы `get()` и `put()`, внутри которых предусмотрены все возможные ситуации: добавление элемента с уже существующим ключом, удаление из кэша самого старого элемента и т.д.

## Сборка проекта

### Релизная сборка

Чтобы проект собрался как релиз, нужно задать переменную [CMAKE_BUILD_TYPE](https://cmake.org/cmake/help/latest/variable/CMAKE_BUILD_TYPE.html):

```
set(CMAKE_BUILD_TYPE Release)
```

### Подключение GTest

По условию задания библиотека для юнит-тестов GoogleTest _установлена системно._ Значит, вначале нужно ее найти командой [find_package](https://cmake.org/cmake/help/latest/module/FindGTest.html):

```
find_package(GTest REQUIRED)
```

А затем — слинковать с тестами командой [target_link_libraries](https://cmake.org/cmake/help/latest/command/target_link_libraries.html):

```
target_link_libraries(tests PRIVATE lru_cache GTest::gtest)
```

### Подключение Google benchmarks

По условию задания исходный код библиотеки Google benchmarks скачан в локальную директорию `/third_party/google/benchmark/`. Значит, ее нужно подключить в проект с помощью модуля `cmake` [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html):

```
# Подключаем модуль FetchContent
include(FetchContent)

# Объявляем зависимость проекта с именем google_benchmark, хранящуюся
# локально в указанной директории
FetchContent_Declare(
    google_benchmark
    SOURCE_DIR "/third_party/google/benchmark/"
)

# Подключение зависимости
FetchContent_MakeAvailable(google_benchmark)
```

После того как зависимость подключена, с библиотекой для бенчмарков можно линковаться:

```
target_link_libraries(main PRIVATE lru_cache benchmark)
```

### Сборка библиотеки lru_cache

Чтобы собрать динамическую библиотеку, в `lru_cache/CMakeLists.txt` нужно добавить цель для сборки — разделяемую (shared) библиотеку `lru_cache`:

```
add_library(lru_cache SHARED lru_cache.cpp)
```

Чтобы эта команда из файла `lru_cache/CMakeLists.txt` была выполнена, нужно обозначить директорию `lru_cache` как подпроект в корневом файле `CMakeLists.txt`:

```
add_subdirectory(lru_cache)
```

Так как хедер библиотеки `lru_cache.h` подключен в бенчмарки и юнит-тесты, компилятору нужно указать путь к нему:

```
include_directories(lru_cache)
```

И, наконец, с библиотекой нужно слинковать все цели сборки, которые ее используют.

```
target_link_libraries(цель_сборки PRIVATE lru_cache)
```