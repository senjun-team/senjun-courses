# Глава 12. Автоматизация сборки программы

Понимание, [как устроена](/courses/cpp/chapters/cpp_chapter_0110/) компиляция программы, поможет вам работать со сложными проектами и диагностировать ошибки сборки. Однако на практике собирать проект прямым вызовом компилятора непрактично. Вместо этого используют одну из систем автоматизации сборки.

За свою 40-летнюю историю С++ так и не обзавелся стандартной системой сборки, но среди многообразия популярных инструментов [де-факто лидирует](https://www.jetbrains.com/lp/devecosystem-2023/cpp/#cpp_projectmodels_two_years) CMake. С одной стороны, изучение CMake несколько выходит за рамки курса по C++. Но с другой стороны, было бы странно погружаться в дебри языка и не уметь создавать и собирать проекты, состоящие более чем из пары-тройки файлов. Поэтому мы кратко разберемся, что такое CMake и как он вам поможет со сборкой.

![Лого CMake](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-12/illustrations/cpp/cmake.png) {.illustration}

Как и в прошлой главе, в этой не будет задач. Вместо них вы самостоятельно поэкспериментируете со сборкой. Вы можете воспользоваться для этого нашим [Docker-образом.](/courses/cpp/chapters/cpp_chapter_0110/#block-docker-image)

## Что такое CMake

[CMake](https://cmake.org/) (Cross-platform Make) — это система для автоматизации компиляции, пакетирования и установки. CMake не занимается сборкой напрямую. Вместо этого он генерирует необходимые файлы для другого инструмента и вызывает его. CMake умеет работать поверх:
- систем сборки, таких как [Make](https://www.gnu.org/software/make/) и [Ninja](https://ninja-build.org/).
- IDE [Microsoft Visual Studio](https://learn.microsoft.com/en-us/cpp/build/cmake-projects-in-visual-studio?view=msvc-170) и [Apple Xcode.](https://cmake.org/cmake/help/latest/generator/Xcode.html) Для них он создает необходимые проектные файлы.

Если ваш проект собирается через CMake, вам будет удобно работать с ним из любой распространенной IDE для C++, начиная с [Qt Creator](https://doc.qt.io/qtcreator/creator-how-to-install.html) и заканчивая [Visual Studio Code.](https://code.visualstudio.com/docs/languages/cpp)

CMake решает две основные задачи:
- Кроссплатформенная сборка. Она выгодно отличает CMake от таких проприетарных инструментов как Microsoft Visual Studio и Apple Xcode.
- Упрощение управления проектом по сравнению с более старыми инструментами вроде [GNU Autotools.](https://ru.wikipedia.org/wiki/Autotools)

CMake включает три консольных инструмента:
- `cmake` для сборки,
- `ctest` для запуска тестов,
- `cpack` для пакетирования и создания инсталлятора.

## Команды CMake {#block-commands}

CMake читает файлы конфигурации CMakeLists.txt, в которых перечисляются команды на макроязыке CMake.

За пару десятилетий эволюции макроязык CMake адаптировался под нужды индустрии. По аналогии с «Modern C++» в обиход вошел термин [«Modern CMake».](https://cliutils.gitlab.io/modern-cmake/README.html) Он относится к CMake версии 3.15 и выше, начиная с которой файлы CMakeLists.txt становятся все более удобными и читабельными. А поддержка C++ модулей появилась в CMake 3.28.

Допустим, у нас есть простейший проект, состоящий из двух файлов:

```
├── main.cpp
└── CMakeLists.txt
```

Рассмотрим, как выглядит `CMakeLists.txt`, описывающий получение бинаря с именем `run` из `main.cpp`. Считаем, что в `main.cpp` не импортируются никакие модули.

```
cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(hello LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")

add_executable(run main.cpp)
```

С символа `#` в макроязыке CMake начинаются однострочные комментарии.

Команда [cmake_minimum_required](https://cmake.org/cmake/help/latest/command/cmake_minimum_required.html) задает минимально необходимую версию CMake, без которой не получится собрать проект.

Команда [project](https://cmake.org/cmake/help/latest/command/project.html) задает имя проекта и сохраняет его в переменной `PROJECT_NAME`. Например, это удобно для задания имени в одном месте и переиспользования в именах артефактов сборки, таких как библиотеки и исполняемые файлы.

Команда [set](https://cmake.org/cmake/help/latest/command/set.html) задает значение для переменной. Если имя переменной начинается с префикса `CMAKE_`, это означает, что перед вами [специальная переменная,](https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html) используемая самой утилитой CMake. Мы задали три таких переменных:
- `CMAKE_CXX_STANDARD`. На основании значения этой переменной CMake передает компилятору специфичную для него опцию, конкретизирующую, с каким стандартом C++ собирать проект.
- `CMAKE_CXX_STANDARD_REQUIRED` флаг для завершения сборки с ошибкой, если значение `CMAKE_CXX_STANDARD` не задано.
- `CMAKE_CXX_FLAGS` — опции компилятора. Мы присвоили этой переменной строковое значение `"${CMAKE_CXX_FLAGS} -O3"`. Здесь переменная `CMAKE_CXX_FLAGS` обернута в конструкцию `${}`. Это необходимо для получения значения переменной.

И, наконец, команда [add_executable](https://cmake.org/cmake/help/latest/command/add_executable.html) добавляет цель сборки — исполняемый файл. Первым аргументом команды идет имя исполняемого файла, а затем разделенные пробелом файлы. 

По любой из команд вы можете посмотреть [подробное описание.](https://cmake.org/cmake/help/latest/index.html#command-line-tools) Для этого воспользуйтесь поиском по документации.

## Процесс сборки через CMake

При сборке любого проекта команду `cmake` требуется вызвать дважды с разными опциями. Сначала для создания файлов сборки, затем — для самой сборки.

Файлы сборочной системы и артефакты сборки принято хранить в отдельной директории, чтобы не замусоривать проект. Зачастую это директория `build` внутри проекта.Чтобы создать в ней файлы сборки, `cmake` вызывается с опцией `-B`.

```bash
cmake -B build/
```

Если на этом этапе явно не указать систему сборки, CMake сгенерирует файлы для Make.

После того как файлы сборки готовы, можно компилировать проект. Для этого `cmake` вызывается с опцией `--build`.

```bash
cmake --build build/
```

Если компиляция завершается успешно, на жесткий диск сохраняются ее артефакты — исполняемые файлы и библиотеки. По умолчанию они также находятся в директории `build`. 

Вызовите `cmake` без аргументов, чтобы посмотреть краткую справку. А вызов `cmake --help` подскажет, какие опции есть у `cmake`. Например, вы можете узнать, для чего нужна опция `-S`.

### Сборка простого проекта с хедерами

Создайте директорию `hello_compiler` для одноименного проекта, сохраните в ней [один хедер и два cpp-файла.](/courses/cpp/chapters/cpp_chapter_0100/#block-hello-compiler) Добавьте CMakeLists.txt:

```
├── hello_compiler.h
├── hello_compiler.cpp
├── main.cpp
└── CMakeLists.txt
```

Содержимое CMakeLists.txt выглядит так:

```
cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(hello LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Символ \ нужен, чтобы разбить длинную строку на несколько
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ \
                    -Werror -Wall -Wno-unused-variable \
                    -Wno-logical-op-parentheses -O3")

add_executable(main main.cpp hello_compiler.cpp)
```

Теперь соберите проект:

```bash
cmake -B build/
cmake --build build/
```

Если сборка прошла успешно, можно запустить бинарь:

```bash
./build/main
```

### Сборка простого проекта с модулями

Теперь соберем [вариант проекта](/courses/cpp/chapters/cpp_chapter_0100/#block-project-modules) `hello_compiler`, содержащий пользовательский модуль и импортирующий `std`. Добавьте в проект CMakeLists.txt:

```
hello_compiler
├── hello_compiler.cppm
├── main
└── CMakeLists.txt
```

Обратите внимание на команды в CMakeLists.txt, необходимые для работы с модулями. Считаем, что [BMI](/courses/cpp/chapters/cpp_chapter_0110/#block-bmi) модуля стандартной библиотеки уже хранится в `/usr/local/lib/`.

```
cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(hello LANGUAGES CXX)

# Если в системе установлено несколько компиляторов,
# мы выбираем именно clang++. Если он не найден, 
# проект не соберется
set(CMAKE_CXX_COMPILER clang++)

# Запрет на использование специфичных для компилятора
# расширений
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Установка переменной STD_MODULE_FILE равной
# пути к BMI модуля std
set(STD_MODULE_FILE /usr/local/lib/std.pcm)

# Среди опций для компилятора передаем значение переменной
# STD_MODULE_FILE
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -O3 \
                    -fmodule-file=std=${STD_MODULE_FILE}")

# Добавление цели сборки - бинарного файла main. Перечисление
# необходимых для этого cpp-файлов и модулей будет выполнено
# отдельной командой target_sources
add_executable(main)

target_sources(main
    PRIVATE main.cpp
    PRIVATE FILE_SET hello_modules TYPE CXX_MODULES FILES hello_compiler.cppm)
```

Команда [target_sources](https://cmake.org/cmake/help/latest/command/target_sources.html) определяет, какие файлы использовать для сборки цели. В ней мы перечислили, какие `cpp`-файлы и пользовательские модули потребуются для компиляции `main`. 

Соберем проект. Для этого опцией `-G` явно зададим систему сборки `Ninja` вместо `Make`. На данный момент в `Make` не реализована полная поддержка модулей.

```bash
cmake -B build -G Ninja
cmake --build build/
./build/main
```

### Подпроекты

Допустим, вы пишете проект для проигрывания аудио-файлов, который компилируется в динамическую библиотеку. Но библиотека — не единственная цель сборки. Помимо нее есть бинарные файлы юнит-тестов и примеров работы с аудио. Все они подключают хедеры библиотеки и линкуются с ней.

Структура такого проекта может выглядеть следующим образом:

```
audio/
├── include
│   └── audio
│       └── play.h
├── src
│   └── play.cpp
├── examples
│   ├── play_mp3_single_file.cpp
│   └── ...
└── test
    ├── test_flac.cpp
    └── ...
```

В директории `include` хранятся хедеры, которые пользователи библиотеки должны подключать в свой код:

```c++
#include "audio/play.h"
```

В директории `src` лежит реализация этих хедеров. А `test` и `examples` содержат тесты и примеры кода.

Для сборки такого проекта через `cmake` можно завести не один, а три файла `CMakeLists.txt`:

```
audio/
├── CMakeLists.txt
├── include
│   └── audio
│       └── play.h
├── src
│   └── play.cpp
├── examples
│   ├── CMakeLists.txt
│   ├── play_mp3_file.cpp
│   └── ...
└── test
    ├── CMakeLists.txt
    ├── test_flac.cpp
    └── ...
```

Файл `CMakeLists.txt` в корне проекта собирает [динамическую](/courses/cpp/chapters/cpp_chapter_0110/#block-dynamic-libs) (shared) библиотеку `audio` и добавляет _подпроекты:_

```
...
include_directories(include)
...
add_library(audio SHARED src/play.cpp)
...
add_subdirectory(examples)
add_subdirectory(test)
```

Команда [include_directories](https://cmake.org/cmake/help/latest/command/include_directories.html) добавляет заданную директорию к путям, по которым компилятор ищет хедеры для всего проекта. Это нужно, чтобы препроцессор смог найти хедер `play.h`. Если вы хотите задать путь для конкретной цели сборки, вместо этой команды используйте [target_include_directories](https://cmake.org/cmake/help/latest/command/target_include_directories.html).

Команда [add_library](https://cmake.org/cmake/help/latest/command/add_library.html) добавляет цель сборки — библиотеку.

Команда [add_subdirectory](https://cmake.org/cmake/help/latest/command/add_subdirectory.html) обозначает, что в указанной директории содержится подпроект с файлом `CMakeLists.txt`, команды из которого тоже необходимо выполнить. Например, в `examples/CMakeLists.txt` могут быть такие команды:

```
...

add_executable(play_mp3_file play_mp3_file.cpp)

target_link_libraries(play_mp3_file PRIVATE audio)
...
```

Команда [target_link_libraries](https://cmake.org/cmake/help/latest/command/target_link_libraries.html) линкует цель сборки с набором библиотек.

Подпроекты в `cmake` нужны, чтобы структурировать скрипты сборки в соответствии с логической организацией проекта. Без подпроектов все команды для сборки пришлось бы хранить в одном-единственном огромном файле `CMakeLists.txt`, который было бы крайне тяжело читать и изменять.

## Что использовать вместо CMake

Несмотря на удобство Modern CMake, у CMake есть конкуренты:
- [SCons](https://scons.org/) — самодостаточная система cборки, написанная на Python.
- [Gradle](https://docs.gradle.org/current/userguide/building_cpp_projects.html) — выходец из мира Java, работает поверх JVM.
- [Meson](https://mesonbuild.com/) — как и CMake, не занимается сборкой напрямую. Вместо Make по умолчанию использует [Ninja.](https://ninja-build.org/)
- [Basel](https://bazel.build/start/cpp) — мультиязычная система сборки от Google.

![Лого SCons, Gradle, Meson, Basel](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-12/illustrations/cpp/meson_etc__logo.png) {.illustration}

## Пакетные менеджеры

Почти всегда в коммерческой разработке требуется автоматизация не только сборки, но и сопутствующих ей процессов:
- разрешения зависимостей проекта,
- версионирования,
- пакетирования,
- доставки пакета в репозиторий.

Эти задачи решают пакетные менеджеры. В мире C++ два самых распространенных пакетных менеджера — это [Conan](https://docs.conan.io/2/tutorial.html) и [vcpkg.](https://learn.microsoft.com/en-us/vcpkg/) Оба легко встраиваются в CI/CD и умеют работать в связке с популярными системами автоматизации сборки.

![Лого Conan и vcpkg](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-12/illustrations/cpp/packaging.png) {.illustration}

Можно ли жить без пакетных менеджеров? Вполне, хоть это и менее удобно. В таком случае разрешение зависимостей чаще всего организуется с помощью [git-сабмодулей](https://git-scm.com/book/en/v2/Git-Tools-Submodules) либо управления внешним проектом через функционал модулей [ExternalProject](https://cmake.org/cmake/help/latest/module/ExternalProject.html) и [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) в CMake. Пакетирование при этом организуется стандартными средствами для создания rpm или deb пакетов. А версионирование и заливка пакета в репозиторий делегируется продуктам, отвечающим за CI/CD.

## Домашнее задание

Поизучайте различные варианты CMakeLists.txt в опенсорсных проектах, например [GoogleTest,](https://github.com/google/googletest) [Boost](https://github.com/boostorg/boost) и [Nlohmann Json.](https://github.com/nlohmann/json)

Посмотрите, как организована структура четырех проектов с хедерами и модулями. Соберите их.
- С хедерами: [leveldb,](https://github.com/google/leveldb/tree/main) [libtorrent.](https://github.com/rakshasa/libtorrent/tree/master)
- С модулями: [infinity,](https://github.com/infiniflow/infinity/tree/main) [BS::thread_pool.](https://github.com/bshoshany/thread-pool)

-----

## Резюме

- CMake — это система для автоматизации компиляции, пакетирования и установки.
- CMake не занимается сборкой напрямую. Он генерирует необходимые файлы для другого инструмента и вызывает его.
- CMake читает файлы конфигурации CMakeLists.txt, в которых перечисляются команды на макроязыке CMake.
- Сборка проекта через CMake состоит из двух этапов: создания файлов сборки и непосредственно компиляции проекта.