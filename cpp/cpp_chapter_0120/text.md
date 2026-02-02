# Глава 12. Автоматизация сборки программы

Понимание, [как устроена](/courses/cpp/chapters/cpp_chapter_0111/) компиляция программы, поможет вам работать со сложными проектами и диагностировать ошибки сборки. Однако на практике собирать проект прямым вызовом компилятора непрактично. Вместо этого подключают одну из систем автоматизации сборки.

За свою 40-летнюю историю С++ так и не обзавёлся стандартной системой сборки, но среди многообразия популярных инструментов [де-факто лидирует](https://www.jetbrains.com/lp/devecosystem-2023/cpp/#cpp_projectmodels_two_years) CMake. С одной стороны, изучение CMake несколько выходит за рамки курса по C++. Но с другой стороны, было бы странно погружаться в дебри языка и не уметь собирать проекты, состоящие более чем из пары-тройки файлов. Поэтому мы кратко разберёмся, что такое CMake и как он упрощает сборку.


![Лого CMake](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/main/illustrations/cpp/cmake.png) {.illustration}


Как и в прошлой главе, в этой не будет задач. Вместо них вы самостоятельно поэкспериментируете со сборкой. Вы можете воспользоваться для этого нашим [Docker-образом.](/courses/cpp/chapters/cpp_chapter_0111/#block-docker-image)

## Что такое CMake

[CMake](https://cmake.org/) (Cross-platform Make) — это система для автоматизации компиляции, пакетирования и установки. CMake не занимается сборкой напрямую. Вместо этого он создаёт необходимые файлы для другого инструмента и вызывает его. Этот инструмент называется _генератором._ CMake умеет работать поверх:
- систем сборки, таких как [Make](https://www.gnu.org/software/make/) и [Ninja](https://ninja-build.org/).
- IDE, например [Microsoft Visual Studio](https://learn.microsoft.com/en-us/cpp/build/cmake-projects-in-visual-studio?view=msvc-170) и [Apple Xcode.](https://cmake.org/cmake/help/latest/generator/Xcode.html) Для них он создаёт необходимые проектные файлы.

Если ваш проект собирается через CMake, вам будет удобно работать с ним из любой распространённой IDE для C++, начиная с [Qt Creator](https://doc.qt.io/qtcreator/creator-how-to-install.html) и заканчивая [Visual Studio Code.](https://code.visualstudio.com/docs/languages/cpp)

CMake решает две основные задачи:
- Кроссплатформенная сборка. Она выгодно отличает CMake от таких проприетарных инструментов как Microsoft Visual Studio и Apple Xcode.
- Упрощение управления проектом по сравнению с более старыми инструментами вроде [GNU Autotools.](https://ru.wikipedia.org/wiki/Autotools)

CMake включает три консольных инструмента:
- `cmake` для сборки,
- `ctest` для запуска тестов,
- `cpack` для пакетирования и создания инсталлятора.

## Команды CMake {#block-commands}

CMake читает файлы конфигурации CMakeLists.txt, в которых перечисляются команды на макроязыке CMake. Этот макроязык является тьюринг-полным: с его помощью можно решить любую вычислимую задачу, в том числе управлять процессом сборки самых сложных проектов. Для этого в синтаксисе языка есть все необходимое: переменные, условия, циклы и даже функции.

За пару десятилетий эволюции макроязык CMake адаптировался под нужды индустрии. По аналогии с «Modern C++» в обиход вошёл термин [«Modern CMake».](https://cliutils.gitlab.io/modern-cmake/README.html) Он относится к CMake версии 3.15 и выше, начиная с которой файлы CMakeLists.txt становятся все более удобными и читабельными. В CMake 3.28 появилась поддержка C++ модулей. 

Допустим, у нас есть простейший проект, состоящий из двух файлов:

```
demo/
├── main.cpp
└── CMakeLists.txt
```

Рассмотрим, как выглядит CMakeLists.txt, описывающий получение бинарника с именем `run` из `main.cpp`. Считаем, что в `main.cpp` не импортируются никакие модули.

```
cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(hello LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(run main.cpp)
```

С символа `#` в макроязыке CMake начинаются однострочные комментарии. Команды регистронезависимы, но [рекомендуется](https://cmake.org/cmake/help/latest/guide/tutorial/A%20Basic%20Starting%20Point.html#exercise-1-building-a-basic-project) использовать нижний регистр.

Команда [cmake_minimum_required](https://cmake.org/cmake/help/latest/command/cmake_minimum_required.html) должна идти первой. Она задаёт минимально необходимую версию CMake, без которой не получится собрать проект.

Команда [project](https://cmake.org/cmake/help/latest/command/project.html) задаёт имя проекта и сохраняет его в переменной `PROJECT_NAME`. Например, это удобно для переиспользования имени проекта в именах артефактов сборки, таких как библиотеки и исполняемые файлы.

Команда [set](https://cmake.org/cmake/help/latest/command/set.html) задаёт значение для переменной. Если имя переменной начинается с префикса `CMAKE_`, это означает, что перед вами [специальная переменная,](https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html) используемая самой утилитой CMake. Мы задали две таких переменных:
- `CMAKE_CXX_STANDARD`. На основании значения этой переменной CMake передаёт компилятору специфичную для него опцию, конкретизирующую, с каким стандартом C++ собирать проект.
- `CMAKE_CXX_STANDARD_REQUIRED` — флаг для завершения сборки с ошибкой, если значение `CMAKE_CXX_STANDARD` не задано.

Наконец, команда [add_executable](https://cmake.org/cmake/help/latest/command/add_executable.html) добавляет цель сборки — исполняемый файл. Первым аргументом команды идёт имя исполняемого файла, а затем разделённые пробелом файлы. 

По любой из команд вы можете посмотреть [подробное описание.](https://cmake.org/cmake/help/latest/index.html#command-line-tools) Для этого воспользуйтесь поиском по документации.

## Процесс сборки через CMake

При сборке проекта CMake работает с двумя директориями: деревом исходников и деревом сборки. Дерево исходников (source tree) — это корень вашего проекта с файлом CMakeLists.txt. А дерево сборки (build tree) — путь, по которому CMake создаёт кеш, файлы для генератора и, в конечном итоге, артефакты сборки. Эти два пути задаются опциями `-S` и `-B`: 

```
cmake [options] -S <path-to-source> -B <path-to-build>
```

Если любую из опций опустить, то значением по умолчанию будет текущая директория. Но хорошей практикой считается разграничение дерева исходников от дерева сборки. Такой подход называется [out-of-source build](https://cmake.org/cmake/help/latest/manual/cmake.1.html#introduction-to-cmake-buildsystems) и позволяет не захламлять директорию проекта файлами CMake.

Сборка проекта через CMake состоит из нескольких этапов:
- Конфигурация.
    - `cmake` читает файлы CMakeLists.txt.
    - Он анализирует команды и значения переменных. Проверяет окружение: доступность зависимостей, пути к требуемым библиотекам, версию компилятора и т.д.
    - По завершению конфигуации обновляется файл CMakeCache.txt. Он содержит все полученные на предыдущем шаге настройки.
- Генерация.
    - `cmake` создаёт файлы для выбранного генератора. Например, это Makefile для Мake и проектные файлы для MSVC.
    - Этап генерации нужен, чтобы транслировать команды на макроязыке CMake в файлы с инструкциями для конкретного генератора. 
- Сборка.
    - `cmake` вызывает генератор и передаёт ему созданные на предыдущем этапе файлы.
    - Генератор в свою очередь уже вызывает компилятор, чтобы скомпилировать проект на C++.

Этапы конфигурации и генерации нужно проходить только при первой сборке проекта либо при изменении файлов CMakeLists.txt. Они объединяются командой:

```bash
cmake -S demo/ -B build/
```

Здесь мы в качестве дерева сборки задали директорию с именем `build`. CMake создаст её и заполнит всем необходимым, в том числе файлами для генератора и результатами сборки.

Если не указать генератор, CMake создаст файлы для Make. Для явного задания генератора используется опция `-G`:  

```bash
cmake -S demo/ -B build/ -G Ninja
```

После успешного выполнения конфигурации и генерации `cmake` запускается непосредственно для сборки. Для этого ему передаётся опция `--build` и путь к дереву сборки. Обратите внимание, что опции `-B` и `--build` имеют разное предназначение. Не путайте их: `-B` явно указывает дерево сборки для этапов конфигурации и генерации, а `--build` стартует этап сборки. 

```bash
cmake --build build/
```

Если компиляция завершается успешно, на жёсткий диск сохраняются её артефакты — исполняемые файлы и библиотеки.

Вызовите `cmake` без аргументов, чтобы посмотреть краткую справку. А вызов `cmake --help` подскажет, какие опции есть у `cmake`.

### Сборка простого проекта с хедерами

Создайте директорию `hello_compiler` для одноимённого проекта, сохраните в ней [один хедер и два cpp-файла.](/courses/cpp/chapters/cpp_chapter_0102/#block-hello-compiler) Добавьте CMakeLists.txt:

```
hello_compiler
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
                    -Wno-logical-op-parentheses")

add_executable(main main.cpp hello_compiler.cpp)
```

Переменная `CMAKE_CXX_FLAGS` нужна для задания конкретных опций компилятора. Мы присвоили ей строковое значение `"${CMAKE_CXX_FLAGS} ..."`, чтобы добавить к _уже установленным_ опциям несколько дополнительных. Внутри строки переменная `CMAKE_CXX_FLAGS` обернута в конструкцию `${}`. Это необходимо для подстановки значения переменной, а не её имени. 

Опция `-stdlib=libc++` равносильна `-lc++`, с которой вы [уже знакомы.](/courses/cpp/chapters/cpp_chapter_0102/#block-opts) [-Werror](https://clang.llvm.org/docs/UsersManual.html#cmdoption-Werror) позволяет трактовать любые предупреждения компиляции как ошибки и завершать сборку. Самостоятельно разберитесь, для чего нужны опции `-Wall`, `-Wno-unused-variable` и `-Wno-logical-op-parentheses`. {#block-flags}

Теперь соберите проект:

```bash
cmake -S hello_compiler/ -B build/
cmake --build build/
```

Если сборка прошла успешно, можно запустить бинарник:

```bash
./build/main
```

### Сборка простого проекта с модулями

Теперь соберём [вариант проекта](/courses/cpp/chapters/cpp_chapter_0103/#block-project-modules) `hello_compiler`, содержащий пользовательский модуль и импортирующий `std`. Добавьте в проект CMakeLists.txt:

```
hello_compiler
├── hello_compiler.cppm
├── main
└── CMakeLists.txt
```

Обратите внимание на команды в CMakeLists.txt, необходимые для работы с модулями. Считаем, что [BMI](/courses/cpp/chapters/cpp_chapter_0112/#block-bmi) модуля стандартной библиотеки уже хранится в `/usr/local/lib/`.

```
cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(hello LANGUAGES CXX)

# Если в системе установлено несколько компиляторов,
# мы выбираем именно clang++. Если он не найден, 
# проект не соберётся
set(CMAKE_CXX_COMPILER clang++)

# Запрет на использование специфичных для компилятора
# расширений
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Установка переменной STD_MODULE_FILE равной
# пути к BMI модуля std
set(STD_MODULE_FILE /usr/local/lib/std.pcm)

# Среди опций для компилятора передаём значение переменной
# STD_MODULE_FILE
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ \
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
cmake -S hello_compiler/ -B build/ -G Ninja
cmake --build build/
./build/main
```

## Подпроекты {#block-subproject}

Допустим, вы пишете проект для проигрывания аудио-файлов, который компилируется в динамическую библиотеку. Но библиотека — не единственная цель сборки. Помимо неё есть бинарные файлы юнит-тестов и примеров работы с аудио. Все они подключают хедеры библиотеки и линкуются с ней.

Структура такого проекта может выглядеть следующим образом:

```
audio
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

```cpp
#include "audio/play.h"
```

В директории `src` лежит реализация этих хедеров. А `test` и `examples` содержат тесты и примеры кода.

Для сборки такого проекта через `cmake` можно завести не один, а три файла CMakeLists.txt:

```
audio
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

Файл CMakeLists.txt в корне проекта собирает [динамическую](/courses/cpp/chapters/cpp_chapter_0112/#block-dynamic-libs) (shared) библиотеку `audio` и добавляет _подпроекты:_

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

Команда [add_subdirectory](https://cmake.org/cmake/help/latest/command/add_subdirectory.html) обозначает, что в указанной директории содержится подпроект с файлом CMakeLists.txt, команды из которого тоже необходимо выполнить. Например, в `examples/CMakeLists.txt` могут быть такие команды:

```
...

add_executable(play_mp3_file play_mp3_file.cpp)

target_link_libraries(play_mp3_file PRIVATE audio)
...
```

Команда [target_link_libraries](https://cmake.org/cmake/help/latest/command/target_link_libraries.html) линкует цель сборки с набором библиотек.

Подпроекты в `cmake` нужны, чтобы структурировать скрипты сборки в соответствии с логической организацией проекта. Без подпроектов все команды для сборки пришлось бы хранить в одном-единственном огромном файле CMakeLists.txt, который было бы крайне тяжело читать и изменять.

## Типы сборок

[Как вы помните,](/courses/cpp/chapters/cpp_chapter_0111/#block-optimizations) у компиляторов есть опции, указывающие, насколько активно нужно оптимизировать код: `-O0`, `-O1` и другие. `-O0` означает отсутствие оптимизиаций, а `-O3` — применение полного набора.

Также у компиляторов есть опция `-g`, предназначенная для сохранения отладочных символов (debug info). Они добавляются прямо в бинарник или сохраняются отдельным файлом, чтобы у отладчика была возможность связать бинарный код с исходным кодом. Это нужно для отображения имён переменных и функций, стека вызовов и других полезных при отладке вещей.

Комбинация из этих и некоторых других опций компилятора описывает _тип сборки_. В CMake он задатся переменной `CMAKE_BUILD_TYPE`. Она принимает значения `Debug`, `Release`, `RelWithDebInfo` и [другие.](https://cmake.org/cmake/help/latest/variable/CMAKE_BUILD_TYPE.html)

```
set(CMAKE_BUILD_TYPE Release)
```

От значения `CMAKE_BUILD_TYPE` зависит набор опций _по умолчанию,_ передаваемых компилятору. При релизной сборке как правило передаётся `-O3`, при отладочной — `-O0 -g`, а при релизной сборке с отладочными символами `RelWithDebInfo` — `-O2 -g`.

Переменная `CMAKE_CXX_FLAGS` лишь _дополняет_ этот набор опций, но не переопределяет. Если переменная `CMAKE_BUILD_TYPE` не задана явно, то большинство генераторов создают отладочную сборку.

Фиксировать значение `CMAKE_BUILD_TYPE` внутри CMakeLists.txt — не самый гибкий подход. Ведь всякий раз для смены типа сборки придётся редактировать эту переменную в коде скрипта. К счастью, есть более удобные способы присвоить ей необходимое значение.

## Варианты установки переменных

У команды `set` для задания переменной есть альтернативы: опция `-D` и переменные окружения.

[Опция](https://cmake.org/cmake/help/latest/manual/cmake.1.html#cmdoption-cmake-D) `-D` консольной команды `cmake` позволяет на этапе конфигурации определить все необходимые переменные.

Формат: `-D <var>=<value>`. Пробел между `-D` и именем переменной чаще всего опускают:

```bash
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_STANDARD=23 -S demo/ -B build/
```

[Некоторые](https://cmake.org/cmake/help/latest/manual/cmake-env-variables.7.html) переменные можно передавать в CMake через окружение. Переменная окружения должна быть задана до вызова `cmake`:

```bash
CXX=clang++ CMAKE_BUILD_TYPE=RelWithDebInfo cmake -S demo/ -B build/
```

## Вывод информации в консоль

Когда вы начнёте вносить изменения в файлы CMakeLists.txt реальных проектов, то наверняка столкнётесь с трудностями. Вместо релизной сборки будет внезапно собираться отладочная, компилятор не сможет найти пути к нужным хедерам, а линковщик — к библиотекам. Чтобы локализовать и исправить проблему, выводите в консоль всю полезную информацию. Используйте для этого команду [message](https://cmake.org/cmake/help/latest/command/message.html):

```
message("Here goes your text")
```

Чтобы вывести значение переменной, не забудьте обрамить её символами `${}`:

```
message("Build type: ${CMAKE_BUILD_TYPE}")
```

Перед текстом можно указать тип сообщения, например `WARNING` или `FATAL_ERROR`. После вывода сообщения с типом `FATAL_ERROR` CMake сразу же завершает сборку.

```
message(STATUS "Flags for compiler: ${CMAKE_CXX_FLAGS}")
```

Про поддерживаемые типы сообщений вы можете почитать [здесь.](https://cmake.org/cmake/help/latest/command/message.html#general-messages) 

Задание: установите значение переменной `CMAKE_BUILD_TYPE` тремя возможными способами — командой `set`, опцией `-D` и через переменную окружения. В CMakeLists.txt выведите значение `CMAKE_BUILD_TYPE` в консоль. Что будет, если одновременно задать переменную окружения и опцию `-D` с разными значениями? Какое из них подхватит CMake?

## Работа с зависимостями

Практически любой серьёзный проект на C++ задействует внешние зависимости для решения типовых задач. В качестве зависимости может выступать что угодно, хоть файлы ресурсов с иконками или переводами текста на разные языки. Однако в большинстве случев речь идёт о C++ библиотеках. Они [бывают](/courses/cpp/chapters/cpp_chapter_0112/#block-libraries) статическими, динамическими или header-only.

Представим, что нужная библиотека уже собрана и присутствует в системе. Например, если она входит в состав пакета, установленного пакетным менеджером. В таком случае для обнаружения хедеров и бинарных файлов библиотеки в CMakeLists.txt прописывается команда `find_package`. Вам остаётся только убедиться, что компилятор находит её хедеры, и слинковаться с бинарными файлами библиотеки.

Более интересный сценарий: доступен только исходный код библиотеки, и её предстоит собрать. Допустим, в проекте используется [git,](https://git-scm.com/) а нужная библиотека лежит в git-репозитории. Тогда её можно подгрузить в проект в качестве [git-сабмодуля](https://git-scm.com/book/en/v2/Git-Tools-Submodules) (git-submodule). Исходный код библиотеки станет частью проекта, например попадёт в директорию `third_party/library_name`. В CMakeLists.txt работа с такой библиотекой ничем не будет отличаться от работы с любым другим [подпроектом.](/courses/cpp/chapters/cpp_chapter_0120/#block-subproject)

У git-сабмодулей есть альтернативы: модули CMake для работы с зависимостями. [Модуль](https://cmake.org/cmake/help/book/mastering-cmake/chapter/Modules.html) (module) в CMake — это набор команд на макроязыке CMake, вынесенных в отдельный файл. Чтобы скачивать и собирать зависимости проекта, доступны модули [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) и [ExternalProject](https://cmake.org/cmake/help/latest/module/ExternalProject.html).

Рассмотрим варианты подключения зависимостей, не касающиеся git.

### Подключение библиотек, которые установлены в системе

Для поиска зависимостей, установленных системно, в CMake предусмотрена команда [find_package](https://cmake.org/cmake/help/latest/command/find_package.html). У неё десятки параметров, но обязательный только один — имя пакета. 

Так выглядит поиск библиотеки [fmt](https://github.com/fmtlib/fmt). Она послужила фундаментом для реализации стандартного форматирования строк `std::format` и популярна в проектах, не поддерживающих C++20:

```
find_package(fmt)
```

Помимо имени пакета вам могут потребоваться несколько других параметров:

```
find_package(<PackageName> [version] [EXACT] [REQUIRED|QUIET] [COMPONENTS <component1> ...])
```

- `version` — версия пакета.
- `EXACT` — указание, что требуется конкретная версия, а не минимально необходимая.
- Флаг `REQUIRED` прекращает сборку с ошибкой, если зависимость не обнаружена в системе. Флаг `QUIET` наоборот продолжает сборку, даже если пакет не найден.
- `COMPONENTS` — список требуемых компонентов пакета.

Так выглядит поиск компонентов `program_options` и `filesystem` пакета [Boost](/courses/cpp/chapters/cpp_chapter_0012/#block-boost):

```
find_package(Boost 1.89 REQUIRED COMPONENTS program_options filesystem)

# Делаем видимыми компилятору пути к хедерам
include_directories(${Boost_INCLUDE_DIRS})

add_executable(run main.cpp)

# Линкуем библиотеки к цели сборки
target_link_libraries(run ${Boost_LIBRARIES})
```

В этом примере команда `find_package` определила расположение компонентов Boost и установила переменные `Boost_INCLUDE_DIRS` и `Boost_LIBRARIES` в соответствующие значения. Чтобы подробнее разобраться, как это работает, советуем почитать [первые два раздела](https://cmake.org/cmake/help/book/mastering-cmake/chapter/Finding%20Packages.html) документации про поиск пакетов в CMake.

### Подключение библиотек через модуль CMake

В CMake есть два модуля, которые позволяют скачивать и собирать зависимости проекта: [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) и [ExternalProject](https://cmake.org/cmake/help/latest/module/ExternalProject.html). У них схожее назначение, но разные сценарии использования.

FetchContent работает исключительно с зависимостями, собираемыми через CMake. А ExternalProject подойдёт, даже если зависимость строится через другую систему автоматизации сборки (например, GNU autotools).

FetchContent подгружает и делает зависимость доступной _на этапе конфигурации._ Затем она собирается как часть основного проекта. ExternalProject скачивает и строит зависимость _на этапе сборки._ Построение при этом происходит максимально изолированно от основного проекта.

Модуль ExternalProject менее распространён: в большинстве случаев хватает функционала FetchContent. На нем и остановимся.

Представим, что у проекта две библиотеки-зависимости: [spdlog](https://github.com/gabime/spdlog) для логирования и [libunifex](https://github.com/facebookexperimental/libunifex) для асинхронного запуска задач. Тогда их подключение через FetchContent будет выглядеть так: {#block-fetchcontent-example}

```
# Подключаем модуль CMake FetchContent
include(FetchContent)

# Описываем зависимости
FetchContent_Declare(
  # Имя, которое мы даём зависимости:
  spdlog

  # URI
  GIT_REPOSITORY https://github.com/gabime/spdlog.git

  # Хеш коммита или git-тег:
  GIT_TAG        6fa36017cfd5731d617e1a934f0e5ea9c4445b13
)

FetchContent_Declare(
  libunifex
  GIT_REPOSITORY https://github.com/facebookexperimental/libunifex.git
  GIT_TAG        17fecea7fa3bf7345cb781fff5be33b05245ef90
)

# Получаем зависимости
FetchContent_MakeAvailable(spdlog libunifex)
```

Процесс подключения зависимости через FetchContent состоит из трёх шагов:
- [include(FetchContent)](https://cmake.org/cmake/help/latest/command/include.html) — подключение модуля `FetchContent`, в котором реализованы команды `FetchContent_Declare` и `FetchContent_MakeAvailable`.
- [FetchContent_Declare](https://cmake.org/cmake/help/latest/module/FetchContent.html#command:fetchcontent_declare) — описание зависимости и её характеристик.
- [FetchContent_MakeAvailable](https://cmake.org/cmake/help/latest/module/FetchContent.html#command:fetchcontent_makeavailable) - загрузка зависимости и определение целей для сборки из этой зависимости.

После того как зависимость подгружена, с ней можно линковаться командой [target_link_libraries](https://cmake.org/cmake/help/latest/command/target_link_libraries.html).

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

Все эти задачи решают пакетные менеджеры. В мире C++ два самых распространённых пакетных менеджера — это [Conan](https://docs.conan.io/2/tutorial.html) и [vcpkg.](https://learn.microsoft.com/en-us/vcpkg/) Оба легко встраиваются в CI/CD и умеют работать в связке с популярными системами автоматизации сборки.


![Лого Conan и vcpkg](https://raw.githubusercontent.com/senjun-team/senjun-courses/refs/heads/cpp-chapter-12/illustrations/cpp/packaging.png) {.illustration}


Можно ли жить без пакетных менеджеров? Вполне, хоть это и менее удобно. В таком случае разрешение зависимостей чаще всего организуется с помощью [git-сабмодулей](https://git-scm.com/book/en/v2/Git-Tools-Submodules) либо модули [ExternalProject](https://cmake.org/cmake/help/latest/module/ExternalProject.html) и [FetchContent](https://cmake.org/cmake/help/latest/module/FetchContent.html) в CMake. Пакетирование при этом организуется стандартными средствами для создания rpm или deb пакетов. А версионирование и заливка пакета в репозиторий делегируется продуктам, отвечающим за CI/CD.

## Домашнее задание

Поизучайте различные варианты CMakeLists.txt в опенсорсных проектах, например [GoogleTest,](https://github.com/google/googletest) [Boost](https://github.com/boostorg/boost) и [Nlohmann Json.](https://github.com/nlohmann/json)

Посмотрите, как организована структура четырёх проектов с хедерами и модулями. Соберите их.
- С хедерами: [leveldb,](https://github.com/google/leveldb/tree/main) [libtorrent.](https://github.com/rakshasa/libtorrent/tree/master)
- С модулями: [infinity,](https://github.com/infiniflow/infinity/tree/main) [BS::thread_pool.](https://github.com/bshoshany/thread-pool)

-----

## Резюме

- CMake — это система для автоматизации компиляции, пакетирования и установки.
- CMake не занимается сборкой напрямую. Он генерирует необходимые файлы для генератора — инструмента, занимающегося сборкой, и вызывает его.
- CMake читает файлы конфигурации CMakeLists.txt, в которых перечисляются команды на макроязыке CMake.
- Компиляция C++ проекта через CMake состоит из этапов конфигурации, генерации и непосредственно сборки.