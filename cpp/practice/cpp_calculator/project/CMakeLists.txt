cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(calculator LANGUAGES CXX)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_COLOR_DIAGNOSTICS ON) # Disables ninja color-codes suppression

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -stdlib=libc++ -Werror -Wall \
    -Wno-unused-variable -Wno-logical-op-parentheses -O3 \
    -fcolor-diagnostics") # Enables colored output for clang

# Подключение модулей
include(precompiled_modules.cmake)

add_executable(main)

target_sources(main
    PUBLIC
    main.cpp
    PUBLIC
    FILE_SET senjun_modules TYPE CXX_MODULES FILES
    calculator.cppm
)

target_link_libraries(main PUBLIC std_module)

add_executable(tests)

target_sources(tests
    PUBLIC
    tests.cpp
    PUBLIC
    FILE_SET senjun_modules TYPE CXX_MODULES FILES
    calculator.cppm
)

target_link_libraries(tests PUBLIC test_modules)

# Docker-образ, внутри которого компилируется и запускается проект:
# https://github.com/senjun-team/senjun-images/blob/main/cpp/Dockerfile

# Чтобы собрать проект локально, заранее соберите модуль std.
# Как это сделать, можно посмотреть в нашем Docker-образе.
# В нем же можно посмотреть, какие версии clang, cmake и ninja точно подходят.

# Затем выполните шаги:
# cmake -Wno-dev -Bbuild -GNinja
# cmake --build build/ -- -j4