cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(brainfuck_interpreter LANGUAGES CXX)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(STD_MODULE_FILE /usr/local/lib/std.pcm)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -Werror -Wall -Wno-unused-variable -Wno-logical-op-parentheses -O2 -fmodule-file=std=${STD_MODULE_FILE}")

add_executable(main)

target_sources(main
    PUBLIC
    main.cpp
    PUBLIC
    FILE_SET senjun_modules TYPE CXX_MODULES FILES
    brainfuck.cppm
)

add_executable(tests)

target_sources(tests
    PUBLIC
    tests.cpp
    PUBLIC
    FILE_SET senjun_modules TYPE CXX_MODULES FILES
    ut.cppm
    test_helpers.cppm
    brainfuck.cppm
)
# Docker-образ, внутри которого компилируется и запускается проект:
# https://github.com/senjun-team/senjun-images/blob/main/cpp/Dockerfile

# Чтобы собрать проект локально, заранее соберите модуль std.
# Как это сделать, можно посмотреть в нашем Docker-образе.
# В нем же можно посмотреть, какие версии clang, cmake и ninja точно подходят.

# Затем выполните шаги:
# cmake -Wno-dev -Bbuild -GNinja
# cmake --build build/ -- -j4