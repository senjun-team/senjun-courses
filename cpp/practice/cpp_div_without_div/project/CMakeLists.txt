cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

# Включаем флаг для возможности `import std`.
# Эта строка должна идти ДО объявления, что проект на C++ (значение CXX).
set(CMAKE_EXPERIMENTAL_CXX_IMPORT_STD
  "0e5b6991-d74f-4b3d-a41c-cf096e0b2508")

# Для всех целей сборки устанавливаем возможность импорта std в 1.
set(CMAKE_CXX_MODULE_STD 1)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++ -Werror -Wall -O2")

project(div_without_div LANGUAGES CXX)

add_executable(main)

target_sources(main
  PRIVATE
    main.cpp)

target_compile_features(main
  PRIVATE   cxx_std_23
  INTERFACE cxx_std_23)

add_executable(tests)

target_sources(tests
PRIVATE
    tests.cpp)

target_compile_features(tests
PRIVATE   cxx_std_23
INTERFACE cxx_std_23)

# Чтобы собрать проект локально, выполните:
# mkdir build && cd build
# cmake -Wno-dev -GNinja ..
# ninja
