cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(lru LANGUAGES CXX)

# Указание, что сборка - релизная

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BENCHMARK_ENABLE_TESTING OFF)

set(CMAKE_COLOR_DIAGNOSTICS ON) # Для цветного вывода ошибок сборки

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -Werror -Wall \
    -Wno-unused-variable -Wno-logical-op-parentheses -O3 \
    -fcolor-diagnostics") # Цветной вывод ошибок clang

# Поиск системно установленного GTest

# Подключение google_benchmark из /third_party/google/benchmark/

# Добавление путей для поиска хедера lru_cache.h

# Добавление подпроекта lru_cache

add_executable(main benchmarks.cpp)

# Линковка с lru_cache и benchmark

add_executable(tests tests.cpp)

# Линковка с lru_cache и GTest::gtest
