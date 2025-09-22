cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(lru LANGUAGES CXX)

# Указание, что сборка - релизная

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BENCHMARK_ENABLE_TESTING OFF)

# Поиск системно установленного GTest

# Подключение google_benchmark из /third_party/google/benchmark-1.9.4/

# Добавление путей для поиска хедера lru_cache.h

# Добавление подпроекта lru_cache

add_executable(main benchmarks.cpp)

# Линковка с lru_cache и benchmark

add_executable(tests tests.cpp)

# Линковка с lru_cache и GTest::gtest
