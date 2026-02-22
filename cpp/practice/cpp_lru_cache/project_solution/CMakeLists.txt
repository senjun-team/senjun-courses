cmake_minimum_required(VERSION 3.30.0 FATAL_ERROR)

project(lru LANGUAGES CXX)

set(CMAKE_BUILD_TYPE Release)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BENCHMARK_ENABLE_TESTING OFF)

set(CMAKE_COLOR_DIAGNOSTICS ON) # Для цветного вывода ошибок сборки

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -Werror -Wall \
    -Wno-unused-variable -Wno-logical-op-parentheses -O3 \
    -fcolor-diagnostics") # Цветной вывод ошибок clang

find_package(GTest REQUIRED)

include(FetchContent)

FetchContent_Declare(
    google_benchmark
    SOURCE_DIR "/third_party/google/benchmark/"
)

FetchContent_MakeAvailable(google_benchmark)

include_directories(lru_cache)
add_subdirectory(lru_cache)

add_executable(main benchmarks.cpp)
target_link_libraries(main PRIVATE lru_cache benchmark)

add_executable(tests tests.cpp)
target_link_libraries(tests PRIVATE lru_cache GTest::gtest)
