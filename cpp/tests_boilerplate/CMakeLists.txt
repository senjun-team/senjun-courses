find_package(Catch2 3 REQUIRED)

# These tests can use the Catch2-provided main
add_executable(tests fib.cpp)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
