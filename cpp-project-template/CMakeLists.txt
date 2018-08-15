cmake_minimum_required(VERSION 3.8)

enable_testing()
set(CMAKE_CXX_STANDARD 17)
project(__PROJECT_NAME__ CXX)

option(ENABLE_BENCHMARKS "Enable benchmarks." OFF)
option(ENABLE_TESTS "Enable tests." OFF)

add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(benchmark)
add_subdirectory(googletest)
