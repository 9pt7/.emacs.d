cmake_minimum_required(VERSION 3.8)

enable_testing()
set(CMAKE_CXX_STANDARD 17)
project(__PROJECT_NAME__ CXX)

option(ENABLE_BENCHMARKS "Enable benchmarks." OFF)
option(ENABLE_TESTS "Enable tests." OFF)


add_library(__PROJECT_NAME__ src/__PROJECT_NAME__.cc)
target_include_directories(__PROJECT_NAME__ PUBLIC include)

add_executable(__PROJECT_NAME__-main src/main.cc)
target_link_libraries(__PROJECT_NAME__-main __PROJECT_NAME__)

if(ENABLE_BENCHMARKS)
  find_package(benchmark REQUIRED)

  add_executable(bench-prog src/bench.cc)
  target_link_libraries(bench-prog benchmark)
endif()

if(ENABLE_TESTS)
  add_subdirectory(googletest)

  add_executable(__PROJECT_NAME__-test src/__PROJECT_NAME__-test.cc)
  target_link_libraries(__PROJECT_NAME__-test __PROJECT_NAME__ gtest gmock gmock_main)
  add_test(__PROJECT_NAME__-__test __PROJECT_NAME__-test)
endif()
