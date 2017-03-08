cmake_minimum_required(VERSION 3.1)

enable_testing()
set(CMAKE_CXX_STANDARD 14)
project(__PROJECT_NAME__ C CXX)
add_subdirectory(googletest)

add_library(__PROJECT_NAME__ src/__PROJECT_NAME__.cc)
target_include_directories(__PROJECT_NAME__ PUBLIC include)

add_executable(__PROJECT_NAME__-test src/__PROJECT_NAME__-test.cc)
target_link_libraries(__PROJECT_NAME__-test __PROJECT_NAME__ gtest gmock gmock_main)
add_test(__PROJECT_NAME__-__test __PROJECT_NAME__-test)

add_executable(__PROJECT_NAME__-main src/main.cc)
target_link_libraries(__PROJECT_NAME__-main __PROJECT_NAME__)
