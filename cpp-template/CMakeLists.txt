
cmake_minimum_required(VERSION 3.1)

set(PROJECT_NAME_STR __PROJECT_NAME__)
set(PROJECT_TEST_NAME ${PROJECT_NAME_STR}_test)
set(CMAKE_CXX_STANDARD 14)
project(${PROJECT_NAME_STR} C CXX)
add_executable(${PROJECT_NAME_STR} main.cc)
include_directories(inc)

enable_testing()
add_subdirectory(googletest)

file(GLOB TEST_SRC_FILES ${PROJECT_SOURCE_DIR}/test/*.cc)
add_executable(${PROJECT_TEST_NAME} ${TEST_SRC_FILES})

target_link_libraries(${PROJECT_TEST_NAME} gtest gmock gmock_main)
add_test(test1 ${PROJECT_TEST_NAME})
