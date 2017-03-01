
cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_STANDARD 14)
project(__PROJECT_NAME__ C CXX)

include_directories(inc)

add_subdirectory(googletest)

file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/*.cc)
file(GLOB TEST_SRC_FILES ${PROJECT_SOURCE_DIR}/test/*.cc)
add_executable(test1 ${SRC_FILES} ${TEST_SRC_FILES})
target_link_libraries(test1 gtest gmock gmock_main)
