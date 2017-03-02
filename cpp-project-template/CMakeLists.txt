cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_STANDARD 14)
project(__PROJECT_NAME__ C CXX)
add_subdirectory(googletest)
add_subdirectory(main)
