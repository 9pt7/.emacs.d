file(GLOB SRC_FILES *.cc)
add_executable(main ${SRC_FILES})
target_link_libraries(main)
