find_package(benchmark REQUIRED)
add_executable(bench-prog bench.cc)
target_link_libraries(bench-prog benchmark)
