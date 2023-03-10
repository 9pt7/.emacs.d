#include <benchmark/benchmark.h>

using namespace benchmark;

static void BM_StringCreation(benchmark::State& state) {
    for (auto _ : state) {
        std::string empty_string;
        DoNotOptimize(empty_string);
        ClobberMemory();
    }
}
BENCHMARK(BM_StringCreation);

static void BM_StringCopy(benchmark::State& state) {
    std::string x = "hello";
    for (auto _ : state) {
        std::string copy(x);
        DoNotOptimize(x);
        ClobberMemory();
    }
}
BENCHMARK(BM_StringCopy);

BENCHMARK_MAIN();
