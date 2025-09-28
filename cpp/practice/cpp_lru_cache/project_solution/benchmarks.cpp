#include "lru_cache/lru_cache.h"

#include <benchmark/benchmark.h>

#include <algorithm>
#include <random>
#include <vector>

std::vector<int> random_vector(std::size_t len);

static void BM_LRUUsage(benchmark::State& state)
{
    for (auto _: state)
    {
        cache::LRUCache cache(100);
        const std::vector<int> keys = random_vector(1'000);
        const std::string val = "text";

        for (std::size_t i = 0; i < keys.size(); ++i)
        {
            cache.put(keys[i], val);

            if (i % 2 != 0)
            {
                const volatile auto res = cache.get(keys[i - 1]);
            }
        }
    }
}

// Регистрируем функцию в качестве бенчмарка
BENCHMARK(BM_LRUUsage);

BENCHMARK_MAIN();

std::vector<int> random_vector(std::size_t len)
{
    std::random_device rnd_device;
    std::mt19937 mersenne_engine {rnd_device()};
    std::uniform_int_distribution<int> dist {-10, 10};
    
    auto gen = [&](){ return dist(mersenne_engine); };

    std::vector<int> vec(len);
    std::generate(vec.begin(), vec.end(), gen);
    return vec;
}
