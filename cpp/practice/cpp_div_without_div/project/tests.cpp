#include "ut.hpp"

#include "div.h"

// Юнит-тесты
int main()
{
    using namespace boost::ut;

    const std::size_t max_val = std::numeric_limits<std::size_t>::max();

    "Simple cases"_test = [] {
        expect(divide(1, 1) == 1);
        expect(divide(1, 2) == 0);
        expect(divide(2, 1) == 2);
        expect(divide(3, 2) == 1);
        expect(divide(10, 2) == 5);
        expect(divide(11, 2) == 5);
    };

    "Corner cases"_test = [] {
        expect(divide(1, 0) == max_val);
        expect(divide(0, 1) == 0);
        expect(divide(max_val - 1, max_val - 1) == 1);
        expect(divide(max_val - 1, 1) == max_val - 1);
        expect(divide(1, max_val - 1) == 0);
    };

    "Different cases"_test = [] {
        expect(divide(max_val / 2, 1) == max_val / 2);

        for (std::size_t i = 1'000'000; i < 1'000'090; ++i) {
            for (std::size_t j = 90; j < 100; ++j) {
                expect(divide(i, j) == i / j);
            }
        }
    };
}
