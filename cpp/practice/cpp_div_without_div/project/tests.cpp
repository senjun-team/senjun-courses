#include "ut.hpp"

#include "div.h"

// Юнит-тесты
int main()
{
    using namespace boost::ut;

    const std::size_t max_val = std::numeric_limits<std::size_t>::max();

    "Simple cases"_test = [] {
        expect(divide(1, 1) == 1) << "divide(1, 1) must return 1" << fatal;
        expect(divide(1, 2) == 0) << "divide(1, 2) must return 0" << fatal;
        expect(divide(2, 1) == 2) << "divide(2, 1) must return 2" << fatal;
        expect(divide(3, 2) == 1) << "divide(3, 2) must return 1" << fatal;
        expect(divide(10, 2) == 5) << "divide(10, 2) must return 5" << fatal;
        expect(divide(11, 2) == 5) << "divide(11, 2) must return 5" << fatal;
    };

    "Corner cases"_test = [] {
        expect(divide(1, 0) == max_val)  << "divide(1, 0) must return std::numeric_limits<std::size_t>::max()" << fatal;
        expect(divide(0, 1) == 0) << "divide(0, 1) must return 0" << fatal;
        expect(divide(max_val - 1, max_val - 1) == 1) << "divide(std::numeric_limits<std::size_t>::max() - 1, std::numeric_limits<std::size_t>::max() - 1) must return 1" << fatal;
        expect(divide(max_val - 1, 1) == max_val - 1) << "divide(std::numeric_limits<std::size_t>::max() - 1, 1) must return std::numeric_limits<std::size_t>::max() - 1" << fatal;
        expect(divide(1, max_val - 1) == 0) << "divide(1, std::numeric_limits<std::size_t>::max() - 1) must return 0" << fatal;
    };

    "Different cases"_test = [] {
        expect(divide(max_val / 2, 1) == max_val / 2) << "divide(std::numeric_limits<std::size_t>::max() / 2, 1) must return std::numeric_limits<std::size_t>::max() / 2" << fatal;

        for (std::size_t i = 1'000'000; i < 1'000'090; ++i) {
            for (std::size_t j = 90; j < 100; ++j) {
                expect(divide(i, j) == i / j) << "divide(" << i << ", " << j << ") must return " << i / j << fatal;
            }
        }
    };
}
