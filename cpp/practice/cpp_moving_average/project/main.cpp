#include "moving_average.h"

int main()
{
    const std::size_t n = 2;
    MovingAverage ma = MovingAverage(n);
    std::println("Window size: {}", n);

    ma.add(0);
    ma.add(1);
    std::println("Numbers: 0, 1. Moving average = {}", ma.val());

    ma.add(2);
    std::println("Numbers: 1, 2. Moving average = {}", ma.val());

    ma.add(3);
    std::println("Numbers: 2, 3. Moving average = {}", ma.val());
}
