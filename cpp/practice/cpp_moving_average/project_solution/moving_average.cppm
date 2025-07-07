module;

#include <cmath> // содержит константу NAN

export module moving_average;

import std;

export class MovingAverage
{
public:
    MovingAverage(std::size_t n)
    {
        if (n == 0)
            throw std::invalid_argument("Window size must be > 0");
        
        size = n;
    }

    std::size_t window_size()
    {
        return size;
    }

    void add(int number)
    {
        if (numbers.size() == size)
        {
            sum -= numbers.front();
            numbers.pop();
        }

        sum += number;
        numbers.push(number);
    }

    double val()
    {
        if (numbers.size() < size)
            return NAN;

        return static_cast<double>(sum) / size;
    }

private:
    // Значения в скользящем окне
    std::queue<int> numbers;

    // Сумма этих значений
    int sum = 0;

    // Размер окна
    std::size_t size = 0;
};