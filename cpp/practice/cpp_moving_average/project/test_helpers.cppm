module;

#include <cmath> // содержит константу NAN

export module test_helpers;

import std;

// senjun::MovingAverage - это эталонное решение, относительно которого
// будет замеряться производительность решения пользователя.
// Если вы не хотите случайно увидеть решение практики, не читайте этот файл.
export namespace senjun
{
    class MovingAverage
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
}

// Среднее арифметическое значений вектора
export template<class C>
double average(std::vector<C> const& v)
{
    if(v.empty())
        return 0;

    return std::reduce(v.begin(), v.end()) / static_cast<double>(v.size());
}

// Получение вектора длины len, заполненного случайными числами от -100 до 200
export std::vector<int> random_vector(std::size_t len)
{
    std::random_device rnd_device;
    std::mt19937 mersenne_engine {rnd_device()};
    std::uniform_int_distribution<int> dist {-100, 200};
    
    auto gen = [&](){ return dist(mersenne_engine); };

    std::vector<int> vec(len);
    std::generate(vec.begin(), vec.end(), gen);
    return vec;
}