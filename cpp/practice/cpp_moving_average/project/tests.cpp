import boost.ut;
import test_helpers;

import moving_average;

import std;

using namespace boost::ut;

std::string error_message_val(const std::vector<int> & input,
                          std::size_t window_size,
                          std::size_t index,
                          double plan,
                          double fact)
{
    return std::format("Method val() returned invalid value.\n"
                      "Window size: {}.\n"
                      "Input sequence: {}\n"
                      "val() returned invalid value for the last number in input sequence: {}.\n"
                      "Expected value: {}. Actual value: {}",
                      window_size,
                      std::vector<int>(input.begin(), input.begin() + index + 1),
                      input[index],
                      plan,
                      fact
    );
}

std::string error_message_nan(const std::vector<int> & input,
                          double fact,
                          std::size_t window_size,
                          std::size_t index)
{
    return std::format("Method val() must return NAN (but returned {})"
                      "in case if there is not enough numbers in sliding window.\n"
                      "Window size: {}.\n"
                      "Input sequence: {}\n",
                      fact,
                      window_size,
                      std::vector<int>(input.begin(), input.begin() + index + 1)
    );
}

void check_moving_average(std::size_t window_size, std::vector<int> input)
{
    MovingAverage ma(window_size);
    senjun::MovingAverage ma_plan(window_size);

    for (std::size_t i = 0; i < input.size(); ++i)
    {
        ma.add(input[i]);
        ma_plan.add(input[i]);

        if (i + 1 < ma.window_size())
        {
            const double fact = ma.val();
            expect(std::isnan(fact)) << error_message_nan(input, fact, ma.window_size(), i) <<  fatal;
        }
        else
        {
            const double plan = ma_plan.val();
            const double fact = ma.val();
            expect(plan == fact) << error_message_val(input, ma.window_size(), i, plan, fact) << fatal;
        }
    }
}

template<class T>
std::size_t run_and_measure(T & ma, int number)
{
    auto start = std::chrono::high_resolution_clock::now();
    ma.add(number);

    // Реалистичный сценарий: скользящее среднее
    // запрашивается чаще, чем добавляется новое значение.
    // На 1 вызов add() приходится 10 вызовов val().
    for (std::size_t i = 0; i < 10; ++i)
        const volatile double res = ma.val();

    auto finish = std::chrono::high_resolution_clock::now();
    return std::chrono::duration_cast<std::chrono::nanoseconds>(finish-start).count();
}

template<class T1, class T2>
std::pair<double, double> measure_two_instances(T1 & ma1, T2 & ma2, const std::vector<int> & input)
{
    std::vector<std::size_t> measures1(input.size());
    std::vector<std::size_t> measures2(input.size());

    for (int number: input)
    {
        measures1.push_back(run_and_measure(ma1, number));
        measures2.push_back(run_and_measure(ma2, number));
    }

    return {average(measures1), average(measures2)};
}

// Юнит-тесты
int main()
{
    // Тест на то, что конструктор кидает иключение
    "Constructor throws"_test = [] {
        try
        {
            MovingAverage ma = MovingAverage(0);
            expect(false) << "Constructor must throw an exception in case of 0-sized window" << fatal;
        }
        catch(const std::invalid_argument & e)
        {
            // Expected behaviour
        }
        catch(...)
        {
            expect(false) << "Constructor must throw std::invalid_argument" << fatal;
        }
    };

    // Тест, реализующий пример скользящего среднего из текста практики
    "Case from example in text"_test = [] {
        check_moving_average(3, std::vector<int>{5, 0, 1, 8});
    };

    // Тест расчета скользящего среднего для нескольких коротких последовательностей чисел
    "Simple cases"_test = [] {
        for (std::size_t n = 1; n < 6; ++n)
        {
            check_moving_average(n, std::vector<int>{1, 1, 1, 1});
            check_moving_average(n, std::vector<int>{-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5});
            check_moving_average(n, std::vector<int>{0, 10, 100, 1000, 1'000'000, -1, 0, 0, 0});
        }
    };

    // Тест на то, что методы add() и val() работают за константное время,
    // то есть скорость их выполнения не зависит от размера окна.
    "O(1) complexity"_test = [] {
        const std::size_t numbers_count = 3'000;
        std::vector<int> input = random_vector(numbers_count);

        const std::size_t window_size_big = 1'500;
        MovingAverage ma_big = MovingAverage(window_size_big);

        const std::size_t window_size_small = 1;
        MovingAverage ma_small = MovingAverage(window_size_small);

        const auto & [avg_big, avg_small] = measure_two_instances(ma_big, ma_small, input);
        
        std::println("\nMeasuring time of working with small window and with big window...\n"
                     "Average time of add() and val() for small window = {}: {} nanoseconds.\n"
                     "Average time of add() and val() for big window = {}: {} nanoseconds.\n"
                     "Measurements count: {}",
                     window_size_small,
                     static_cast<std::size_t>(avg_small),
                     window_size_big,
                     static_cast<std::size_t>(avg_big),
                     numbers_count - window_size_big + 1);

        // Примем 20% за погрешность вычисления на VPS
        expect(avg_big <= avg_small * 1.2) << "Your implementation of add() or val() doesn't have O(1) complexity" << fatal;
    };

    // Тест на скорость работы решения пользователя. Сравниваем класс пользователя
    // с эталонным.
    "Check solution running time against simple solution"_test = [] {
        std::vector<int> input = random_vector(5'000);
        const std::size_t window_size = 2'500;
        
        MovingAverage ma_user = MovingAverage(window_size);
        senjun::MovingAverage ma_senjun = senjun::MovingAverage(window_size);

        const auto & [avg_user, avg_senjun] = measure_two_instances(ma_user, ma_senjun, input);
        
        std::println("\nChecking user solution against our simple implementation...\n"
                     "Average time of add() and val() for user solution: {} nanoseconds.\n"
                     "Average time of add() and val() for senjun simple solution: {} nanoseconds.\n"
                     "Measurements count: {}. Window size: {}",
                     static_cast<std::size_t>(avg_user),
                     static_cast<std::size_t>(avg_senjun),
                     input.size() - window_size + 1,
                     window_size);

        // Примем 30% за допустимое замедление, включая погрешность вычисления на VPS
        expect(avg_user <= avg_senjun * 1.3) << "Your implementation of MovingAverage doesn't fit our timeouts. Please optimize it." << fatal;
    };
}
