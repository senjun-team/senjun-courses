#pragma once

import std;

inline std::size_t divide(std::size_t a, std::size_t b) {
    if (b == 0) {
        return std::numeric_limits<std::size_t>::max();
    }

    // Результат целочисленного деления a на b
    std::size_t res = 0;

    while (a >= b) {
        // На сколько бит влево нужно сдвинуть b
        int n = 0;

        // Находим максимальное количество бит, на которое нужно сдвинуть b,
        // b при этом не должен стать больше a
        while (a >= (b << (n + 1))) {
            ++n;
        }

        // Уменьшаем a на b * 2 ^ n
        a -= b << n;

        // К результату деления добавляем 2 ^ n, то есть количество раз,
        // которое b было вычтено из a с помощью побитового сдвига
        res += 1 << n;
    }

    return res;
}
