# Практика. Конвейер задач

## Описание проекта

Задача состоит из двух частей.
1. Написать функцию `executePipeline`. Она должна обеспечивать конвейерный запуск функций-воркеров `job`. Выход первого воркера является входом для второго воркера, а выход второго — входом для третьего и т.д. 
2. Написать несколько функций-воркеров, которые делают саму работу. 

Воркеры должны использовать такие функции. 
1. Функция `encrypt` шифрует.
2. Функция `compress` сжимает.  

Обе эти функции находятся в файле `common.go`.  

Необходимо реализовать расчет со следующей цепочкой.
1. На вход в канал `in` из теста воркеру `encryptAndCompress` последовательно подаются некоторые строки `data`.
2. Функция `encryptAndCompress` считает значение `encrypt(data)+"~"+encrypt(compress(data))`. Это конкатенация двух строк через тильду `~`. Результат `middleData` пишется на выход в канал `out`. 
3. Функция `multiEncrypt` считает значение `encrypt(th+middleData)`. Данные `middleData` приходят на вход из канала `in`. Этот же канал является `out` для `encryptAndCompress`. Здесь `th=0..5` — цифра в соответствующем диапазоне, приведенная к строке. На выходе `multiEncrypt` — конкатенация результатов в порядке расчета.
4. Функция `generateResult` получает строки из `multiEncrypt`, [сортирует](https://golang.org/pkg/sort/) по возрастанию, объединяет отсортированный результат через символ нижнего подчеркивания `_`  в одну строку. Результат пишется на выход.

Ограничения:
* `compress` может одновременно вызываться только `1 раз`, считается `10 мс`. Если одновременно запустится несколько — будет задержка на `1 с`.
* `encrypt` считается `1 с`.
* На все расчеты у нас `3 с`. Таким образом, необходимо написать код, который выполняется параллельно. 

## Пример

Результаты, которые выводятся, если отправить `2` значения — закомментировано в тесте:

```
encryptAndCompress data apple
encryptAndCompress compress a1p2l1e1
encryptAndCompress encrypt(data) EAcVHhE=
encryptAndCompress encrypt(compress(data)) EEYVQBhIFEY=
encryptAndCompress result EAcVHhE=~EEYVQBhIFEY=

multiEncrypt middleData EAcVHhE=~EEYVQBhIFEY=
multiEncrypt encrypt(th+middleData) 0 QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt encrypt(th+middleData) 1 QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt encrypt(th+middleData) 2 QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt encrypt(th+middleData) 3 QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt encrypt(th+middleData) 4 RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt encrypt(th+middleData) 5 RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiEncrypt result QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==

encryptAndCompress data orange
encryptAndCompress compress o1r1a1n1g1e1
encryptAndCompress encrypt(data) HgUEHBMc
encryptAndCompress encrypt(compress(data)) HkYXQxVIH0YCQxFI
encryptAndCompress result HgUEHBMc~HkYXQxVIH0YCQxFI

multiEncrypt middleData HgUEHBMc~HkYXQxVIH0YCQxFI
multiEncrypt encrypt(th+middleData) 0 QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt encrypt(th+middleData) 1 QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt encrypt(th+middleData) 2 Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt encrypt(th+middleData) 3 Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt encrypt(th+middleData) 4 RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt encrypt(th+middleData) 5 RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiEncrypt result QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=

generateResult
QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=_QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
```

Код писать в `worker.go`. В этот файл не надо добавлять ничего из `common.go`. Это один пакет.
