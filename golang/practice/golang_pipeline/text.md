# Практика. Pipeline

## Описание проекта

Задача состоит из двух частей.
1. Написать функцию `executePipeline`. Она должна обеспечивать конвейерный запуск функций-воркеров `job`. Выход первого воркера является входом для второго воркера, а выход второго — входом для третьего и т.д. 
2. Написать несколько функций, которые делают саму работу. 

Обозначим: 
1. Пусть `xor` считается через функцию `encryptorXOR`.
2. Пусть `rle` считается через функцию `compressorRLE`.

Обе эти функции находятся в файле `common.go`.  
Через `xor` происходит шифрование, через `rle` — сжатие. 

Необходимо реализовать расчет со следующей цепочкой:
1. На вход из теста воркеру `singleRes` последовательно подаются некоторые строки `data`.
2. `singleRes` считает значение `xor(data)+"~"+xor(rle(data))`. Это конкатенация двух строк через тильду `~`. Результат `singleData` пишется на выход.
3. `multiRes` считает значение `xor(th+singleData)`. Здесь `th=0..5` — цифра в соответствующем диапазоне, приведенная к строке. На выходе `multiRes` — конкатенация результатов в порядке расчета.
4. `allResults` получает все результаты, [сортирует](https://golang.org/pkg/sort/) по возрастанию, объединяет отсортированный результат через символ нижнего подчеркивания `_`  в одну строку. Результат пишется на выход.

Ограничения:
* `rle` может одновременно вызываться только `1 раз`, считается `10 мс`. Если одновременно запустится несколько — будет задержка на `1 с`.
* `xor` считается `1 с`.
* На все расчеты у нас `3 с`. Таким образом, необходимо написать код, который выполняется параллельно. 

## Пример

Результаты, которые выводятся, если отправить `2` значения — закомментировано в тесте:

```
singleRes data apple
singleRes rle a1p2l1e1
singleRes xor(data) EAcVHhE=
singleRes xor(rle(data)) EEYVQBhIFEY=
singleRes result EAcVHhE=~EEYVQBhIFEY=

multiRes singleData EAcVHhE=~EEYVQBhIFEY=
multiRes xor(th+singleData) 0 QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes xor(th+singleData) 1 QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes xor(th+singleData) 2 QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes xor(th+singleData) 3 QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes xor(th+singleData) 4 RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes xor(th+singleData) 5 RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
multiRes result QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==

singleRes data orange
singleRes rle o1r1a1n1g1e1
singleRes xor(data) HgUEHBMc
singleRes xor(rle(data)) HkYXQxVIH0YCQxFI
singleRes result HgUEHBMc~HkYXQxVIH0YCQxFI

multiRes singleData HgUEHBMc~HkYXQxVIH0YCQxFI
multiRes xor(th+singleData) 0 QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes xor(th+singleData) 1 QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes xor(th+singleData) 2 Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes xor(th+singleData) 3 Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes xor(th+singleData) 4 RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes xor(th+singleData) 5 RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=
multiRes result QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=

allResults QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=_QTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==
```

Код писать в `worker.go`. В этот файл не надо добавлять ничего из `common.go`. Это один пакет.

## Тестирование 

Запускать из папки `cmd`:

```
go test -v -race
```
