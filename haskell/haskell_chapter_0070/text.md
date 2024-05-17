# Глава 7. Пусть будет там, Где...

В этой главе мы узнаем, как сделать наши функции более удобными и читабельными.

## Пусть

В нижеследующих примерах мы вновь будем использовать расширение GHC `MultiWayIf`. Рассмотрим следующую функцию:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  if | timeInS <  40 -> timeInS + 120
     | timeInS >= 40 -> timeInS + 8 + 120
```

Мы считаем время некоторого события, и если исходное время меньше `40` секунд — результирующее время увеличено на `120` секунд, в противном случае — ещё на `8` секунд сверх того. Перед нами классический пример «магических чисел» (англ. magic numbers), когда смысл конкретных значений скрыт за семью печатями. Что за `40`, и что за `8`? Во избежание этой проблемы можно ввести временные выражения, и тогда код станет совсем другим:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold  = 40
      correction = 120
      delta      = 8
  in
  if | timeInS <  threshold -> timeInS + correction
     | timeInS >= threshold -> timeInS + delta + correction
```

Вот, совсем другое дело! Мы избавились от «магических чисел», введя поясняющие выражения `threshold`, `correction` и `delta`. Конструкция `let-in` вводит поясняющие выражения по схеме:

```haskell
let DECLARATIONS in EXPRESSION
```

где `DECLARATIONS` — выражения, декларируемые нами, а `EXPRESSION` — выражение, в котором используется выражения из `DECLARATION`. Когда мы написали:

```haskell
let threshold = 40
```

мы объявили: «Отныне выражение `threshold` равно выражению `40`». Выглядит как присваивание, но мы-то уже знаем, что в Haskell его нет. Теперь выражение `threshold` может заменить собою число `40` внутри выражения, следующего за словом `in`:

```haskell
  let threshold = 40
      ...
  in if | timeInS <  threshold -> ...
        | timeInS >= threshold -> ...
```

С помощью ключевого слова `let` можно ввести сколько угодно пояснительных/промежуточных выражений, что делает наш код понятнее, а во многих случаях ещё и короче.

И кстати, мы ведь можем упростить условную конструкцию, воспользовавшись `otherwise`:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold  = 40
      correction = 120
      delta      = 8
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
```

Напишите функцию `f`, считающую вес тела по его массе. Вес равен массе, умноженной на ускорение свободного падения (9.81 м/с^2).  {.task_text}

Вместо «магического числа» 9.81 заведите выражение `g` в блоке `let-in` функции `f`. {.task_text}

```haskell {.task_source #haskell_chapter_0070_task_0010}
module Main where

-- Your code here

main :: IO ()
main = do
    print (f 10.0)
    print (f 50.2)
```
Воспользуйтесь конструкцией `let g = 9.81 in ...`. {.task_hint}
```haskell {.task_answer}
module Main where

f :: Double -> Double
f m = let g = 9.81 
      in m * g

main :: IO ()
main = do
    print (f 10.0)
    print (f 50.2)
```

Важно помнить, что введённое конструкцией `let-in` выражение существует лишь в рамках выражения, следующего за словом `in`.

Сократим область видимости промежуточного выражения `delta`. {.task_text}

Что выведет этот код? В случае ошибки напишите `error`. {.task_text}

```haskell
{-# LANGUAGE MultiWayIf #-}

module Main where

calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold  = 40
      correction = 120
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise ->
        let delta = 8 in timeInS
                         + delta
                         + correction

main :: IO ()
main = print (calculateTime 40)
```

```consoleoutput {.task_source #haskell_chapter_0070_task_0020}
```
Функция `calculateTime` применяется к значению 40, которое равно `threshold`. Это означает, что выполнится `otherwise` и будет вычислено выражение 40 + 8 + 120. {.task_hint}
```haskell {.task_answer}
168
```

При желании `let`-выражения можно записывать и в строчку:

```haskell
  ...
  let threshold = 40; correction = 120
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise ->
        let delta = 8 in timeInS + delta + correction
```

В этом случае мы перечисляем их через точку с запятой. Лично мне такой стиль не нравится, но выбирать вам.

Подключите расширение `MultiWayIf`. Вместо «магических чисел» 200 и 403 в функции `requestStatus` используйте выражения `httpOk` и `httpForbidden`. {.task_text}

```haskell {.task_source #haskell_chapter_0070_task_0030}
module Main where

requestStatus :: Int -> String
requestStatus code =
  if | code == 200 -> "Congratulations!"
     | code == 403 -> "Access denied."
     | otherwise -> "Unexpected error."
     
main :: IO ()
main = do
    print (requestStatus 200)
    print (requestStatus 403)
    print (requestStatus 503)
```
Синтаксис подключения расширения: `{-# LANGUAGE MultiWayIf #-}`. При использовании нескольких выражений в блоке `let-in` каждое из них пишется на отдельной строке. {.task_hint}
```haskell {.task_answer}
{-# LANGUAGE MultiWayIf #-}

module Main where

requestStatus :: Int -> String
requestStatus code =
  let httpOk = 200
      httpForbidden = 403
  in
  if | code == httpOk -> "Congratulations!"
     | code == httpForbidden -> "Access denied."
     | otherwise -> "Unexpected error."
     
main :: IO ()
main = do
    print (requestStatus 200)
    print (requestStatus 403)
    print (requestStatus 503)
```

## Где

Существует иной способ введения промежуточных выражений, взгляните:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS +
                    delta +
                    correction
  where
    threshold  = 40
    correction = 120
    delta      = 8
```

Ключевое слово `where` делает примерно то же, что и `let`, но промежуточные выражения задаются в конце функции. Такая конструкция читается подобно научной формуле:

```
  S = V * t,      -- Выражение
где
  -- Всё то, что
  -- используется
  -- в выражении.
  S = расстояние,
  V = скорость,
  t = время.
```

В отличие от `let`, которое может быть использовано для введения супер-локального выражения (как в задаче про сокращение области видимости `delta`), все `where`-выражения доступны в любой части выражения, предшествующего ключевому слову `where`.

Перепишите тело функции `priceIncludingDiscount`, принимающей цену на товар и возвращающую цену с учетом скидки.  {.task_text}

Используйте выражение `where`: значение 150 сделайте выражением `minPriceForDiscount`, а 15 — выражением `discount`. {.task_text}

```haskell {.task_source #haskell_chapter_0070_task_0040}
module Main where

priceIncludingDiscount :: Double -> Double
priceIncludingDiscount price
  | price >= 150 = price * (1 - 15 / 100.0)
  | otherwise = price
     
main :: IO ()
main = do
    print (priceIncludingDiscount 20)
    print (priceIncludingDiscount 200)
```
Синтаксис: `EXPRESSION where DECLARATIONS`. {.task_hint}
```haskell {.task_answer}
module Main where

priceIncludingDiscount :: Double -> Double
priceIncludingDiscount price
  | price >= minPriceForDiscount = price * (1 - discount / 100.0)
  | otherwise = price
  where
    discount = 15
    minPriceForDiscount = 150
     
main :: IO ()
main = do
    print (priceIncludingDiscount 20)
    print (priceIncludingDiscount 200)
```

## Вместе

Мы можем использовать `let-in` и `where` совместно, в рамках одной функции:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40 in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    correction = 120
    delta      = 8
```

Часть промежуточных значений задана вверху, а часть — внизу. Общая рекомендация: не смешивайте `let-in` и `where` без особой надобности, такой код читается тяжело, избыточно.

В качестве промежуточных могут выступать и более сложные выражения!  {.task_text}

Что выведет этот код? В случае ошибки напишите `error`. {.task_text}

```haskell
{-# LANGUAGE MultiWayIf #-}

module Main where

calculateTime :: Int -> Int
calculateTime timeInS =
  let threshold = 40 in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    -- Это промежуточное выражение зависит от аргумента...
    correction = timeInS * 2
    -- А это - от другого выражения...
    delta      = correction - 4

main :: IO ()
main = print (calculateTime 50)
```

```consoleoutput {.task_source #haskell_chapter_0070_task_0050}
```
Выражение `correction` равно `timeInS * 2`, то есть теперь оно зависит от значения аргумента функции. А выражение `delta` зависит в свою очередь от `correction`.  {.task_hint}
```haskell {.task_answer}
246
```

Что интересно, мы можем менять порядок задания выражений:

```haskell
  ...
  let threshold = 40
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    delta      = correction - 4
    correction = timeInS * 2
```

Выражение `delta` теперь задано первым по счёту, но это не имеет никакого значения. Ведь мы всего лишь объявляем равенства, и результат этих объявлений не влияет на конечный результат вычислений. Конечно, порядок объявления равенств не важен и для `let`-выражений:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let delta     = correction - 4
      threshold = 40
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    correction = timeInS * 2
```

Мало того, что мы задали `let`-выражения в другом порядке, так мы ещё и использовали в одном из них выражение `correction`! То есть в `let`-выражении использовалось `where`-выражение. А вот проделать обратное, увы, не получится:

```haskell
calculateTime :: Int -> Int
calculateTime timeInS =
  let delta     = correction - 4
      threshold = 40
  in
  if | timeInS < threshold -> timeInS + correction
     | otherwise -> timeInS + delta + correction
  where
    correction = timeInS * 2 * threshold -- Из let??
```

При попытке скомпилировать такой код мы получим ошибку:

```bash
Not in scope: ‘threshold’
```

Таково ограничение: использовать `let`-выражения внутри `where`-выражений невозможно, ведь последние уже не входят в выражение, следующее за словом `in`.

Ну что ж, пора двигаться дальше, ведь внутренности наших функций не ограничены условными конструкциями.

