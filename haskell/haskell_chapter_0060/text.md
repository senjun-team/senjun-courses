# Глава 6. Выбор и образцы

Эта глава откроет нам другие способы выбора, также вы узнаете про сопоставление с образцом. Уверяю, вы не останетесь равнодушными!

## Не только из двух

Часто мы хотим выбирать не только из двух возможных вариантов. Вот как это можно сделать:

```haskell
analyzeGold :: Int -> String
analyzeGold standard =
  if standard == 999
    then "Wow! 999 standard!"
    else if standard == 750
           then "Great! 750 standard."
           else if standard == 585
                  then "Not bad! 585 standard."
                  else "I don't know such a standard..."

main :: IO ()
main = putStrLn (analyzeGold 999)
```

Уверен, вы уже стираете плевок с экрана. Вложенная `if-then-else` конструкция не может понравиться никому, ведь она крайне неудобна в обращении. А уж если бы анализируемых проб золота было штук пять или семь, эта лестница стала бы поистине ужасной. К счастью, в Haskell можно написать по-другому:

```haskell
analyzeGold :: Int -> String
analyzeGold standard =
  if | standard == 999 -> "Wow! 999 standard!"
     | standard == 750 -> "Great! 750 standard."
     | standard == 585 -> "Not bad! 585 standard."
     | otherwise -> "I don't know such a standard..."
```

Не правда ли, так красивее? Это — множественный `if`. Работает он по схеме:

```haskell
if | COND1 -> EXPR1
   | COND2 -> EXPR2
   | ...
   | CONDn -> EXPRn
   | otherwise -> COMMON_EXPR
```

где `COND1..n` — выражения, дающие ложь или истину, а `EXPR1..n` — соответствующие им результирующие выражения. Особая функция `otherwise` соответствует общему случаю, когда ни одно из логических выражений не дало `True`, и в этой ситуации результатом условной конструкции послужит выражение `COMMON_EXPR`.

Проведите рефакторинг функции `mercuryState`, которая принимает температуру ртути в градусах Цельсия и возвращает ее агрегатное состояние.  {.task_text}

В теле функции замените вложенный `if-then-else` на множественный `if`. {.task_text}

```haskell {.task_source #haskell_chapter_0060_task_0010}
{-# LANGUAGE MultiWayIf #-}

module Main where

eqAbs :: Double -> Double -> Double -> Bool
eqAbs a b eps = abs (a - b) < eps

mercuryState :: Double -> String
mercuryState t = if eqAbs t (-38.83) 1e-2
                              then "Solid -> liquid"
                              else if eqAbs t 356.73 1e-2 
                                then "Liquid -> gas"
                                else if t > (-38.83) && t < 356.73
                                  then "Liquid"
                                  else if t < (-38.83)
                                    then "Solid"
                                    else "Gas"

main :: IO ()
main = do
    print (mercuryState (-39))
    print (mercuryState (-38.83))
    print (mercuryState 0)
    print (mercuryState 356.73)
    print (mercuryState 400)
```
Синтаксис множественного `if`: `if | COND1 -> EXPR1 | ... | CONDn -> EXPRn | otherwise -> COMMON_EXPR`. {.task_hint}
```haskell {.task_answer}
{-# LANGUAGE MultiWayIf #-}

module Main where

eqAbs :: Double -> Double -> Double -> Bool
eqAbs a b eps = abs (a - b) < eps

mercuryState :: Double -> String
mercuryState t = if | eqAbs t (-38.83) 1e-2 -> "Solid -> liquid"
                    | eqAbs t 356.73 1e-2 -> "Liquid -> gas"
                    | t > (-38.83) && t < 356.73 -> "Liquid"
                    | t < (-38.83) -> "Solid"
                    | otherwise -> "Gas"

main :: IO ()
main = do
    print (mercuryState (-39))
    print (mercuryState (-38.83))
    print (mercuryState 0)
    print (mercuryState 356.73)
    print (mercuryState 400)
```

Не пренебрегайте `otherwise`! Возвращаясь к функции `analyzeGold`: если вы не укажете `otherwise` и при этом примените функцию к значению, отличному от проверяемых:

```haskell
analyzeGold :: Int -> String
analyzeGold standard =
  if | standard == 999 -> "Wow! 999 standard!"
     | standard == 750 -> "Great! 750 standard."
     | standard == 585 -> "Not bad! 585 standard."

main :: IO ()
main = putStrLn (analyzeGold 583)  -- Ой...
```

компиляция завершится успешно, однако в момент запуска программы вас ожидает неприятный сюрприз в виде ошибки:

```
Non-exhaustive guards in multi-way if
```

Проверка получилась неполной, вот и причина ошибки.

Кстати, видите слово `guards` в сообщении об ошибке? Вертикальные черты перед логическими выражениями — это и есть охранники (англ. guard), неусыпно охраняющие наши условия. Потешное название выбрали. Чтобы читать их было легче, воспринимайте их как аналог слова «ИЛИ».

А сейчас стоп. На самом деле такой код не скомпилируется, так как не хватает одной маленькой, но важной детали. Вот как должен выглядеть модуль `Main`:

```haskell
{-# LANGUAGE MultiWayIf #-}  -- Что это??

module Main where

analyzeGold :: Int -> String
analyzeGold standard =
  if | standard == 999 -> "Wow! 999 standard!"
     | standard == 750 -> "Great! 750 standard."
     | standard == 585 -> "Not bad! 585 standard."
     | otherwise -> "I don't know such a standard..."

main :: IO ()
main = putStrLn (analyzeGold 999)
```

Вот теперь всё в порядке. Но что это за странный комментарий в первой строке модуля? Вроде бы оформлен как многострочный комментарий, но выглядит необычно. Перед нами — указание расширения языка Haskell.

Стандарт [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/) — это официальный стержень языка. Однако компилятор GHC, давно уж ставший компилятором по умолчанию при разработке на Haskell, обладает рядом особых возможностей. По умолчанию многие из этих возможностей выключены, а прагма `LANGUAGE` как раз для того и предназначена, чтобы их включать/активизировать. В данном случае мы включили расширение `MultiWayIf`. Именно это расширение позволяет нам использовать множественный `if`. Такого рода расширений существует очень много, и мы будем часто их использовать.

Помните: расширение, включённое с помощью прагмы `LANGUAGE`, действует лишь в рамках текущего модуля. И если я прописал его только в модуле `app/Main.hs`, то на модуль `src/Lib.hs` механизм `MultiWayIf` не распространяется.

## Без Если

Множественный `if` весьма удобен, но есть способ более красивый. Взгляните:

```haskell
analyzeGold :: Int -> String
analyzeGold standard
  | standard == 999 = "Wow! 999 standard!"
  | standard == 750 = "Great! 750 standard."
  | standard == 585 = "Not bad! 585 standard."
  | otherwise = "I don't know such a standard..."
```

Ключевое слово `if` исчезло. Схема здесь такая:

```haskell
function arg  -- Нет знака равенства?
  | COND1 = EXPR1
  | COND2 = EXPR2
  | ...
  | CONDn = EXPRn
  | otherwise = COMMON_EXPR
```

Устройство почти такое же, но, помимо исчезновения ключевого слова `if`, мы теперь используем знаки равенства вместо стрелок. Именно поэтому исчез знакомый нам знак равенства после имени аргумента `arg`. В действительности он, конечно, никуда не исчез, он лишь перешёл в выражения. А чтобы это легче прочесть, напишем выражения в строчку:

```haskell
function arg | COND1 = EXPR1 | ...
```

То есть перед нами уже не одно определение функции, а цепочка определений, потому нам и не нужно ключевое слово `if`.

Объявите модуль `Main`. Заведите функцию `requestStatus`. Она должна принимать целочисленный аргумент — HTTP-код ответа от сервера. И в зависимости от кода возвращать строку: {.task_text}
- 1xx — "Informational".
- 2xx — "Success".
- 3xx — "Redirection".
- 4xx — "Client error".
- 5xx — "Server error".
- В любом другом случае — "Invalid HTTP code".
 {.task_text}

```haskell {.task_source #haskell_chapter_0060_task_0020}
-- Your code here

main :: IO ()
main = do
    print (requestStatus 0)
    print (requestStatus 102)
    print (requestStatus 201)
    print (requestStatus 304)
    print (requestStatus 403)
    print (requestStatus 500)
    print (requestStatus 600)
```
Пример условия из цепочки определений: `| code >= 300 && code < 400 = "Redirection"`. {.task_hint}
```haskell {.task_answer}
module Main where

requestStatus :: Int -> String
requestStatus code
  | code >= 100 && code < 200 = "Informational"
  | code >= 200 && code < 300 = "Success"
  | code >= 300 && code < 400 = "Redirection"
  | code >= 400 && code < 500 = "Client error"
  | code >= 500 && code < 600 = "Server error"
  | otherwise = "Invalid HTTP code"

main :: IO ()
main = do
    print (requestStatus 0)
    print (requestStatus 102)
    print (requestStatus 201)
    print (requestStatus 304)
    print (requestStatus 403)
    print (requestStatus 500)
    print (requestStatus 600)
```

Что выведет этот код? В случае ошибки напишите `error`. {.task_text}

```haskell
module Main where

f :: Int -> Int -> Char
f x y | div x 2 == 0 && div y 2 /= 0 = 'A' | otherwise = 'B'

main :: IO ()
main = print (f 17 0)
```

```consoleoutput {.task_source #haskell_chapter_0060_task_0030}
```
Если аргумент `x` функции `f` четный, а `y` — нечетный, то функция возвращает `'A'` Иначе — `'B'`. {.task_hint}
```haskell {.task_answer}
B
```

Но и эту цепочку определений `function arg | COND1 = EXPR1 | ...` можно упростить.

## Сравнение с образцом {#block-pattern-matching}

Убрав слово `if`, мы и с нашими виртуальными «ИЛИ» можем расстаться. В этом случае останется лишь это:

```haskell
analyzeGold :: Int -> String  -- Одно объявление.
-- И множество определений...
analyzeGold 999 = "Wow! 999 standard!"
analyzeGold 750 = "Great! 750 standard."
analyzeGold 585 = "Not bad! 585 standard."
analyzeGold _   = "I don't know such a standard..."
```

Мы просто перечислили определения функции `analyzeGold` одно за другим. На первый взгляд, возможность множества определений одной и той же функции удивляет, но если вспомнить, что применение функции суть выражение, тогда ничего удивительного.

Когда функция `analyzeGold` применяется к конкретному аргументу, этот аргумент последовательно сравнивается с образцом (англ. pattern matching). Образца здесь три: `999`, `750` и `585`. И если раньше мы сравнивали аргумент с этими числовыми значениями явно, посредством функции `==`, теперь это происходит скрыто.

Идея сравнения с образцом очень проста: что-то (в данном случае реальный аргумент) сопоставляется с образцом (или образцами) на предмет «подходит/не подходит». Если подходит — то есть сравнение с образцом даёт результат `True` — готово, используем соответствующее выражение. Если же не подходит — переходим к следующему образцу.

Сравнение с образцом, называемое ещё «сопоставлением с образцом», используется в Haskell чрезвычайно широко. В русскоязычной литературе перевод словосочетания «pattern matching» не особо закрепился, вместо этого так и говорят «паттерн матчинг». Я поступлю так же.

Но что это за символ подчёркивания такой, в последнем варианте определения? Вот этот `_`: {#block-wildcard-match}

```haskell
analyzeGold _ = "I don't know such a standard..."
```

С формальной точки зрения, это — универсальный образец, сравнение с которым всегда истинно. Ещё говорят, что с ним матчится (англ. match) всё что угодно. А с неформальной — это символ, который можно прочесть как «мне всё равно». Мы как бы говорим: «В данном случае нас не интересует конкретное содержимое аргумента, нам всё равно, мы просто возвращаем строку `I don't know such a standard...`».

Важно отметить, что сравнение аргумента с образцами происходит последовательно, сверху вниз. Поэтому если мы напишем так:

```haskell
analyzeGold :: Int -> String
analyzeGold _   = "I don't know such a standard..."
analyzeGold 999 = "Wow! 999 standard!"
analyzeGold 750 = "Great! 750 standard."
analyzeGold 585 = "Not bad! 585 standard."
```

наша функция будет всегда возвращать первое выражение, строку `I don't know such a standard...`, и это вполне ожидаемо: первая же проверка гарантированно даст нам `True`, ведь с образцом `_` совпадает всё что угодно. Таким образом, общий образец следует располагать в самом конце, чтобы мы попали на него лишь после того, как не сработали все остальные образцы.

Напишите функцию `baseToInt`, которая принимает строку и возвращает число по принципу: {.task_text}
- "Bin" — 2.
- "Oct" — 8.
- "Hex" — 16.
- -1 в любом другом случае.
 {.task_text}

Для реализации функции используйте паттерн матчинг.  {.task_text}

```haskell {.task_source #haskell_chapter_0060_task_0040}
module Main where

-- Your code here

main :: IO ()
main = do
    print (baseToInt "Bin")
    print (baseToInt "Oct")
    print (baseToInt "Hex")
    print (baseToInt "Random")
```
Не забудьте обработать общий случай: `baseToInt _ = -1`. {.task_hint}
```haskell {.task_answer}
module Main where

baseToInt :: String -> Int
baseToInt "Bin" = 2
baseToInt "Oct" = 8
baseToInt "Hex" = 16
baseToInt _ = -1

main :: IO ()
main = do
    print (baseToInt "Bin")
    print (baseToInt "Oct")
    print (baseToInt "Hex")
    print (baseToInt "Random")
```

## Конструкция case-of

Существует ещё один вид паттерн матчинга, с помощью конструкции `case-of`:

```haskell
analyzeGold standard =
  case standard of
    999 -> "Wow! 999 standard!"
    750 -> "Great! 750 standard."
    585 -> "Not bad! 585 standard."
    _   -> "I don't know such a standard..."
```

Запомните конструкцию `case-of`, мы встретимся с нею не раз. Работает она по модели:

```haskell
case EXPRESSION of
  PATTERN1 -> EXPR1
  PATTERN2 -> EXPR2
  ...
  PATTERNn -> EXPRn
  _        -> COMMON_EXPR
```

где `EXPRESSION` — анализируемое выражение, последовательно сравниваемое с образцами `PATTERN1..n`. Если ни одно не сработало — как обычно, упираемся в универсальный образец `_` и выдаём `COMMON_EXPR`.

Перепишите функцию `baseToInt` с использованием `case-of`.  {.task_text}

```haskell {.task_source #haskell_chapter_0060_task_0050}
module Main where

baseToInt :: String -> Int
baseToInt "Bin" = 2
baseToInt "Oct" = 8
baseToInt "Hex" = 16
baseToInt _ = -1

main :: IO ()
main = do
    print (baseToInt "Bin")
    print (baseToInt "Oct")
    print (baseToInt "Hex")
    print (baseToInt "Random")
```
Функция принимает единственный аргумент, допустим `base`. В теле функции в выражении `case base of` обрабатываются 3 варианта строк и универсальный образец `_`. {.task_hint}
```haskell {.task_answer}
module Main where

baseToInt :: String -> Int
baseToInt base =
  case base of
  "Bin" -> 2
  "Oct" -> 8
  "Hex" -> 16
  _ -> -1

main :: IO ()
main = do
    print (baseToInt "Bin")
    print (baseToInt "Oct")
    print (baseToInt "Hex")
    print (baseToInt "Random")
```

В последующих главах мы встретимся и с другими видами паттерн матчинга, ведь он используется не только для выбора.
