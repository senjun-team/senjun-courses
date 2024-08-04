# Глава 11. Кортеж

В этой главе мы познакомимся с кортежем и ещё ближе подружимся с паттерн матчингом.


## Кортеж
Кортеж (англ. tuple) — стандартная структура данных Haskell. В отличие от списка, она может содержать данные как одного типа, так и разных.

Структуры, способные содержать данные разных типов, называют гетерогенными (в переводе с греческого: «разного рода»).

Вот как выглядит кортеж:

```haskell
("Haskell", 2010)
```

Круглые скобки и значения, разделённые запятыми. Этот кортеж содержит значение типа `String` и ещё одно, типа `Int`. Вот ещё пример:

```haskell
("Haskell", "2010", "Standard")
```

То есть ничто не мешает нам хранить в кортеже данные одного типа.

## Тип кортежа

Тип списка строк, как вы помните, `[String]`. И не важно, сколько строк мы запихнули в список, одну или миллион — его тип останется неизменным. С кортежем же дело обстоит абсолютно иначе.

Тип кортежа зависит от количества его элементов. Вот тип кортежа, содержащего две строки:

```haskell
(String, String)
```

Вот ещё пример:

```haskell
(Double, Double, Int)
```

И ещё:

```haskell
(Bool, Double, Int, String)
```

Тип кортежа явно отражает его содержимое. Поэтому если функция применяется к кортежу из двух строк, применить её к кортежу из трёх никак не получится, ведь типы этих кортежей различаются:

```haskell
-- Разные типы
(String, String)
(String, String, String)
```

## Действия над кортежами

Со списками можно делать много всего, а вот с кортежами — не очень. Самые частые действия — собственно формирование кортежа и извлечение хранящихся в нём данных. Например:

```haskell
makeAlias :: String -> String -> (String, String)
makeAlias host alias = (host, alias)
```

Пожалуй, ничего проще придумать нельзя: на входе два аргумента, на выходе — двухэлементный кортеж с этими аргументами. Двухэлементный кортеж называют ещё парой (англ. pair). И хотя кортеж может содержать сколько угодно элементов, на практике именно пары встречаются чаще всего.

Обратите внимание, насколько легко создаётся кортеж. Причина тому — уже знакомый нам [паттерн матчинг.](/courses/haskell/chapters/haskell_chapter_0060#block-pattern-matching)

Мы просто указываем соответствие между левой и правой сторонами определения: «Пусть первый элемент пары будет равен аргументу `host`, а второй — аргументу `alias`». Ничего удобнее и проще и придумать нельзя. 

Переделайте функцию `makeAlias` из примера выше: пусть она возвращает кортеж не из двух, а из трех элементов: хоста, URL и имени (alias). {.task_text}

Для получения URL конкатенируйте строку `"https://"` и имя. {.task_text}

```haskell {.task_source #haskell_chapter_0110_task_0010}
module Main where

-- Your code here

main :: IO ()
main = print (makeAlias "173.194.71.106" "www.google.com")
```
Оператор `++` — это оператор конкатенации, склеивающий две строки в одну. Строго говоря, он склеивает два списка, но мы-то с вами уже знаем, что `String` есть ни что иное, как `[Char]`. Таким образом, `"https://"` ++ `"www.google.com"` даёт нам `"https://www.google.com"`. {.task_hint}
```haskell {.task_answer}
module Main where

makeAlias :: String -> String -> (String, String, String)
makeAlias host alias = (host, "https://" ++ alias, alias)

main :: IO ()
main = print (makeAlias "173.194.71.106" "www.google.com")
```

Извлечение элементов из кортежа также производится через паттерн матчинг:

```haskell  {.example_for_playground}
makeAlias :: String -> String -> (String, String)
makeAlias host alias = (host, alias)

main :: IO ()
main =
  let (host, alias) = makeAlias "173.194.71.106"
                                "www.google.com"
  in print (host ++ ", " ++ alias)
```

Функция `makeAlias` даёт нам пару из хоста и имени. Но что это за странная запись возле уже знакомого нам слова `let`? Это промежуточное выражение, но выражение хитрое, образованное через паттерн матчинг. Чтобы было понятнее, сначала перепишем функцию без него:

```haskell  {.example_for_playground .example_for_playground_001}
main :: IO ()
main =
  let pair  = makeAlias "173.194.71.106"
                        "www.google.com"
      host  = fst pair  -- Берём первое...
      alias = snd pair  -- Берём второе...
  in print (host ++ ", " ++ alias)
```

При запуске этой программы получим:

```
"173.194.71.106, www.google.com"
```

Стандартные функции `fst` и `snd` возвращают первый и второй элемент кортежа соответственно. Работают они только с кортежем из двух элементов, то есть с парой. 

Выражение `pair` здесь соответствует паре, выражение `host` — значению хоста, а `alias` — значению имени. Но не кажется ли вам такой способ избыточным? Мы в Haskell любим изящные решения, поэтому предпочитаем паттерн матчинг. 

Раскроем подробнее вышеприведённый способ:

```haskell
let (host, alias) = makeAlias "173.194.71.106" "www.google.com"

let (host, alias) = ("173.194.71.106", "www.google.com")
```

Читается это так: `host` присвоить значение "173.194.71.106"; `alias` присвоить значение "www.google.com". Вот такая простая магия. Функция `makeAlias` даёт нам пару, и мы достоверно знаем это! А если знаем, нам не нужно вводить промежуточные выражения вроде `pair`. Мы сразу говорим, что точно знаем: выражение, вычисленное функцией `makeAlias` — это пара значений.

```haskell
let (host, alias) = makeAlias "173.194.71.106" "www.google.com"
```

Это «зеркальная» модель. Через паттерн матчинг формируем:

```haskell
-- Формируем правую сторону
-- на основе левой...
host alias = (host, alias)
```

и через него же извлекаем:

```haskell
-- Формируем левую сторону
-- на основе правой...
(host, alias) = ("173.194.71.106", "www.google.com")
```

Элементы списка можно получать по индексу с помощью оператора `!!`. А что насчет кортежей? С ними такой фокус **не сработает!** Чтобы получить элемент кортежа, применяйте паттерн матчинг. Либо функции `fst` и `snd` для пар. Есть и третий способ. О нем — в конце главы.

Что выведет этот код? В случае ошибки напишите `error`. {.task_text}

```haskell  {.example_for_playground}
module Main where

chessMove :: String
          -> (String, String)
          -> (String, (String, String))
chessMove color (from, to) = (color, (from, to))

main :: IO ()
main = print (color ++ ": " ++ from ++ "-" ++ to)
  where
    (color, (from, to)) = chessMove "white" ("e2", "e4")
```

```consoleoutput {.task_source #haskell_chapter_0110_task_0020}
```
Функция `chessMove` даёт нам кортеж с кортежем, а раз мы точно знаем вид этого кортежа, сразу указываем `where`-выражение в виде образца: `(color, (from, to)) = chessMove "white" ("e2", "e4")`. Функция `print` применится к конкатенации строк "white", ": ", "e2", "-", "e4". {.task_hint}
```haskell {.task_answer}
white: e2-e4
```

Обратите внимание, объявление функции в задаче отформатировано чуток иначе: типы выстроены друг под другом через выравнивание стрелок под двоеточием. Вы часто встретите такой стиль в Haskell-проектах.

Напишите функцию `formatLocation`, которая: {.task_text}
- Принимает кортеж из двух значений типа `Double`: широты и долготы. 
- Возвращает строку вида `"You are here: LAT, LON"`, где `LAT` и `LON` — значения широты и долготы. Для превращения `Double` в строку воспользуйтесь встроенной функцией `show`.
 {.task_text}

В функции `main` используйте конструкцию `where`, чтобы вывести на экран результат применения функции к кортежу из чисел 34.7 и 10.1. {.task_text}

```haskell {.task_source #haskell_chapter_0110_task_0030}
module Main where

-- Your code here

main :: IO ()
main = -- And here
```
Определите функцию `formatLocation`, принимающую кортеж и возвращающую строку. В блоке `where` функции `main` заведите кортеж: `loc = (34.7, 10.1)`. {.task_hint}
```haskell {.task_answer}
module Main where

formatLocation :: (Double, Double) -> String
formatLocation (lat, lon) = "You are here: " ++ show lat ++ ", " ++ show lon

main :: IO ()
main = print (formatLocation loc)
  where
    loc = (34.7, 10.1)
```

## Кортежи и универсальные образцы

Мы можем вытаскивать по образцу лишь часть нужной нам информации. [Помните](/courses/haskell/chapters/haskell_chapter_0060#block-wildcard-match) универсальный образец `_`? 

Взгляните. Функция `patientEmail` даёт нам почту пациента. {#block-patient-email}

```haskell  {.example_for_playground}
-- Поясняющие псевдонимы
type UUID     = String
type FullName = String
type Email    = String
type Age      = Int
type Patient = (UUID, FullName, Email, Age)

patientEmail :: Patient -> Email
patientEmail (_, _, email, _) = email

main :: IO ()
main =
  putStrLn (patientEmail ( "63ab89d"
                         , "John Smith"
                         , "johnsm@gmail.com"
                         , 59
                         ))
```

Тип `Patient` — это псевдоним для кортежа из четырёх элементов: уникальный идентификатор, полное имя, адрес почты и возраст. Дополнительные псевдонимы делают наш код яснее: одно дело видеть безликую `String` и совсем другое — `Email`.

Рассмотрим внутренность функции `patientEmail`:

```haskell
patientEmail (_, _, email, _) = email
```

Функция говорит нам: «Да, я знаю, что мой аргумент — это четырёхэлементный кортеж, но меня в нём интересует исключительно третий по счёту элемент, соответствующий адресу почты, его я и верну».

Универсальный образец `_` делает наш код лаконичнее и понятнее, ведь он помогает нам игнорировать то, что нам неинтересно. Строго говоря, мы не обязаны использовать `_`, но с ним будет лучше.

Напишите функцию `extractTimestamp`, которая принимает кортеж из 3-х элементов: широты, долготы и временной метки. Функция должна вернуть значение временной метки. {.task_text}

Заведите псевдонимы и используйте их в объявлении функции: `Latitude`, `Longitude`, `UnixTimestamp`. {.task_text}

```haskell {.task_source #haskell_chapter_0110_task_0040}
module Main where

-- Your code here

main :: IO ()
main = print (extractTimestamp (37.0932, 51.8821, 1715536019))
```
Требуется получить третий элемент кортежа: `(_, _, ts) = ts`. {.task_hint}
```haskell {.task_answer}
module Main where

type Latitude = Double
type Longitude = Double
type UnixTimestamp = Int

extractTimestamp :: (Latitude, Longitude, UnixTimestamp) -> UnixTimestamp
extractTimestamp (_, _, ts) = ts

main :: IO ()
main = print (extractTimestamp (37.0932, 51.8821, 1715536019))
```

## А если ошиблись?

При использовании паттерн матчинга в отношении пары следует быть внимательным. Представим себе, что вышеупомянутый тип `Patient` был расширен:

```haskell
type UUID      = String
type FullName  = String
type Email     = String
type Age       = Int
type DiseaseId = Int  -- Новый элемент.
type Patient = ( UUID
               , FullName
               , Email
               , Age
               , DiseaseId
               )
```

Был добавлен идентификатор заболевания. И всё бы хорошо, но внести изменения в функцию `patientEmail` мы забыли:

```haskell
patientEmail :: Patient -> Email
patientEmail (_, _, email, _) = email
```

В кортеже `(_, _, email, _)` отсутствует пятый элемент! К счастью, в этом случае компилятор строго обратит наше внимание на ошибку:

```
Couldn't match type ‘(t0, t1, String, t2)’
               with ‘(UUID, FullName, Email, Age, DiseaseId)’
Expected type: Patient
  Actual type: (t0, t1, String, t2)
In the pattern: (_, _, email, _)
```

Оно и понятно: функция `patientEmail` использует образец, который уже некорректен. Вот почему при использовании паттерн матчинга следует быть внимательным.

На этом наше знакомство с кортежем считаю завершённым, в последующих главах мы будем использовать их периодически.

## Для любопытных

Для работы с элементами многоэлементных кортежей можно использовать готовые библиотеки, во избежание длинных паттерн матчинговых цепочек. Например, пакет [tuple](http://hackage.haskell.org/package/tuple). Он не входит в стандартную библиотеку и устанавливается отдельно. 

Для того чтобы в своем проекте использовать сторонний пакет, его нужно указать в качестве зависимости (секция `dependencies` конфига `package.yaml`), после чего собрать проект через `stack build`. Подробнее об этом [будет](/courses/haskell/chapters/haskell_chapter_0150/#block-add-dependency) в главе про Hackage и библиотеки. {#block-add-dependency}

И вот как выглядит импорт модуля `Data.Tuple.Select` из пакета `tuple`:


```haskell {.example_for_playground}
import Data.Tuple.Select

main :: IO ()
main = print (sel4 (123, 7, "hydra", "DC:4", 44, "12.04"))
```

Функция `sel4` из модуля `Data.Tuple.Select` извлекает четвёртый по счёту элемент кортежа, в данном случае строку `"DC:4"`. Там есть функции вплоть до `sel32`, авторы вполне разумно сочли, что никто, находясь в здравом уме и твёрдой памяти, не станет оперировать кортежами, состоящими из более чем 32 элементов.

Кроме того, мы и обновлять элементы кортежа можем:

```haskell {.example_for_playground}
import Data.Tuple.Update

main :: IO ()
main = print (upd2 2 ("si", 45))
```

```
("si",2)
```

Второй элемент кортежа изменился с `45` на `2`.

Для обновления кортежа в пакете `tuple` есть модуль `Data.Tuple.Update`. Он содержит функции вида `updN val tpl`, где `N` — номер элемента кортежа (начиная с 1), `val` — новое значение для элемента, `tpl` — сам кортеж. {.task_text}

Естественно, по причине неизменности кортежа, никакого обновления тут не происходит, но выглядит симпатично. {.task_text}

Подключите этот модуль. Затем допишите определение функции `f`, которая принимает целое число и кортеж. Она заменяет второй элемент кортежа на это число.  {.task_text}

```haskell {.task_source #haskell_chapter_0110_task_0050}
module Main where

f :: Int -> (String, Int) -> (String, Int)
-- Your code here

main :: IO ()
main = print (f 3 ("si", 45))
```
Примените функцию `upd2` к значению типа `Int` и кортежу `(String, Int)`. {.task_hint}
```haskell {.task_answer}
module Main where

import Data.Tuple.Update

f :: Int -> (String, Int) -> (String, Int)
f newVal tpl = upd2 newVal tpl

main :: IO ()
main = print (f 3 ("si", 45))
```
