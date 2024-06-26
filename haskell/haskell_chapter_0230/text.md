# Глава 23. Конструктор типа

В предыдущих главах мы познакомились с АТД, которые сами по себе уже весьма полезны. И всё же есть в них одно ограничение: они напрочь лишены гибкости. Вот тебе конкретные поля, а вот тебе конкретные типы, будь счастлив. Но существует способ наделить наши тип куда большей силой. 

Эта глава станет для нас переломной, ведь с неё начнётся наш путь в мир действительно мощных типов.

## Опциональный тип

Допустим, у нас есть список пар следующего вида:

```haskell
type Chapters = [(FilePath, String)]

chapters :: Chapters
chapters = [ ("/list.html",  "Список")
           , ("/tuple.html", "Кортеж")
           , ("/hof.html",   "ФВП")
           ]
```

Тип `FilePath` есть не более чем стандартный синоним для типа `String`, но он более информативен. Итак, этот список содержит названия трёх глав книги и пути к ним. И вот понадобилась нам функция, которая извлекает название главы по её пути:

```haskell
lookupChapterNameBy :: FilePath -> Chapters -> String
lookupChapterNameBy _ [] = ""  -- Так ничего и не нашли...
lookupChapterNameBy path ((realPath, name) : others)
  | path == realPath = name -- Пути совпадают, вот вам имя.
  | otherwise        = lookupChapterNameBy path others
```

Всё предельно просто: рекурсивно бежим по списку пар `chapters`, на каждом шаге извлекая через паттерн матчинг путь ко главе и её имя. Сравниваем пути и, ежели совпадают — на выходе получается имя, соответствующее заданному пути. Если же, пройдя весь список, мы так и не нашли соответствующего пути, на выходе будет пустая строка.

Используем так:

```haskell
main :: IO ()
main = putStrLn $
  if | null name -> "No such chapter, sorry..."
     | otherwise -> "This is chapter name: " ++ name
  where
    name = lookupChapterNameBy "/tuple.html" chapters
```

Если на выходе функции `lookupChapterNameBy` пустая строка, значит мы ничего не нашли, в противном же случае показываем найденное имя.

Ну и как вам такое решение? Вроде бы красивое, но почему, собственно, пустая строка? Я вполне мог написать заготовку для очередной главы и ещё не дать ей имя:

```haskell
chapters :: Chapters
chapters = [ ("/list.html",  "Список")
           , ("/tuple.html", "Кортеж")
           , ("/hof.html",   "ФВП")
           , ("/monad.html", "")  -- Заготовка
           ]
```

В этом случае наше решение ломается: пустая строка на выходе функции `lookupChapterNameBy` может означать теперь как то, что мы не нашли главы с таким путём, так и то, что глава-то существует, просто её имя пока не задано. Следовательно, нам нужен другой механизм проверки результата поиска, более однозначный.

Определим опциональный тип. Опциональным (англ. optional) называют такой тип, внутри которого либо есть нечто полезное, либо нет. Выглядеть он будет так:

```haskell
data Optional = NoSuchChapter
              | Chapter String
```

Если значение типа `Optional` создано с помощью нульарного конструктора `NoSuchChapter`, это означает, что внутри ничего нет, перед нами значение-пустышка. Это и будет соответствовать тому случаю, когда нужную главу мы не нашли. А вот если значение было создано с помощью унарного конструктора `Chapter`, это несомненно будет означать то, что мы нашли интересующую нас главу. 

Перепишем функцию `lookupChapterNameBy`:

```haskell
lookupChapterNameBy :: FilePath -> Chapters -> Optional
lookupChapterNameBy _ [] = NoSuchChapter -- Пустышка
lookupChapterNameBy path ((realPath, name) : others)
  | path == realPath = Chapter name      -- Реальное имя
  | otherwise        = lookupChapterNameBy path others
```

Код стал более понятным. И вот как мы будем работать с этой функцией:

```haskell
main :: IO ()
main = putStrLn $
  case result of
    NoSuchChapter -> "No such chapter, sorry..."
    Chapter name  -> "This is chapter name: " ++ name
  where
    result = lookupChapterNameBy "/tuple.html" chapters
```

Отныне функция `lookupChapterNameBy` сигнализирует о неудачном поиске не посредством пустой строки, а посредством нульарного конструктора. Это и надёжнее, и читабельнее.

Красиво, но в этом элегантном решении всё-таки остаётся один изъян: оно намертво привязано к типу `String`:

```haskell
data Optional = NoSuchChapter
              | Chapter String

--                      Почему
--                      именно
--                      String?
```

В самом деле, почему? Например, в Haskell широкое применение получил тип `Text` из одноимённого пакета. Этот тип, кстати, значительно мощнее и эффективнее стандартной `String`. Значит, если мы захотим определить опциональный тип и для `Text`, придётся дублировать:

```haskell
data Optional = NoSuchChapter | Chapter String

data Optional = NoSuchChapter | Chapter Text
```

Однако компилятор наотрез откажется принимать такой код:

```
Multiple declarations of ‘Optional’
```

Имена-то типов одинаковые! Хорошо, уточним:

```haskell
data OptionalString = NoSuchChapter | Chapter String

data OptionalText   = NoSuchChapter | Chapter Text
```

Но и в этом случае компиляция не пройдёт:

```
Multiple declarations of ‘NoSuchChapter’

...

Multiple declarations of ‘Chapter’
```

Конструкторы значений тоже одноимённые, опять уточняем:

```haskell
data OptionalString = NoSuchChapterString
                    | ChapterString String

data OptionalText   = NoSuchChapterText
                    | ChapterText Text
```

Вот теперь это работает, но код стал избыточным. А вдруг мы пожелаем добавить к двум строковым типам ещё и третий? Или четвёртый? Что ж нам, для каждого типа вот так вот уточнять? Нет, умный в гору не пойдёт — есть лучший путь.

## Может быть

В стандартной библиотеке живёт тип по имени `Maybe`:

```haskell
data Maybe a = Nothing | Just a
```

Тип `Maybe` (от англ. maybe, «может быть») нужен для создания тех самых опциональных значений. Впрочем, я выразился неточно, ведь, несмотря на ключевое слово `data`, `Maybe` — это не совсем тип, это конструктор типа (англ. type constructor). Данная концепция используется в Haskell чрезвычайно часто, и, как и большинство концепций в этом языке, она столь полезна потому, что очень проста.

**Конструктор типа** — это то, что создаёт новый тип (потенциально, бесконечное множество типов). Когда мы явно определяем тип, он прямолинеен и однозначен:

```haskell
data Optional = NoSuchChapter | Chapter      String

--   имя типа   нульарный       унарный      поле
--              конструктор     конструктор  типа
--              значения        значения     String
```

Когда же мы определяем конструктор типа, мы создаём концептуальный скелет для будущих типов. Взглянем ещё раз (к-тор — это конструктор, для краткости):

```haskell
--            ______________________________
--           /                              `v

data Maybe  a        = Nothing   | Just      a
--   к-тор  типовая    нульарный   унарный   поле
--   типа   заглушка   к-тор       к-тор     типа
--                     значения    значения  a
```

Здесь присутствует [уже знакомая нам](/courses/haskell/chapters/haskell_chapter_0140#block-polymorphic-types) типовая заглушка `a`, она-то и делает `Maybe` конструктором типа. Как мы помним, на место типовой заглушки всегда встаёт какой-то тип.

Перепишем функцию `lookupChapterNameBy` для работы с `Maybe`:

```haskell
lookupChapterNameBy :: FilePath -> Chapters -> Maybe String
lookupChapterNameBy _ [] = Nothing  -- Пустышка
lookupChapterNameBy path ((realPath, name) : others)
  | path == realPath = Just name    -- Реальное имя
  | otherwise        = lookupChapterNameBy path others
```

Рассмотрим обновлённое объявление:

```haskell
lookupChapterNameBy :: FilePath
                    -> Chapters -> Maybe String

--                                 это тип такой,
--                                 называется
--                                 Maybe String
```

На выходе видим значение типа `Maybe String`. Этот тип был порождён конструктором `Maybe`, применённым к типу `String`. Стоп, я сказал «применённым»? Да, именно так: вы можете воспринимать конструктор типа как особую «функцию», назовём её «типовая функция». Нет, это не официальный термин из Haskell, это просто аналогия: обычная функция работает с данными, а типовая функция работает с типами. Сравните это:

```haskell
   length   [1, 2, 3] = 3

-- функция  данное    = другое данное
```

и это:

```haskell
   Maybe    String    = Maybe String

-- типовая  тип       = другой тип
-- функция
```

Применение конструктора типа к существующему типу порождает некий новый тип, и это очень мощная техника, используемая в Haskell почти на каждом шагу. Например, если нам нужно завернуть в опциональное значение уже не `String`, а ранее упомянутый `Text`, мы ничего не должны менять в конструкторе `Maybe`:

```haskell
   Maybe    Text = Maybe Text

-- типовая  тип  = другой тип
-- функция
```

Какой тип подставляем на место `a`, такой тип и станет опциональным. В этом и заключается красота конструкторов типов, ведь они дают нам колоссальный простор для творчества.

В главе про рекурсию [одной из задач было](/courses/haskell/chapters/haskell_chapter_0160#block-task-indexof) написать функцию `indexOf` от двух аргументов: целого числа и списка.  Функция должна была вернуть индекс первого вхождения заданного значения в список, либо -1, если оно не найдено.  {.task_text}

Перепишите функцию так, чтобы вместо -1 она возвращала **опциональный** индекс. {.task_text}

```haskell {.task_source #haskell_chapter_0230_task_0010}
module Main where

indexOf :: Int -> [Int] -> Int
indexOf val lst = findVal 0 lst
  where
    findVal :: Int -> [Int] -> Int
    findVal _ [] = -1
    findVal index (h:t)
     | val == h = index
     | otherwise = findVal (index + 1) t
     

main :: IO ()
main = do
       print $ indexOf 5 []
       print $ indexOf 5 [1, 2, 3]
       print $ indexOf 5 [5]
       print $ indexOf 5 [5, 10]
       print $ indexOf 5 [1, 5]
       print $ indexOf 5 [1, 5, 5]
       print $ indexOf 5 [1, 2, 3, 4, 5, 6]
```
В объявлении функции используйте ключевое слово `Maybe`. Вместо -1 используйте `Nothing`. А при возвращении индекса не забудьте ключевое слово `Just`. {.task_hint}
```haskell {.task_answer}
module Main where

indexOf :: Int -> [Int] -> Maybe Int
indexOf val lst = findVal 0 lst
  where
    findVal :: Int -> [Int] -> Maybe Int
    findVal _ [] = Nothing
    findVal index (h:t)
     | val == h = Just index
     | otherwise = findVal (index + 1) t
     

main :: IO ()
main = do
       print $ indexOf 5 []
       print $ indexOf 5 [1, 2, 3]
       print $ indexOf 5 [5]
       print $ indexOf 5 [5, 10]
       print $ indexOf 5 [1, 5]
       print $ indexOf 5 [1, 5, 5]
       print $ indexOf 5 [1, 2, 3, 4, 5, 6]
```

А теперь мы подошли к очень важной теме.

## Этажи

Что такое тип `Maybe String`? Да, мы уже знаем, это АТД. Но что это такое по сути? Зачем мы конструируем сложные типы из простых? Я предлагаю вам аналогию, которая поможет нам взглянуть на этот вопрос несколько иначе. Эта аналогия отнюдь не аксиома, просто я нашёл её полезной для себя самого. Думаю, вам она тоже будет полезна. Конечно, предлагать аналогии — дело неблагодарное, ведь любая из них несовершенна и может быть так или иначе подвергнута критике. Поэтому не воспринимайте мою аналогию как единственно верную.

С точки зрения типов любую Haskell-программу можно сравнить с многоэтажным домом. И вот представьте, мы смотрим на этот дом со стороны.

На самом нижнем этаже расположены простейшие стандартные типы, такие как `Int`, `Double`, `Char` или список. Возьмём, например, тип `Int`. Что это такое? Целое число. Оно не несёт в себе никакого смысла, это всего лишь число в вакууме. Или вот строка — что она такое? Это просто набор каких-то символов в том же вакууме, и ничего более. И если бы мы были ограничены лишь этими типами, наша программистская жизнь была бы весьма грустной.

А вот на втором и последующих этажах живут типы куда более интересные. Например, на одном из этажей живёт тип `Maybe String`. При создании типа `Maybe String` происходит важное событие: мы поднимаемся с первого на более высокий этаж. Считайте эти этажи уровнями абстракции. Если тип `String` — это всего лишь безликая строка, то тип `Maybe String` — это уже не просто строка, это опциональная строка, или, если хотите, строка, наделённая опциональностью. Подняться на тот или иной этаж в нашем типовом небоскрёбе — это значит взять более простой тип и наделить его новым смыслом, новыми возможностями.

Или вот вспомним тип `IPAddress`:

```haskell
data IPAddress = IPAddress String
```

Мы опять-таки взяли ничего не значащую строку и подняли её на этаж под названием `IPAddress`, и теперь это уже не просто какая-то строка, это IP-адрес. Новый тип наделил бессмысленную строку вполне определённым смыслом. А когда мы вытаскиваем внутреннюю строку из `IPAddress` с помощью паттерн матчинга, мы вновь оказываемся на первом этаже.

А вот ещё наш тип, `EndPoint`:

```haskell
data EndPoint = EndPoint IPAddress Int
```

Тут мы поднялись ещё чуток: сначала подняли строку на этаж IP-адреса, а затем взяли его и тип `Int` и подняли их на следующий этаж под названием `EndPoint`, и на этом этаже перед нами уже не просто какой-то IP-адрес и какое-то число, перед нами уже связанные друг с другом адрес и порт.

А вот ещё один пример, знакомство с которым я откладывал до сих пор. Вспомним определение главной функции `main`: {#block-io}

```haskell
main :: IO ()
```

Я [обещал рассказать](/courses/haskell/chapters/haskell_chapter_0050#block-io) о том, что такое `IO`, и вот теперь рассказываю: `IO` — это тоже конструктор типа. Правда, конструктор особенный, непохожий на наши `IPAddress` или `EndPoint`, но об этом подробнее в следующих главах. Так вот поднявшись на этаж под названием `IO`, мы получаем очень важную способность — способность взаимодействовать с внешним миром: файл прочесть, на консоль текст вывести, и в том же духе. И потому тип `IO String` — это уже не просто невесть откуда взявшаяся строка, но строка, полученная из внешнего мира (например, из файла). 

И единственная возможность наделить наши функции способностью взаимодействовать с внешним миром — поднять (ну или опустить) их на `IO`-этаж. Вот так и получается: в процессе работы программы мы постоянно прыгаем в лифт и переезжаем с одного типового этажа на другой.

Но запомните: не все этажи одинаковы! Не со всякого этажа можно напрямую попасть на любой другой. Более того, есть такие этажи, оказавшись на котором, мы в конечном итоге обязаны на него и вернуться. Понимаю, сейчас это порождает больше вопросов, нежели ответов, но не беспокойтесь: ответы ждут нас в последующих главах.

