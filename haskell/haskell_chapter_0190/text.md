# Глава 19. Наши типы

Вот мы и добрались до Второго Кита Haskell — до **Типов**. Конечно, мы работали с типами почти с самого начала, но вам уже порядком надоели все эти `Int` и `String`, не правда ли? Пришла пора познакомиться с типами куда ближе.

## Знакомство

Удивительно, но в Haskell очень мало встроенных типов, то есть таких, о которых компилятор знает с самого начала. Есть `Int`, есть `Double`, `Char`, ну и ещё несколько. Все же остальные типы, даже носящие статус стандартных, не являются встроенными в язык. Вместо этого они определены в стандартной или иных библиотеках, причём определены точно так же, как мы будем определять и наши собственные типы. А поскольку без своих типов написать сколь-нибудь серьёзное приложение у нас не получится, тема эта достойна самого пристального взгляда.

Определим тип `Transport` для двух известных протоколов транспортного уровня модели OSI:

```haskell
data Transport = TCP | UDP
```

Перед нами — очень простой, но уже наш собственный тип. Рассмотрим его внимательнее.

Ключевое слово `data` — это начало определения типа. Далее следует название типа, в данном случае `Transport`. Имя любого типа обязано начинаться с большой буквы. Затем идёт знак равенства, после которого начинается фактическое описание типа, его «тело». В данном случае оно состоит из двух простейших конструкторов. 

**Конструктор значения** (англ. data constructor) — это то, что строит значение данного типа. Здесь у нас два конструктора, `TCP` и `UDP`, каждый из которых строит значение типа `Transport`. Имя конструктора тоже обязано начинаться с большой буквы. Иногда для краткости конструктор значения называют просто конструктором.

Подобное определение легко читается:

```haskell
   data  Transport  =    TCP  |    UDP

-- тип   Transport  это  TCP  или  UDP
```

Теперь мы можем использовать тип `Transport`, то есть создавать значения этого типа и что-то с ними делать. Например, в `let`-выражении:

```haskell
  let protocol = TCP
```

Мы создали значение `protocol` типа `Transport`, использовав конструктор `TCP`. А можно и так:

```haskell
  let protocol = UDP
```

Хотя мы использовали разные конструкторы, тип значения `protocol` в обоих случаях один и тот же — `Transport`.

Расширить подобный тип предельно просто. Добавим новый протокол SCTP (Stream Control Transmission Protocol):

```haskell
data Transport = TCP | UDP | SCTP
```

Третий конструктор значения дал нам третий способ создать значение типа `Transport`.

## Значение-пустышка

Задумаемся: говоря о значении типа `Transport` — о чём в действительности идёт речь? Казалось бы, значения-то фактического нет: ни числа никакого, ни строки — просто три конструктора. Так вот они и есть значения. Когда мы пишем:

```haskell
  let protocol = SCTP
```

мы создаём значение типа `Transport` с конкретным содержимым в виде `SCTP`. Конструктор — это и есть содержимое. Данный вид конструктора называется **нульарным** (англ. nullary). Тип `Transport` имеет три нульарных конструктора. И даже столь простой тип уже может быть полезен нам:

```haskell  {.example_for_playground .example_for_playground_001}
checkProtocol :: Transport -> String
checkProtocol transport = case transport of
  TCP  -> "That's TCP protocol."
  UDP  -> "That's UDP protocol."
  SCTP -> "That's SCTP protocol."

main :: IO ()
main = putStrLn . checkProtocol $ TCP
```

В результате увидим:

```bash
That's TCP protocol.
```

Функция `checkProtocol` объявлена как принимающая аргумент типа `Transport`, а применяется она к значению, порождённому конструктором `TCP`. В данном случае конструкция `case-of` сравнивает аргумент с конструкторами. 

Именно поэтому нам не нужна функция `otherwise`, ведь никаким иным способом, кроме как с помощью трёх конструкторов, значение типа `Transport` создать невозможно, а значит, один из конструкторов гарантированно совпадёт.

Тип, состоящий только из нульарных конструкторов, называют ещё перечислением (англ. enumeration). Конструкторов может быть сколько угодно, в том числе один-единственный (хотя польза от подобного типа была бы невелика). Вот ещё один известный пример:

```haskell
data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
```

Здесь значение типа `Day` отражено одним из семи конструкторов. Обратите внимание на форматирование, когда ментальные «ИЛИ» выровнены строго под знаком равенства. Такой стиль вы встретите во многих реальных Haskell-проектах.

## Директива deriving

Что будет, если попытаться вывести значение нашего типа `Day` в консоль? Или сравнить одно значение этого типа с другим?

```haskell  {.example_for_playground}
module Main where

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday

main :: IO()
main = do
       print Wednesday
       print $ Wednesday > Tuesday
```

Компилятор сообщит об ошибке! И это логично, ведь он понятия не имеет, как приводить значения типа `Day` к строке или как их сравнивать между собой:

```
No instance for ‘Show Day’ arising from a use of ‘print’
...
No instance for ‘Ord Day’ arising from a use of ‘>’
```

Однако очень часто мы хотим, чтобы наши типы имели некое поведение по умолчанию, например для проверки на равенство, возможности сортировки, вывода в консоль. Ключевое слово `deriving` как раз используется, чтобы у пользовательского типа появилась такая возможность:

```haskell  {.example_for_playground}
module Main where

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday deriving (Eq, Ord, Show)

main :: IO()
main = do
       print Wednesday
       print $ Wednesday > Tuesday
```

Директива `deriving` позволяет сделать наш тип экземпляром одного или нескольких **классов типов.** Например, `Bounded`, `Enum`, `Eq`, `Ord`, `Read`, `Show`. Как следствие у типа появляется нужное нам поведение.

О классах типов мы поговорим в следующих главах. А пока кратко опишем, для чего именно нужен каждый из перечисленных классов типов:
- `Bounded` предназначен для типов, у значений которых есть максимальное и минимальное значение. 
- `Enum` полезен для типов, у значений которых можно определить предшествующие и последующие значения. Например, для типа `Day` мы вправе указать `deriving (Enum)`, а для типа `Transport` это было бы бессмысленно.
- `Eq` и `Ord` необходимы для сравнения и проверки значений на равенство.
- `Read` нужен, чтобы конвертировать строку в значение типа.
- `Show` выполняет обратную функцию: превращает значение типа в строку.


Заведите тип `Day`, перечисляющий дни недели.  {.task_text}

Заведите тип `WorkMode` с двумя конструкторами: `FiveDays` и `SixDays` для пятидневной и шестидневной рабочей недели.  {.task_text}

А теперь напишите функцию `workingDays`, которая принимает аргумент типа `WorkMode` и в зависимости от его значения возвращает список рабочих дней включая субботу или без нее.  {.task_text}

```haskell {.task_source #haskell_chapter_0190_task_0010}
module Main where

-- Your code here

main :: IO()
main = do
       print $ workingDays FiveDays
       print $ workingDays SixDays
```
Функция `workingDays` возвращает список типа `[Day]`, и в случае пятидневной рабочей недели, отражённой конструктором `FiveDays`, этот список сформирован пятью конструкторами, а в случае шестидневной — шестью конструкторами. {.task_hint}
```haskell {.task_answer}
module Main where

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday deriving (Show)

data WorkMode = FiveDays
              | SixDays

workingDays :: WorkMode -> [Day]
workingDays FiveDays = [Monday, 
                       Tuesday, 
                       Wednesday, 
                       Thursday, 
                       Friday]
workingDays SixDays = [Monday, 
                       Tuesday, 
                       Wednesday, 
                       Thursday, 
                       Friday, 
                       Saturday]
main :: IO()
main = do
       print $ workingDays FiveDays
       print $ workingDays SixDays
```

Заведите тип-перечисление `Theme` из трех значений: `Light`, `Dark`, `Default`. Оно характеризует цветовую тему некоей IDE. {.task_text}

Уже готова функция `getTheme`, которая по названию конкретной цветовой гаммы получает тему. Например, цветовая гамма "abyss" относится к темной теме. {.task_text}

Вам необходимо написать тело функции `countThemes`. Эта функция принимает два списка: {.task_text}
- Список строковых названий тем. Например, `["Light", "Default"]`.
- Список строк — названий цветовых гамм, выбранных различными пользователями.
 {.task_text}

Функция должна вернуть количество цветовых гамм, относящихся к заданным темам. Например, `countThemes ["Dark", "Default"] ["abyss", "solarized light", "default"]` вернет 2, то есть количество цветовых гамм из второго списка, которые соответствуют темам из первого списка.  {.task_text}

Чтобы преобразовать строку в значение типа-перечисления, воспользуйтесь функцией `read`.  {.task_text}

```haskell {.task_source #haskell_chapter_0190_task_0020}
module Main where

-- Define Theme type

getTheme :: String -> Theme
getTheme theme =
        case theme of
          "abyss" -> Dark
          "dracula" -> Dark
          "solarized light" -> Light
          "night blue" -> Dark
          _ -> Default

countThemes :: [String] -> [String] -> Int
countThemes themeNames themes = -- Your code here
            
main :: IO()
main = do
       print $ countThemes ["Dark", "Default"] 
                           ["default", 
                           "solarized light", 
                           "solarized light"]
       print $ countThemes ["Dark"] 
                           ["abyss", 
                           "dracula", 
                           "solarized light"]
       print $ countThemes ["Light", "Default"] 
                           ["high contrast", 
                           "solarized light", 
                           "solarized light",
                           "night blue"]
       print $ countThemes ["Light", "Dark"] 
                           ["abyss", 
                           "solarized light", 
                           "solarized light",
                           "night blue"]
```
Для того, чтобы значения типа `Theme` можно было читать из строки, писать в строку и сравнивать между собой, допишите к определению типа: `deriving (Eq, Read, Show)`. В теле функции вы можете завести блок `where` и в нем два выражения. Первое выражение — `targets`, являющееся применением функции `read` к каждому элементу `themeNames`. Второе выражение — `sources`. Это применение функции `getTheme` к каждому элементу `themes`. В теле функции останется отфильтровать `sources` по наличию элемента в `targets` и посчитать длину результирующего списка. {.task_hint}
```haskell {.task_answer}
module Main where

data Theme = Light | Dark | Default deriving (Eq, Read, Show)

getTheme :: String -> Theme
getTheme theme =
        case theme of
          "abyss" -> Dark
          "dracula" -> Dark
          "solarized light" -> Light
          "night blue" -> Dark
          _ -> Default

countThemes :: [String] -> [String] -> Int
countThemes themeNames themes = length $ 
                                filter (\x -> elem x targets) sources
            where 
              targets = map read themeNames
              sources = map getTheme themes
            
main :: IO()
main = do
       print $ countThemes ["Dark", "Default"] 
                           ["default", 
                           "solarized light", 
                           "solarized light"]
       print $ countThemes ["Dark"] 
                           ["abyss", 
                           "dracula", 
                           "solarized light"]
       print $ countThemes ["Light", "Default"] 
                           ["high contrast", 
                           "solarized light", 
                           "solarized light",
                           "night blue"]
       print $ countThemes ["Light", "Dark"] 
                           ["abyss", 
                           "solarized light", 
                           "solarized light",
                           "night blue"]
```

Польза от типов, сформированных нульарными конструкторами, не очень велика, хотя встречаться с такими типами вы будете часто.

Приоткрою секрет: новый тип можно определить не только с помощью ключевого слова `data`, но об этом [узнаем](/courses/haskell/chapters/haskell_chapter_0220/) в одной из следующих глав.

А теперь мы можем познакомиться с типами куда более полезными.

