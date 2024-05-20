# Глава 15. Hackage и библиотеки

Ранее я уже упоминал о библиотеках, пришло время познакомиться с ними поближе, ведь в последующих главах мы будем использовать их постоянно.

## Библиотеки большие и маленькие

За годы существования Haskell разработчики со всего мира создали множество библиотек. Библиотеки избавляют нас от необходимости вновь и вновь писать то, что уже написано до нас. Для любого живого языка программирования написано множество библиотек. В мире Haskell их, конечно, не такая туча, как для той же Java, но порядочно: стабильных есть не менее двух тысяч, многие из которых очень качественные и уже многократно испытаны в серьёзных проектах.

С модулями — файлами, содержащими Haskell-код, — мы уже знакомы, они являются основным кирпичом любого Haskell-проекта. Библиотека, также являясь Haskell-проектом, тоже состоит из модулей (не важно, из одного или из сотен). Поэтому использование библиотеки сводится к использованию входящих в неё модулей. И мы уже неоднократно делали это в предыдущих главах.

Вспомним [пример из главы про ФВП:](/courses/haskell/chapters/haskell_chapter_0140#block-data-char)

```haskell
import Data.Char

toUpperCase :: String -> String
toUpperCase str = map toUpper str

main :: IO ()
main = putStrLn . toUpperCase $ "haskell.org"
```

Функция `toUpper` определена в модуле `Data.Char`, который, в свою очередь, живёт в стандартной библиотеке. Библиотек есть множество, но стандартная лишь одна. Она содержит самые базовые, наиболее широко используемые инструменты. А прежде чем продолжить, зададимся важным вопросом: «Где живут все эти библиотеки?» Они живут в разных местах, но главное из них — Hackage.

## Hackage

Hackage — это центральный репозиторий Haskell-библиотек, или, как принято у нас называть, пакетов (англ. package). Название репозитория происходит от слияния слов `Haskell` и `package`. Hackage существует с 2008 года, вот его адрес: 

[http://hackage.haskell.org/](http://hackage.haskell.org/)

Ранее упомянутая стандартная библиотека тоже находится в Hackage и называется она `base`. Каждой библиотеке выделена своя страница.

Каждый из Hackage-пакетов живёт по адресу, сформированному по неизменной схеме: 

`http://hackage.haskell.org/package/ИМЯПАКЕТА`

Так, дом стандартной библиотеки: 

[http://hackage.haskell.org/package/base](http://hackage.haskell.org/package/base)

Hackage — открытый репозиторий: любой разработчик может добавить туда свои пакеты.

Стандартная библиотека включает в себя более сотни модулей, но есть среди них самый известный, носящий имя `Prelude`. Этот модуль по умолчанию всегда с нами: всё его содержимое автоматически импортируется во все модули нашего проекта. Например, уже известные нам `map` или операторы конкатенации списков живут в модуле `Prelude`, поэтому доступны нам всегда. Помимо них (и многих-многих десятков других функций) в `Prelude` располагаются функции для работы с вводом-выводом, такие как наши знакомые `putStrLn` и `print`.

Hackage весьма большой, поэтому искать пакеты можно двумя способами. Первый — на единой странице всех пакетов. Здесь перечислены все пакеты, а для нашего удобства они расположены по тематическим категориям:

[http://hackage.haskell.org/packages/](http://hackage.haskell.org/packages/)

Второй способ — через специальный поисковик [Hoogle.](http://www.haskell.org/hoogle/) Он скрупулёзно просматривает внутренности Hackage, и вы будете часто им пользоваться как обычным поисковиком. Например, знаем мы имя функции, а в каком пакете/модуле она живёт — забыли. Вбиваем в поиск — получаем результаты.

Чтобы воспользоваться пакетом в нашем проекте, нужно для начала включить его в наш проект. В главе про кортежи мы уже [проделали](/courses/haskell/chapters/haskell_chapter_0110#block-add-dependency) это с пакетом `tuple`. А теперь для примера рассмотрим пакет `text`, предназначенный для работы с текстом. Он нам в любом случае понадобится, поэтому включим его в наш проект незамедлительно. {#block-add-dependency}

Для этого в секцию `dependencies` конфига `package.yaml` добавляем новую запись:

```
dependencies:
- base >= 4.7 && < 5
- tuple
```

Далее выполняем сборку проекта, чтобы stack обнаружил новую зависимость:

```shell
stack build
```

Помните, когда мы впервые настраивали проект, я упомянул, что утилита `stack` умеет ещё и библиотеки устанавливать? Она увидит новую зависимость нашего проекта и установит как сам пакет `text`, так и все те пакеты, от которых, в свою очередь, зависит пакет `text`. 

Помимо установки пакета stack также обновит сборочный файл проекта `ИМЯПРОЕКА.cabal`: в секцию `executable ИМЯПРОЕКТА` в поле `build-depends` через запятую он допишет имя пакета:

```haskell
  build-depends:   base  -- Уже здесь!
                 , real
                 , text  -- А это новый пакет.
```

Файл с расширением `.cabal` — это обязательный сборочный файл Haskell-проекта. Он содержит главные инструкции, касающиеся сборки проекта. С синтаксисом сборочного файла мы будем постепенно знакомиться в следующих главах.

После сборки проекта мы можем импортировать модули из этого пакета в наши модули. И теперь пришла пора узнать, как это можно делать.

## Иерархия в имени

Когда мы пишем:

```haskell
import Data.Char
```

в имени модуля отражена иерархия пакета. `Data.Char` означает, что внутри пакета `base` есть каталог `Data`, внутри которого живёт файл `Char.hs`, открыв который, мы увидим:

```haskell
module Data.Char
...
```

Таким образом, точка в имени модуля отражает файловую иерархию внутри данного пакета. Можете воспринимать эту точку как слэш в Unix-пути. Есть пакеты со значительно более длинными именами, например:

```haskell
module GHC.IO.Encoding.UTF8
```

Соответственно, имена наших собственных модулей тоже отражают место, в котором они живут. Так, один из модулей в моём рабочем проекте носит название `Common.Performers.Click`. Это означает, что живёт этот модуль здесь: `src/Common/Performers/Click.hs`.

## Лицо

Вернёмся к нашему примеру:

```haskell
import Data.Char
```

Импорт модуля `Data.Char` делает доступным для нас всё то, что включено в интерфейс этого модуля. Откроем [наш собственный](/courses/haskell/chapters/haskell_chapter_0020/#block-project-structure) модуль `Lib`:

```haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

Имя функции `someFunc` упомянуто в интерфейсе модуля, а именно между круглыми скобками, следующими за именем модуля. Чуток переформатируем скобки:

```haskell
module Lib (
    someFunc
) where
```

В настоящий момент только функция `someFunc` доступна всем импортёрам данного модуля. Если же мы определим в этом модуле другую функцию `anotherFunc`:

```haskell
module Lib (
    someFunc
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

anotherFunc :: String -> String
anotherFunc s = s ++ "!"
```

она останется невидимой для внешнего мира, потому что её имя не упомянуто в интерфейсе модуля. И если в модуле `Main` мы напишем так:

```haskell
module Main

import Lib

main :: IO ()
main = putStrLn . anotherFunc $ "Hi"
```

компилятор справедливо ругнётся, мол, не знаю функцию `anotherFunc`. Если же мы добавим её в интерфейс модуля `Lib`:

```haskell
module Lib (
    someFunc,
    anotherFunc
) where
```

тогда функция `anotherFunc` тоже станет видимой всему миру. Интерфейс позволяет нам показывать окружающим лишь то, что мы хотим им показать, оставляя служебные внутренности нашего модуля тайной за семью печатями.

## Импортируем по-разному

В реальных проектах мы импортируем множество модулей из различных пакетов. Иногда это является причиной конфликтов, с которыми приходится иметь дело.

Вспомним функцию `putStrLn`: она существует не только в незримом модуле `Prelude`, но и в модуле `Data.Text.IO` из пакета `text`:

```haskell
-- Здесь тоже есть функция по имени putStrLn.
import Data.Text.IO

main :: IO ()
main = putStrLn ...  -- И откуда эта функция?
```

При попытке скомпилировать такой код мы упрёмся в ошибку:

```
Ambiguous occurrence ‘putStrLn’
It could refer to either ‘Prelude.putStrLn’,
                         imported from ‘Prelude’ ...
                      or ‘Data.Text.IO.putStrLn’,
                         imported from ‘Data.Text.IO’ ...
```

Нам необходимо как-то указать, какую из функций `putStrLn` мы имеем в виду. Это можно сделать несколькими способами.

Можно указать принадлежность функции конкретному модулю. Из сообщения об ошибке уже видно, как это можно сделать:

```haskell
-- Здесь тоже есть функция по имени putStrLn.
import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn ...  -- Сомнений нет!
```

Теперь уже сомнений не осталось: используемая нами `putStrLn` принадлежит модулю `Data.Text.IO`, поэтому коллизий нет.

Впрочем, не кажется ли вам подобная форма слишком длинной? В упомянутом ранее стандартном модуле `GHC.IO.Encoding.UTF8` есть функция `mkUTF8`, и представьте себе:

```haskell
import GHC.IO.Encoding.UTF8

main :: IO ()
main =
  let enc = GHC.IO.Encoding.UTF8.mkUTF8 ...
```

Слишком длинно, нужно укоротить. Импортируем модуль под коротким именем `TIO`:

```haskell
import Data.Text.IO as TIO

main :: IO ()
main = TIO.putStrLn ...
```

Вот, так значительно лучше. Короткое имя может состоять даже из одной буквы, но как и полное имя модуля, оно обязательно должно начинаться с большой буквы, поэтому:

```haskell
import Data.Text.IO as tIO  -- Ошибка
import Data.Text.IO as i    -- Тоже ошибка
import Data.Text.IO as I    -- Порядок!
```

Импортируйте модуль `Data.Tuple.Update` под коротким именем `U`. {.task_text}

```haskell {.task_source #haskell_chapter_0150_task_0010}
module Main where

main :: IO ()
main = print $ upd3 6.0 (1.0, 2.0, 3.0, 4.0)
```
Синтаксис: `import Имя.Модуля as КороткоеИмя`. {.task_hint}
```haskell {.task_answer}
module Main where

import Data.Tuple.Update as U

main :: IO ()
main = print $ upd3 6.0 (1.0, 2.0, 3.0, 4.0)
```

Иногда, для большего порядка, используют qualified-импорт:

```haskell
import qualified Data.Text.IO as TIO
```

Ключевое слово `qualified` используется для «строгого» включения модуля: в этом случае мы обязаны указывать принадлежность к нему. 


Используйте строгое включение модуля `Data.Text` и исправьте в соответствии с этим код функции `main`. {.task_text}

```haskell {.task_source #haskell_chapter_0150_task_0020}
{-# LANGUAGE OverloadedStrings #-}

module Main where

main :: IO ()
main = 
       let s :: Text
           s = "lisp"
       in
           print $ justifyLeft 7 '.' s
```
Импортируйте модуль: `import qualified Data.Text as T` Затем укажите имя `T` в двух местах: `let s :: T.Text` и `T.justifyLeft`. {.task_hint}
```haskell {.task_answer}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main :: IO ()
main = 
       let s :: T.Text
           s = "lisp"
       in
           print $ T.justifyLeft 7 '.' s
```

Даже несмотря на то, что функция `justifyLeft` есть только в модуле `Data.Text` и никаких коллизий с `Prelude` нет, мы обязаны указать, что эта функция именно из `Data.Text`. В больших модулях qualified-импорт бывает полезен: с одной стороны, гарантированно не будет никаких конфликтов, с другой, мы сразу видим, откуда родом та или иная функция.

Впрочем, некоторым Haskell-программистам любое указание принадлежности к модулю кажется избыточным. Поэтому они идут по другому пути: выборочное включение/выключение. Например:

```haskell
import Data.Char
import Data.Text (pack)  -- Только её!

main :: IO ()
main = putStrLn $ map toUpper "haskell.org"
```

Мы подразумеваем стандартную функцию `map`, однако в модуле `Data.Text` тоже содержится функция по имени `map`. К счастью, никакой коллизии не будет, ведь мы импортировали не всё содержимое модуля `Data.Text`, а лишь одну его функцию `pack`.

Если же мы хотим импортировать две или более функции, перечисляем их через запятую:

```haskell
import Data.Text (pack, unpack)
```

Перепишите этот импорт на импорт только необходимых определений. {.task_text}

```haskell {.task_source #haskell_chapter_0150_task_0030}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

main :: IO ()
main = 
       let s :: T.Text
           s = "lisp"
       in
           print $ T.justifyLeft 7 '.' s
```
Синтаксис: `import Имя.Модуля (определение1, определение2, ...)`. {.task_hint}
```haskell {.task_answer}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, justifyLeft)

main :: IO ()
main = 
       let s :: Text
           s = "lisp"
       in
           print $ justifyLeft 7 '.' s
```

Существует и прямо противоположный путь: вместо выборочного включения — выборочное выключение. Избежать коллизии между функциями `putStrLn` можно было бы и так:

```haskell
import Data.Text.IO hiding (putStrLn)

main :: IO ()
main = putStrLn ...  -- Сомнений нет: из Prelude.
```

Слово `hiding` позволяет скрывать кое-что из импортируемого модуля. Можно и несколько функций скрыть:

```haskell
import Data.Text.IO hiding ( readFile
                           , writeFile
                           , appendFile
                           )
```

Скройте функцию `putStrLn` из `Prelude`, чтобы появилась возможность использования `putStrLn` из `Data.Text.IO`. {.task_text}

```haskell {.task_source #haskell_chapter_0150_task_0040}
import Data.Text.IO

main :: IO ()
main = let 
       -- Определяем, но не используем:
       f = \text -> putStrLn text
       in
         print 1
```
Синтаксис: `import Имя.Модуля hiding (определение1, определение2, ...)`. {.task_hint}
```haskell {.task_answer}
import Prelude hiding (putStrLn)
import Data.Text.IO

main :: IO ()
main = let 
       -- Определяем, но не используем:
       f = \text -> putStrLn text
       in
         print 1
```

## Оформление

Общая рекомендация такова — оформляйте так, чтобы было легче читать. В реальном проекте в каждый из ваших модулей будет импортироваться довольно много всего. Вот кусочек из одного моего рабочего модуля:

```haskell
import qualified Test.WebDriver.Commands    as WDC
import           Test.WebDriver.Exceptions
import qualified Data.Text                  as T
import           Data.Maybe                 (fromJust)
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Control.Monad              (void)
```

Как полные, так и краткие имена модулей выровнены, такой код проще читать и изменять. Не все программисты согласятся с таким стилем, но попробуем убрать выравнивание:

```haskell
import qualified Test.WebDriver.Commands as WDC
import Test.WebDriver.Exceptions
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad (void)
```

Теперь код выглядит скомканным, его труднее воспринимать. Впрочем, выбор за вами.

