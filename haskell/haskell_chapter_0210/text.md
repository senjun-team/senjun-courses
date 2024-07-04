# Глава 21. АТД: поля с метками

Обсудим, для чего нужно снабжать поля метками (англ. label).

## Проблема
Многие типы в реальных проектах довольно велики. Взгляните:

```haskell
data Arguments = Arguments Port
                           Endpoint
                           RedirectData
                           FilePath
                           FilePath
                           Bool
                           FilePath
```

Значение типа `Arguments` хранит в своих полях некоторые значения, извлечённые из параметров командной строки, с которыми запущена одна из моих программ. И всё бы хорошо, но работать с таким типом абсолютно неудобно. Он содержит семь полей, и паттерн матчинг был бы слишком громоздким, представьте себе:

```haskell
...
  where
    Arguments _ _ _ redirectLib _ _ xpi = arguments
```

Более того, когда мы смотрим на определение типа, назначение его полей остаётся тайной за семью печатями. Видите предпоследнее поле? Оно имеет тип `Bool` и, понятное дело, отражает какой-то флаг. Но что это за флаг, читатель не представляет. К счастью, существует способ, спасающих нас от обеих этих проблем.

## Метки

Мы можем снабдить наши поля метками. Вот как это выглядит:

```haskell
data Arguments = Arguments { runWDServer    :: Port
                           , withWDServer   :: Endpoint
                           , redirect       :: RedirectData
                           , redirectLib    :: FilePath
                           , screenshotsDir :: FilePath
                           , noScreenshots  :: Bool
                           , harWithXPI     :: FilePath
                           }
```

Теперь назначение меток куда понятнее. Схема определения такова:

```haskell
  data Arguments = Arguments   { runWDServer :: Port }

--  тип  такой-то    конструктор   метка поля     тип
--                                               поля
```

Теперь поле имеет не только тип, но и название, что и делает наше определение значительно более читабельным. Поля в этом случае разделены запятыми и заключены в фигурные скобки.

Если подряд идут два или более поля одного типа, его можно указать лишь для последней из меток. Так, если у нас есть вот такой тип:

```haskell
data Patient = Patient { firstName :: String
                       , lastName  :: String
                       , email     :: String
                       }
```

его определение можно чуток упростить и написать так:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email     :: String
                       }
```

Раз тип всех трёх полей одинаков, мы указываем его лишь для последней из меток. Ещё пример полной формы:

```haskell
data Patient = Patient { firstName    :: String
                       , lastName     :: String
                       , email        :: String
                       , age          :: Int
                       , diseaseId    :: Int
                       , isIndoor     :: Bool
                       , hasInsurance :: Bool
                       }
```

и тут же упрощаем:

```haskell
data Patient = Patient { firstName
                       , lastName
                       , email        :: String
                       , age
                       , diseaseId    :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }
```

Поля `firstName`, `lastName` и `email` имеют тип `String`, поля `age` и `diseaseId` — тип `Int`, и оставшиеся два поля — тип `Bool`.

## Getter и Setter?

Что же представляют собой метки? Фактически, это особые функции, сгенерированные автоматически. Эти функции имеют три предназначения:
- создавать,
- извлекать,
- изменять. 

Да, я не оговорился, изменять. Но об этом чуть позже, пусть будет маленькая интрига.

Вот как мы создаём значение типа `Patient`:

```haskell    {.example_for_playground .example_for_playground_001}
main :: IO ()
main = print $ diseaseId patient
  where
    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

Метки полей используются как своего рода setter (от англ. set, «устанавливать»):

```haskell
   patient = Patient { firstName    =      "John"

-- в этом    типа      поле с
-- значении  Patient   этой меткой  равно  этой строке
```

Кроме того, метку можно использовать и как getter (от англ. get, «получать»):

```haskell
main = print $ diseaseId  patient

--             метка как  аргумент
--             функции
```

Мы применяем метку к значению типа `Patient` и получаем значение соответствующего данной метке поля. Поэтому для получения значений полей нам уже не нужен паттерн матчинг.

Но что же за интригу я приготовил под конец? Выше я упомянул, что метки используются не только для задания значений полей и для их извлечения, но и для изменения. Вот что я имел в виду:

```haskell   {.example_for_playground .example_for_playground_002}
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

При запуске программы получим:

```haskell
j.d@gmail.com
```

Но постойте, что же тут произошло? Ведь в Haskell, как мы знаем, нет оператора присваивания, однако значение поля с меткой `email` поменялось. Помню, когда я впервые увидел подобный пример, то очень удивился, мол, уж не ввели ли меня в заблуждение по поводу неизменности значений в Haskell?!

Нет, не ввели. Подобная запись:

```haskell
patientWithChangedEmail = patient {
  email = "j.d@gmail.com"
}
```

действительно похожа на изменение поля через присваивание ему нового значения, но в действительности никакого изменения не произошло. Когда я назвал метку setter-ом, я немного слукавил, ведь классический setter из мира ООП был бы невозможен в Haskell. Посмотрим ещё раз внимательнее:

```haskell
...
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }
```

Взгляните, ведь у нас теперь два значения типа `Patient`, `patient` и `patientWithChangedEmail`. Эти значения не имеют друг ко другу ни малейшего отношения. Вспомните, как я говорил, что в Haskell нельзя изменить имеющееся значение, а можно лишь создать на основе имеющегося новое значение. Это именно то, что здесь произошло: мы взяли имеющееся значение `patient` и на его основе создали уже новое значение `patientWithChangedEmail`, значение поля `email` в котором теперь другое. Понятно, что поле `email` в значении `patient` осталось неизменным.

Будьте внимательны при инициализации значения с полями: вы обязаны предоставить значения для всех полей. Если вы напишете так:

```haskell   {.example_for_playground .example_for_playground_003}
main :: IO ()
main = print $ email patientWithChangedEmail
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"  -- Изменяем???
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
    }

    -- Поле hasInsurance забыли!
```

код скомпилируется, но внимательный компилятор предупредит вас о проблеме:

```
Fields of ‘Patient’ not initialised: hasInsurance
```

Пожалуйста, не пренебрегайте подобным предупреждением, ведь если вы проигнорируете его и затем попытаетесь обратиться к неинициализированному полю:

```haskell
main = print $ hasInsurance patient
  ...
```

ваша программа аварийно завершится на этапе выполнения с ожидаемой ошибкой:

```
Missing field in record construction hasInsurance
```

Не забывайте: компилятор — ваш добрый друг.

Заведите тип `UserQuery`, описывающий запрос пользователя в поисковике. У него должны быть поля: {.task_text}
- `query`: сам поисковый запрос.
- `clickCount`: количество кликов по результатам поисковой выдачи.
- `usedSuggest`: флаг, означающий, воспользовался ли пользователь подсказчиком в строке запроса.
- `viewedAdd`: флаг, означающий, была ли показана на странице реклама.
 {.task_text}

В блоке `where` функции `main` создайте экземпляр `q1` типа `UserQuery`. У него поле `query` равно строке "pancakes", `clickCount` равно 2, `usedSuggest` равно `False`, `viewedAd` равно `True`. {.task_text}

Затем создайте экземпляр `q2`, поля которого равны полям `q1` за исключением `clickCount`: значение этого поля должно быть на 1 больше. {.task_text}

В теле функции `main` выведите в консоль значение поля `clickCount` для `q1` и `q2`. {.task_text}

```haskell {.task_source #haskell_chapter_0210_task_0010}
module Main where

-- UserQuery type definition

main :: IO()
main = do
       -- print clickCount of q1
       -- print clickCount of q2
       print $ query q1
       where
       q2 = -- q1 with incremented clickCount value
       q1 = -- UserQuery with clickCount = 2
```
Синтаксис определения типа: `data TypeName = TypeName { label1 :: Type1, label2 :: Type2, ...}`. {.task_hint}
```haskell {.task_answer}
module Main where

data UserQuery = UserQuery { query :: String
                           , clickCount :: Int
                           , usedSuggest
                           , viewedAd :: Bool
                           }
main :: IO()
main = do
       print $ clickCount q1
       print $ clickCount q2
       print $ query q1
       where
       q2 = q1 { clickCount = clickCount q1 + 1 }
       q1 = UserQuery {
          query  = "pancakes"
         , clickCount = 2
         , usedSuggest = False
         , viewedAd = True
       }
```

Напишите, является ли тип `UserQuery` из предыдущей задачи Типом Сумма или Типом Произведение: {.task_text}
- `sum`, если это Тип Сумма.
- `product`, если это Тип Произведение.
 {.task_text}
 
```haskell
data UserQuery = UserQuery { query :: String
                           , clickCount :: Int
                           , usedSuggest
                           , viewedAd :: Bool
                           }
```

```consoleoutput {.task_source #haskell_chapter_0210_task_0020}
```
Мощность типа `UserQuery` равна произведению всех значений, принимаемых его полями. {.task_hint}
```haskell {.task_answer}
product
```

## Без меток

Помните, что метки полей — это синтаксический сахар, без которого мы вполне можем обойтись. Даже если тип был определён с метками, как наш `Patient`, мы можем работать с ним по-старинке:

```haskell
data Patient = Patient { firstName    :: String
                       , lastName     :: String
                       , email        :: String
                       , age          :: Int
                       , diseaseId    :: Int
                       , isIndoor     :: Bool
                       , hasInsurance :: Bool
                       }

main :: IO ()
main = print $ hasInsurance patient
  where
    -- Создаём по-старинке...
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      True
                      True
```

Соответственно, извлекать значения полей тоже можно по-старинке, через паттерн матчинг.


На строке 15 замените чтение поля через паттерн матчинг на работу с меткой. {.task_text}

```haskell {.task_source #haskell_chapter_0210_task_0030}
module Main where

data Patient = Patient { firstName
                       , lastName
                       , email        :: String
                       , age
                       , diseaseId    :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }

main :: IO ()
main = print insurance
  where
    Patient _ _ _ _ _ _ insurance = patient -- Needs refactoring!
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      False
                      True
```
Синтаксис обращения по метке: `b = label a`. {.task_hint}
```haskell {.task_answer}
module Main where

data Patient = Patient { firstName
                       , lastName
                       , email        :: String
                       , age
                       , diseaseId    :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }

main :: IO ()
main = print insurance
  where
    insurance = hasInsurance patient
    patient = Patient "John"
                      "Doe"
                      "john.doe@gmail.com"
                      24
                      431
                      False
                      True
```

С другими видами синтаксического сахара мы встретимся ещё не раз, на куда более продвинутых примерах.

