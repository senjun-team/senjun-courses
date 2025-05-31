# Глава 25. Классы типов

Если типы и значения — привычные понятия, которые можно найти в том или ином виде в любом языке программирования, то термин _класс типов_ встречается нечасто. У него нет аналогов и в обычном языке, поэтому я сначала постараюсь объяснить его смысл на примере.

В типизированном языке у каждой функции есть тип, но бывают функции, которые могут быть определены на аргументах разных типов. По сути, они описывают схожие понятия, но определены для значений разных типов. Например, функция сравнения на равенство, говорящая о том, что два значения одного типа `a` равны, имеет тип `a -> a -> Bool`. А функция печати выражения имеет тип `a -> String`. Но что такое `a` в этих типах?

Тип `a` является любым типом, для которого сравнение на равенство или печать (преобразование в строку) имеют смысл. Это понятие как раз и кодируется в классах типов.

## Что такое классы типов

Классы типов (type classes) позволяют определять функции с одинаковым именем для разных типов.

У классов типов есть имена. Как и имена классов, они начинаются с большой буквы. Например, класс сравнений на равенство называется `Eq` (от англ. *equals* — равняется), а класс печати выражений имеет имя `Show` (от англ. *show* — показывать). Посмотрим на их определения:

Класс `Eq`:

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

Класс `Show`:

```haskell
class Show a where
    show :: a -> String
```

За ключевым словом `class` следует имя класса, тип-параметр и ещё одно ключевое слово `where`. Далее с отступами пишутся имена определённых в классе значений. Значения класса называются *методами*.

Мы определяем лишь _типы_ методов. Конкретная _реализация_ методов будет зависеть от типа `a`. Методы определяются в _экземплярах_ классов типов. Мы скоро к ним перейдём.

Программистская аналогия класса типов — это интерфейс. В интерфейсе определён набор значений (как констант, так и функций), которые могут быть применены к поддерживающим данный интерфейс типам. К примеру, в интерфейсе «сравнение на равенство» для некоторого типа `a` определены две функции: равно `(==)` и не равно `(/=)` с одинаковым типом `a -> a -> Bool`. А в интерфейсе «печати» для любого типа `a` определена одна функция `show` типа `a -> String`.

Математическая аналогия класса типов — это алгебраическая система. Алгебра изучает свойства объекта в терминах операций, определённых на нём, и взаимных ограничениях этих операций. Алгебраическая система представляет собой набор операций и свойств этих операций. Этот подход позволяет абстрагироваться от конкретного представления объектов.

Например, группа — это все объекты данного типа `a`, для которых определены значения:
- константа — нейтральный *элемент* типа `a`,
- бинарная операция типа `a -> a -> a`,
- операция взятия обратного элемента типа `a -> a`.

При этом на операции *могут* накладываться ограничения, называемые свойствами операций или законами (laws):
- ассоциативность бинарной операции: `a + b == b + a`.
- нейтральный элемент: при сложении любого элемента с нейтральным элементом на выходе мы получаем исходное число.

Такие правила следуют из логики и идеи, которые мы вкладываем в класс `Group`. 
Давайте определим класс для группы:

```haskell
class Group a where
    e   :: a
    (+) :: a -> a -> a
    inv :: a -> a
```

Класс с именем `Group` имеет для некоторого типа `a` три метода:
- константу `e :: a`,
- операцию `(+) :: a -> a -> a`,
- операцию взятия обратного элемента `inv :: a -> a`.

В нашем случае мы хотим сказать, что константа `e` это «нейтральный элемент», то есть такое специальное значение, для которого будет выполняться следующее:

```haskell
a + b == b + a -- ассоциативность
a + e == a -- сложение с нулевым элементов
inv a + a == e -- сохранение значения при применении inv
```

Мы можем представить, что под эти ограничения подходят целые числа:
- `e` — `0`,
- `(+)` — сложение,
- `inv` — минус.

```
0 + 10 == 10
inv 5 + 5 == 0
```

Как и в алгебре, в Haskell классы типов позволяют описывать сущности в терминах определённых на них операций или значений. В примерах мы указываем лишь наличие операций и их типы, так же и в классах типов. Класс типов содержит набор имён его значений с информацией о типах значений.

Представьте, что нам нужен расчет скидки на игру для аккаунта игрока. Однако, мы не хотим зависеть от конкретной реализации маркетплейса.  {.task_text}

Напишите класс `Account` с типом-параметром `a` и операцией `getPlayedTime :: a -> Int`. {.task_text}

```haskell {.task_source  #haskell_chapter_0250_task_0010}
module UserCode where
-- ваше определение класса Account
```

Этот код должен быть похож на код класса `Group`, но класс называется `Account` и содержит определение 1 операции: `getPlayedTime :: a -> Int`. {.task_hint}

```haskell {.task_answer}
module UserCode where

class Account a where
    getPlayedTime :: a -> Int
```

Определив класс `Group`, мы можем начать строить различные выражения, которые затем будут интерпретироваться специфическим для типа образом:

```haskell
twice :: Group a => a -> a
twice a = a + a

isE :: (Group a, Eq a) => a -> Bool
isE x = (x == e)
```

Обратите внимание на запись `Group a =>` и `(Group a, Eq a) =>`. Это называется контекстом объявления типа. В контексте мы говорим, что данный тип должен быть из класса `Group` или из классов `Group` и `Eq`. Это значит, что для этого типа мы можем пользоваться методами этих классов.

В первой функции `twice` мы воспользовались методом `(+)` класса `Group`, поэтому функция имеет контекст `Group a =>`. А во второй функции `isE` мы воспользовались методом `e` класса `Group` и методом `(==)` класса `Eq`, поэтому функция имеет контекст `(Group a, Eq a) =>`.

Ранее мы определили класс `Account`. Мы подготовили функцию расчета скидки `calcDiscount`, однако она работает только для аккаунтов `SteamAccount`, а мы бы хотели для любого маркетплейса. Функция принимает `account`, флаг «черная пятница» `blackFriday` и должна вернуть размер скидки. Скидка может быть от 0 до 15% в зависимости от наигранного времени. В случае черной пятницы скидка будет увеличена на 10%. {.task_text}

Перепишите текущую функцию с использованием класса `SteamAccount` на функцию, которая может работать с любым аккаунтом из класса `Account` (у функции есть контекст `Account a`). {.task_text}

```haskell {.task_source #haskell_chapter_0250_task_0020}
module UserCode where
clamp :: Int -> Int -> Int -> Int
clamp a val b =  min (max a val) b

-- вы можете удалить этот тип, если он мешает
data SteamAccount = SteamAccount {
        steamPlayedTime :: Int, accountId :: String
}

class Account a where
    getPlayedTime :: a -> Int

calcDiscount :: SteamAccount -> Bool -> Int
calcDiscount account isBlackFriday = clamp 0 discount 20
    where timeModifier = clamp 0 (steamPlayedTime account) 15
          discount = if isBlackFriday
              then timeModifier + 10
              else timeModifier
```

Требуется заменить `SteamAccount` на тип `a`, о котором мы ничего не знаем. Чтобы обозначить возможность применить функцию `playedTime` к типу `a`, добавьте контекст `Account a` в определении функции. {.task_hint}

```haskell {.task_answer}
module UserCode where
clamp :: Int -> Int -> Int -> Int
clamp a val b =  min (max a val) b

class Account a where
    getPlayedTime :: a -> Int

calcDiscount :: Account a => a -> Bool -> Int
calcDiscount account isBlackFriday = clamp 0 discount 20
    where timeModifier = clamp 0 (getPlayedTime account) 15
          discount = if isBlackFriday
              then timeModifier + 10
              else timeModifier
```

## Контекст классов типов. Суперклассы

Класс типов также может содержать контекст. Он указывается между ключевым словом `class` и именем класса. Например:

```haskell
class IsPerson a

class IsPerson a => HasName a where
    name :: a -> String
```

Это определение говорит о том, что мы можем сделать экземпляр класса `HasName` только для тех типов, которые содержатся в `IsPerson`. Мы говорим, что класс `HasName` содержится в `IsPerson`. В этом случае класс из контекста `IsPerson` называют *суперклассом* для данного класса `HasName`.

Это сказывается на контексте объявления типа:

```haskell
fun :: HasName a => a -> a
```

Такая запись означает, что мы можем пользоваться для значений типа `a` как методами из класса `HasName`, так и методами из класса `IsPerson`. Поскольку если тип принадлежит классу `HasName`, то он также принадлежит и `IsPerson`.

Запись `(IsPerson a => HasName a)` немного обманывает. Интуитивнее было бы писать `IsPerson a <= HasName a`: если тип `a` в классе `HasName`, то он точно в классе `IsPerson`. Но в Haskell закрепилась другая запись.

В предыдущей задаче мы сделали расчет скидки, не привязанный к маркетплейсу. Теперь мы хотим поощрять премиальных пользователей. Мы могли бы расширить класс `Account`, но библиотеку с этим классом уже используют наши партнеры и мы не хотим ее обновлять. {.task_text}

Поэтому нам нужен новый класс `AccountExtended`, для которого мы сможем узнать, является ли аккаунт премиальным с помощью операции `isVIP :: a -> Bool`. Для этого также понадобится изменить функцию расчета скидки — теперь мы к старому расчету добавим дополнительно 10% скидки для премиальных аккаунтов. {.task_text}

```haskell {.task_source  #haskell_chapter_0250_task_0030}
module UserCode where

class Account a where
    getPlayedTime :: a -> Int

clamp :: Int -> Int -> Int -> Int
clamp a val b =  min (max a val) b

-- вносите изменения начиная отсюда

calcDiscount :: Account a => a -> Bool -> Int
calcDiscount account isBlackFriday = clamp 0 discount 20
    where timeModifier = clamp 0 (getPlayedTime account) 15
          discount = if isBlackFriday
              then timeModifier + 10
              else timeModifier
```

Напишите класс `AccountExtended` с операцией `isVIP :: a -> Bool`. Измените функцию `calcDiscount` так, чтобы она могла использовать операции `isVIP` и `playedTime` над первым аргументом `a`. Для этого замените контекст `Account` на `AccountExteded`. В реализации `calcDiscount` после того, как скидка рассчитана (после применения `clamp 0 discount 20`), добавьте к ней еще 10, если `isVIP account == True`. {.task_hint}

```haskell {.task_answer}
module UserCode where

-- из библиотеки InternalLib мы получили класс
class Account a where
    getPlayedTime :: a -> Int

clamp :: Int -> Int -> Int -> Int
clamp a val b =  min (max a val) b

class Account a => AccountExtended a where
    isVIP :: a -> Bool

calcDiscount :: AccountExtended a => a -> Bool -> Int
calcDiscount account isBlackFriday =
    clamp 0 baseDiscount 20 + vipDiscount
        where timeModifier = clamp 0 (getPlayedTime account) 15
              baseDiscount = if isBlackFriday
                  then timeModifier + 10
                  else timeModifier
              vipDiscount = if isVIP account
                  then 10
                  else 0
```

## Экземпляры классов типов

В *экземплярах* (instance) классов типов мы даём конкретное наполнение для методов класса типов. Определение экземпляра пишется так же, как и определение класса типа, но вместо `class` мы пишем `instance`, вместо некоторого типа — наш конкретный тип, а вместо типов методов — уравнения для них.

Определим экземпляры для `Bool`.

Класс `Eq`:

```haskell
instance Eq Bool where
    (==) True  True  = True
    (==) False False = True
    (==) _     _     = False

    (/=) a b         = not (a == b)
```

Класс `Show`:

```haskell
instance Show Bool where
    show True  = "True"
    show False = "False"
```

Класс `Group`:

```haskell
instance Group Bool where
    e       = True
    (+) a b = and a b
    inv a   = not a
```

Для [конструкторов типов](/courses/haskell/chapters/haskell_chapter_0230/) мы тоже можем определять экземпляры классов, но в этом случае нам надо взять конструктор типа в круглые скобки. Тогда мы получим законченный тип:

```haskell
instance Group a => Group (Maybe a) where
	e = Nothing
	(+) Nothing _ = Nothing
	(+) _ Nothing = Nothing
	(+) (Just a) (Just b) = (+) a b
	inv (Just a) = Just (inv a)
	inv Nothing = Nothing
```

В определении экземпляра `Group` для конструктора типа `Maybe a` мы также используем суперкласс для `a`, потому что вместо `a` может быть что угодно, но мы не знаем как реализовать операции `(+)` и `inv` для чего угодно. Определив контекст `Group a`, мы можем применить `(+)` и `inv` к значению `a` «внутри» `Maybe`.

На самом деле приведённое выше определение экземпляра `Group Bool` имеет проблемы, хотя по типам оно подходит. Оно ошибочно как раз из-за нарушения ограничений, которые мы упоминали в самом начале определения класса `Group`. Для группы необходимо, чтобы для любого `a` выполнялось:

```haskell
inv a + a == e
```

У нас лишь два значения, и это свойство не выполняется ни для одного из них. Проверим:

```haskell
    inv True   + True
 => (not True) + True
 => False      + True
 => and False    True
 => False

    inv False   + False
 => (not False) + False
 => True        + False
 => and True      False
 => False
```
Из этого следует, что реализация экземпляра `Group Bool` может привести к неожиданному результату.

Мы можем написать инстанс `Group (Maybe Bool)`, и тогда в качестве `e` мы сможем указать `Nothing` и определить соответствующим образов операции, чтобы все наши ограничения выполнялись. Выше мы определили `Group (Maybe a)` с условием, что `a` реализует класс `Group`. В случае с `Bool` мы не можем так сделать, поэтому придется предоставить явную реализацию `Maybe Bool`:

```haskell
instance Group (Maybe Bool) where
	e = Nothing
	(+) Nothing _ = Nothing
	(+) _ Nothing = Nothing
	(+) (Just True) (Just True) = True
	(+) (Just False) (Just _) = False
	(+) (Just _) (Just False) = False
	inv (Just True) = Just False
	inv (Just False) = Just True
	inv Nothing = Nothing
```

Проверять свойства очень важно, потому что другие люди, читая ваш код и используя ваши функции, будут на них рассчитывать.

Представьте, что вам нужно написать web-сервис, в котором выбрали коммуникацию в формате JSON. Так как существующие реализации сериализации обладают [фатальным недостатком](https://ru.wikipedia.org/wiki/%D0%A1%D0%B8%D0%BD%D0%B4%D1%80%D0%BE%D0%BC_%D0%BD%D0%B5%D0%BF%D1%80%D0%B8%D1%8F%D1%82%D0%B8%D1%8F_%D1%87%D1%83%D0%B6%D0%BE%D0%B9_%D1%80%D0%B0%D0%B7%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B8), ваша команда решила написать свою. У вас уже есть класс `ToJSON`. Осталось написать его реализацию для заданных ниже типов данных (`Category`, `Point`, `SensitivePayload`) и базового `Int` и `Double`. {.task_text}

`SensitivePayload` должен стать объектом с ключами `category` и `poi`.  {.task_text}

`Category` — строка `secret` для конструктора `Secret` и `nonsecret` для `NonSecret`. Не забудьте добавить экранированные двойные кавычки (`"\"secret\""`).  {.task_text}

`Point` будет объектом с ключами `x` (первый аргумент) и `y`. {.task_text}

```haskell {.task_source  #haskell_chapter_0250_task_0040}
module UserCode where
class ToJSON a where
    toJSON :: a -> String

data Point a = Point a a
data Category = Secret | NonSecret
data SensitivePayload = SensitivePayload
        { dataCategory :: Category
        , poi :: Point Int
        }
```

Начните с класса для `Int`. В его реализации вам поможет функция `show`. Добавьте инстанс `Category`. Посмотрите на реализацию инстанса `Show Bool` выше - pattern matching может пригодиться. Результатом будет строка в строке. Вам снова понадобится экранирование кавычек. Затем сделайте инстанс для `Point` - сформируйте строку вида `{"x":12, "y":33}`. Не вставляйте символы пробела. Добавьте контекст классу `ToJSON a =>`, и вы сможете использовать внутри `toJson` для значений. Важно, что надо взять в круглые скобки тип `(Point a)`. Ключи в json указываются в кавычках, поэтому имя ключа будет окантовано экранированными кавычками. И, конечно, инстанс `SensitivePayload`. Используйте суперклассы снова и не забудьте экранирование ключей. {.task_hint}

```haskell {.task_answer}
module UserCode where
class ToJSON a where
    toJSON :: a -> String

data Point a = Point a a
data Category = Secret | NonSecret
-- pov - point of interest
data SensitivePayload = SensitivePayload {
    dataCategory :: Category,
    poi :: Point Int }

instance ToJSON Int where
    toJSON = show

instance ToJSON a => ToJSON (Point a) where
    toJSON (Point x y) =
        "{\"x\":" ++ toJSON x
        ++ ","
        ++ "\"y\":" ++ toJSON y
        ++ "}"

instance ToJSON Category where
    toJSON Secret = "\"secret\""
    toJSON NonSecret = "\"nonsecret\""

instance ToJSON SensitivePayload where
    toJSON (SensitivePayload cat point) =
        "{\"category\":" ++ toJSON cat
        ++ ",\"poi\":" ++ toJSON point
        ++ "}"
```
