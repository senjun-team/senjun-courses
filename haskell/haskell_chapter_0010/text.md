# Глава 1. Введение

Перед вами — курс о Haskell, удивительном и прекрасном языке программирования.

## Первые вопросы

Мне задавали их множество раз. Отвечаю.

### Что такое этот ваш Haskell?

Haskell — чисто функциональный язык программирования общего назначения, может быть использован для решения самого широкого круга задач. Компилируемый, но может вести себя и как скриптовый. Кроссплатформенный. Ленивый, со строгой статической типизацией. И он не похож на другие языки. Совсем. {#block-about}

### Это что, какой-то новый язык?

Вовсе нет. История Haskell началась ещё в 1987 году. Этот язык был рождён в математических кругах, когда группа людей решила создать лучший функциональный язык программирования. В 1990 году вышла первая версия языка, названного в честь известного американского математика [Хаскелла Карри](https://en.wikipedia.org/wiki/Haskell_Curry). В 1998 году язык был стандартизован, а начиная с 2000-х началось его медленное вхождение в мир практического программирования. За эти годы язык совершенствовался, и вот в 2010 мир увидел его обновлённый стандарт. Так что мы имеем дело с языком, который старше Java.

### И кто его сделал?

Haskell создавался многими людьми. Наиболее известная реализация языка нашла своё воплощение в компиляторе GHC (The Glasgow Haskell Compiler), родившегося в 1989 году в Университете Глазго. У компилятора было несколько главных разработчиков, из которых наиболее известны двое, [Simon Peyton Jones](http://research.microsoft.com/en-us/people/simonpj/) и [Simon Marlow](http://community.haskell.org/~simonmar/). Впоследствии весомый вклад в разработку GHC внесли ещё несколько сотен человек. Исходный код компилятора GHC [открыт](https://ghc.haskell.org/trac/ghc). Кстати, сам компилятор на 82% написан на Haskell.

### А библиотеки для Haskell имеются?

О да! Их даже не сотни — их тысячи. В процессе чтения вы познакомитесь со многими из них.

### И что, его уже можно в production?

Он уже в production. С момента выхода первого стандарта язык улучшался, развивалась его экосистема, появлялись новые библиотеки, выходили в свет книги. Haskell полностью готов к серьёзному коммерческому использованию, о чём свидетельствуют истории успешного внедрения Haskell в бизнесе, в том числе [крупном](https://dshevchenko.biz/hs-research/Haskell-in-the-Large.pdf).

### А порог вхождения в Haskell высокий?

И да и нет. Освоение Haskell сложно в первую очередь из-за его непохожести на остальные языки, поэтому людям, имеющим опыт работы с другими языками, мозги поломать придётся. Именно поломать, а не просто пошевелить ими: Haskell заставляет иначе взглянуть даже на привычные вещи. С другой стороны, Haskell проще многих известных языков. Не верьте мне на слово, вскоре вы и сами в этом убедитесь. И знайте: многие люди, узнав вкус Haskell, категорически не желают возвращаться к другим языкам. Я вас предупредил.

### А я слышал ещё про какие-то монады

Да, есть такое дело. Некоторые вещи из мира Haskell не имеют прямых аналогов в других языках программирования, и это вводит новичков в ступор. Но не беспокойтесь: я сам прошёл через этот ступор и хорошо вас понимаю. Помните: новое лишь кажется страшным.

### А если сравнить его с C++/Python/Scala

Сравнение Haskell с другими языками выходит за рамки этого курса. Несколько раз вы встретите здесь кусочки кода на других языках, но я привожу их исключительно для того, чтобы подчеркнуть различие с Haskell, а вовсе не для сравнения в контексте «лучше/хуже». И вообще, я буду изо всех сил стараться не восхвалять Haskell без меры, я хочу лишь рассказать вам правду о нём. Мой вывод об этом языке я уже сделал, а свой вывод о нём вы должны сделать сами.

## Почему этот курс появился

Потому что меня откровенно достало. Почти все известные мне книги о Haskell начинаются с примера реализации быстрой сортировки и — куда ж без неё! — последовательности Фибоначчи. Этот курс не такой: минимум академизма, максимум практичности.

### Цель

Функциональное программирование — своеобразное гетто посреди мегаполиса нашей индустрии. Доля функциональных языков пока ещё очень мала, и многие разработчики побаиваются знакомства с этими языками, и с Haskell в особенности. Вероятно, вы слышали, что Haskell — это что-то архисложное, сугубо научное и непригодное для реальной жизни? Читайте дальше, и вскоре вы убедитесь в обратном.

### Что здесь есть

Что ждет вас в этом курсе?

Во-первых, я научу вас главному в Haskell. Основам, без освоения которых двигаться дальше никак не получится.

Во-вторых, я разрушу страх. Уже много лет вокруг Haskell витает дух страха, и я сполна ощутил его на себе. В действительности Haskell совсем не страшный, в нём нет чёрной магии, и чтобы программировать на нём, вам не нужна учёная степень. Более того, вы удивитесь, насколько просто в Haskell делать многие вещи, но эта простота откроется вам лишь после того, как вы близко познакомитесь с Тремя Китами Haskell, а также с госпожой Черепахой, поддерживающей оных. Имена этих Китов и Черепахи вы узнаете уже в третьей главе.

Этот курс не возведёт вас на вершины Haskell, но она откроет вам путь к этим вершинам.

### Чего здесь нет

Трёх вещей вы не найдёте в этом курсе:

1. Исчерпывающего справочника по Haskell. Дублировать [официальное описание стандарта Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/) я не стану.
2. Набора готовых рецептов. За рецептами пожалуйте на [Stack Overflow](http://stackoverflow.com/questions/tagged/haskell).
3. Введения в математическую теорию. Несмотря на то, что Haskell корнями своими уходит в математику, в этом курсе нет погружения в теорию категорий и в иные теории. Извините, если разочаровал.

### Обещание

Возможно, вы по уши влюбитесь в Haskell. Возможно, он вызовет у вас отвращение. Обещаю одно — скучно не будет. Начнём.