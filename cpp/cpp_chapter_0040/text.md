# Вступление
В данной главе мы познакомимся с основами C++. В конце вы будуте знать как в зависимости от условий менять поведение программы, итерироваться по массивам с помощью циклов, создавать функции, классы и структуры. 
Так же мы затронем шаблоны C++, макросы и выполнение кода на этапе компиляции. 
# условия
Для управления потоком исполнения кода есть 3 варианта: `if`, тернарный оператор `?` и `switch`. 
## if ... else
Рассмотрим выражение `if`. В C++, для `if` фигурные скобки не обязательны, но в отличии от Python например, отсуп ни на что не влияет и условие будет влиять на выполнение только одной следующей строки:

```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter != 4000) std::println("That doesn't expected");
std::println("You will see that anyway");
```
При выполнении примера выше мы увидим только второе сообщение. У Хьюстона нет проблем!
Конечно мы можем добавить действие в случае "если" и в случае "иначе". 
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter != 4000) 
	std::println("Huston, we have a problem");
else 
	std::println("Ok then");
```
При необходимости произвести несколько действий, если `charsInThisChapter` не равно 4000, придется добавить фигурные скобки:
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter != 4000) {
	std::println("Huston, we have a problem");
	std::println("I repeat, Huston, we have a problem");
}
else 
	std::println("Ok then");
```
Когда же понадобится проверить несколько различных условий, то можно "соединять" несколько if, как бы образуя "конструкцию `else if`":
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter < 100) {
    std::println("Is that all you can write?");
}
else if (charsInThisChapter == 4000) {
    std::println("well, that was expected.");
} 
else if (charsInThisChapter > 4000) {
    std::println("Too Long...Don't Read.");
} 
else {
    std::println("there should be some throw, but we don't know how to do it for that moment");
}
```
Почему "конструкцию `else if`" в кавычках ? Потому что в C++ не выделяется `else if` в какое-то отдельное выражение или составное ключевое слово. Давайте расставим скобки до конца, чтобы было понятнее:
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter < 100) {
    std::println("Is that all you can write?");
}
else {
	if (charsInThisChapter == 4000) {
	    std::println("well, that was expected.");
	} 
	else {
		if (charsInThisChapter > 4000) {
		    std::println("Too Long...Don't Read.");
		} 
		else {
		    std::println("there should be some throw, but we don't know how to do it for that moment");
		}
	}
}
```
Как видите, второй `if` как бы вложен внутрь `else` первого. Когда мы убираем скобки, то выглядит это более "плоским", но по факту оно именно так и воспринимается компилятором. 

{.task_text } Чтобы размять руки, предлагаем посчитать ворон. Представим, что вороны могут образовывать "табуны" (табуны это про коней, но допустим) по 2, 5, 10, 89 и 1000 штук. Напишите код, который напечатает для каждого из вариантов:
- 2 - "пара ворон"
- 5 - "мало ворон"
- 10 - "группа ворон"
- 89- "орда ворон"
- 1000 - "[легион](https://ru.wikibooks.org/wiki/Heroes_of_Might_and_Magic_III/%D0%98%D0%B3%D1%80%D0%BE%D0%B2%D0%BE%D0%B9_%D0%BC%D0%B8%D1%80#%D0%A0%D0%B0%D0%B7%D0%B2%D0%B5%D0%B4%D0%BA%D0%B0_%D0%B8_%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D1%81%D1%82%D1%8C_%D0%B2%D0%BE%D0%B9%D1%81%D0%BA) ворон"
- в других случаях - "неизвестно"
```cpp {.task_source ci-wrap=function}
const int crowsCount = 1000;
if (crowsCount == 2) {
	std::println("мало ворон");
} 
else if (/* ваш код */) {
	std::println("??? ворон");
}
/* кажется надо добавить пару условий */
else {
	std::println("неизвестно ворон");
}
```
## тернарный оператор
Тернарный оператор - когда хочется все в 1 строке. 
Его структура выглядит так: `CONDITION ? TRUE_EXPRESSION : FALSE_EXPRESSION`. Тернарный оператор, это выражение, которое вычисляется в некоторое значение, потому слева от его `CONDITION` можно поставить присваивание чему-то. Например:
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
string message = charsInThisChapter == 42 ? "University, it that you?": "well, that was expected.";
std::println("{}", message);
```
Здесь мы значение `message`  устанавливаем в зависимости от 
Их можно вкладывать друг в друга, но читаемость от этого сильно страдает, потому что в итоговое выражение располагается либо между `?` и `:` (`TRUE_EXPRESSION`) либо справа от `:` (`FALSE_EXPRESSION`). 
Вот пример
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = 1000;
string message = crowsCount == 89 ? "орда": crowsCount == 1000 ? "легион" : "неизвестно";
std::println("{}", message);
```
Вариант через "отрицание" читается еще хуже
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = 1000;
string message = crowsCount != 89 ? crowsCount != 1000 ? "неизвестно" : "легион" : "орда";
std::println("{}", message);
```
Для читаемости можно расставить круглые скобки:
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = 1000;
string message = crowsCount != 89 ? (crowsCount != 1000 ? "неизвестно" : "легион") : "орда";
std::println("{}", message);
```
## switch case default
`switch` хорошо работает, когда нам нужно сравнить значение, с более чем 2-3 вариантами и эти варианты хорошо известны. 
Пример выше, с определением размера вороньего табуна, можно переписать в новом виде:
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = ;
switch(crowsCount) {
case 2:
	std::println("пара ворон");
case 5:
	std::println("мало ворон");
case 10:
	std::println("группа ворон");
case 89:
	std::println("орда ворон");
case 1000:
	std::println("легион ворон");
default:
	std::println("неизвестно ворон");
}
```
# Циклы

## for
`for` как и в других СИ-подобных
```cpp {.example_for_playground ci-wrap=function}
int chaptersCount = 7;
int currentChapter = 4;
std::println("TODO:");
for (int i = 0; i < chaptersCount; ++i) {
	std::println("- [x] read chapter {}", i);
}
```
## range based for
## while do
## do while

# Функции
## Синтаксис. Функции


# Структуры и классы

## Объединение данных
## Разница между классами и структурами
   В каких случаях стоит использовать структуры, а в каких - классы.
## Инкапсуляция: модификаторы видимости

## Конструктор, деструктор,  введение в RAII   

# Перечисления
## enum
TODO: switch
## enum class
TODO: switch
## различия между enum и enum class

# Пространства имён
## Синтаксис
## Вложенный вариант

# Обработка исключений
## try..catch

# Шаблоны

## 
## Определение функций с шаблонным параметром

# Макросы

# Вычисление на этапе компиляции
## consexpr
## consteval
## constinit

# Полезные ссылки

[C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#c2-use-class-if-the-class-has-an-invariant-use-struct-if-the-data-members-can-vary-independently)
- когда использовать классы, а когда - структуры.
