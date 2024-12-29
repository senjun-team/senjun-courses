
# Вступление
В данной главе мы познакомимся с основами C++. В конце вы будете знать как в зависимости от условий менять поведение программы, итерироваться по массивам с помощью циклов, создавать функции, классы и структуры. 
Так же мы затронем шаблоны C++, макросы и выполнение кода на этапе компиляции. 
# условия
Для управления потоком исполнения кода есть 3 варианта: инструкция `if`, тернарный оператор `?` и `switch`. В этой главе мы затронем только первые два. 
## if ... else
Рассмотрим инструкцию `if`. В C++, для `if` (и некоторых других конструкций) фигурные скобки не обязательны, но в отличие от Python например, отступ ни на что не влияет и условие будет влиять на выполнение только одной следующей строки:

```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter < 4000) std::println("Write more plz");
std::println("You will see that anyway");
```
При выполнении примера выше мы увидим только второе сообщение. 
При необходимости произвести несколько действий, придется добавить фигурные скобки:
```cpp {.example_for_playground ci-wrap=function}
const int charsInThisChapter = 4000;
if (charsInThisChapter != 4000) {
	std::println("Writer we have a problem");
	std::println("I repeat Writer we have a problem");
}
else 
	std::println("Ok then");
```
Когда же понадобится проверить несколько различных условий, то можно сразу за `else` написать `if`:
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
`else if` не является специальной конструкцией. Расставим скобки до конца:
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
Как видите, второй `if` вложен внутрь `else` первого. Когда мы убираем скобки, то выглядит это более "плоским", что принято считать более читаемым. 

Чтобы размять руки, предлагаем посчитать ворон. Представим, что вороны могут образовывать "табуны" (табуны это про коней, но допустим) по 2, 5, 10, 89 и 1000 штук. Напишите код, который напечатает для каждого из вариантов: {.task_text } 
- 2 - "пара ворон"
- 5 - "мало ворон"
- 10 - "группа ворон"
- 89- "орда ворон"
- 1000 - "[легион](https://ru.wikibooks.org/wiki/Heroes_of_Might_and_Magic_III/%D0%98%D0%B3%D1%80%D0%BE%D0%B2%D0%BE%D0%B9_%D0%BC%D0%B8%D1%80#%D0%A0%D0%B0%D0%B7%D0%B2%D0%B5%D0%B4%D0%BA%D0%B0_%D0%B8_%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%BD%D0%BE%D1%81%D1%82%D1%8C_%D0%B2%D0%BE%D0%B9%D1%81%D0%BA) ворон"
- в других случаях - "неизвестно".
```cpp {.task_source ci-wrap=function}
const int crowsCount = 1000;
if (crowsCount == 2) {
	return "мало ворон";
} 
else if (/* ваш код */) {
	return "??? ворон";
}
/* и еще пару условий */
else {
	return "неизвестно ворон";
}
```

{.task_hint} Нужно заполнить условие внутри `else if` и дописать еще 2 `else if` (не забывайте про фигурные скобки!).

```cpp {.task_answer ci-wrap=function} 
const int crowsCount = 1000;
if (crowsCount == 2) {
	return "мало ворон";
} 
else if (crowsCount == 5) {
	return "мало ворон";
}
else if (crowsCount == 10) {
	return "мало ворон";
}
else if (crowsCount == 89) {
	return "мало ворон");
}
else {
	return "неизвестно ворон";
}
```
## тернарный оператор
Тернарный оператор - вместо тысячи слов. (с)
Тернарный оператор, в отличие от `if`, может применяться справа от знака равно. 
Например:
```cpp {.example_for_playground ci-wrap=function}
std::string message { 33 > 42 ? "33 is greater than 42" : "no, 33 is less then 42" };
```
Здесь мы значение `message`  устанавливаем в зависимости от результата сравнения. 
Если записать эту же логику с `if` получится более многословно:
```cpp {.example_for_playground ci-wrap=function}
std::string message;
if (33 > 42) {
	message = "33 is greater than 42";
} else {
	message = "no, 33 is less then 42";
}
```

Их можно вкладывать друг в друга, но читаемость от этого сильно страдает:
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = 1000;
std::string crowsCountAsText = crowsCount == 89 ? "орда": crowsCount == 1000 ? "легион" : "неизвестно";
std::println("{} ворон", crowsCountAsText);
```
Вариант через "отрицание" читается еще хуже
```cpp {.example_for_playground ci-wrap=function}
const int crowsCount = 1000;
std::string crowsCountAsText = crowsCount == 89 ? "орда" : crowsCount == 1000 ? "легион" : "неизвестно";
std::println("{} ворон", crowsCountAsText);
```
# Циклы

## for
Один из вариантов итерации - `for`. Его структура: `for (INIT; COND; STEP_ACTION)`.
```cpp {.example_for_playground ci-wrap=function}
int chaptersCount = 7;
int currentChapter = 4;
std::println("TODO:");
for (int i = 0; i < chaptersCount; ++i) {
	std::println("- [x] read chapter {}", i);
}
```
Данный пример вы уже могли неоднократно видеть, однако хочется напомнить, что можно итерироваться не только от 0 до некоторого N.
Вы будете часто встречать такую конструкцию
```cpp {.example_for_playground ci-wrap=function}
for (auto it = array.begin(); it != array.end(); it++) {
	// ...
}
```
Забегая вперед - это итерирование по некоторому массиву, с помощью концепции "итератор". 
## range based for

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
