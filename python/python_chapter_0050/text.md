# Глава 5. Циклы

В питоне есть два вида циклов: `for` и `while`. О них и поговорим.


## Цикл while
Цикл `while` — это всем известный классический `while` из JavaScript, C++, Java:

```python
x = 100

while x > 0:
    handle(x)
    x -= 1
```

Вариация вечного цикла:

```python
while True:
    rotate_spinner()
```

Этот цикл тоже является вечным, потому что 1 интерпретируется как «истина»:

```python
while 1:
    run()
```

## Цикл for
Цикл `for` последовательно перебирает элементы коллекции:

```python
for resp in responses:
    parse(resp)
```

Пример прохода в цикле `for` по последовательности чисел от 0 до 9, которую генерирует функция `range()`:

```python
for x in range(10):
    print(x * x)
```

Имплементируйте тело функции. Пусть она возвращает количество слов, из которых состоит строка `s` в snake_case. snake_case — стиль написания составных слов, при котором слова разделяются символом подчеркивания `_`. {.task_text}

Например, если на вход функции прилетает строка `s`, равная `"not_supported_format"`, функция вернет 3. А для `"naive_solution"` вернет 2. {.task_text}

```python  {.task_source #python_chapter_0050_task_0010}
def get_words_count(s):
    # Your code here
```
Не забудьте обработать случай, когда строка `s` пустая. Количество слов, из которых состоит слово в snake_case, равно количеству символов `_` + 1. {.task_hint}
```python {.task_answer}
def get_words_count(s):
    if s == "":
        return 0
        
    count = 0
    
    for c in s:
        if c == "_":
            count += 1

    return count + 1
```

## Операторы continue и break
В питоне есть оператор `continue` для перехода на следующую итерацию цикла.

В этом цикле выведутся все согласные буквы:

```python
s = "Now is better than never."

for letter in s:
    if letter in "aeiouy":
        continue

    print(letter)
```

Также в питоне есть оператор `break` для выхода из цикла. Этот цикл завершится на первом встреченном пробеле:

```python
s = "Although never is often better than right now."

for letter in s:
    if letter == " ":
        break

    print(letter)
```

## Ключевое слово else в связке с циклами
Забавная особенность питона: ключевое слово `else` может использоваться не только в связке с `if`, но и после циклов `while` и `for`! В таком случае `else` сработает, только если выход из цикла произошел **без** участия `break`. 

```python
s = "If the implementation is hard to explain, it's a bad idea."

for letter in s:
    if letter == "y":
        break

    print(letter)
else:
    print("There is no letter 'y' in string")
```

В данном примере в консоль будет выведен текст из блока `else`. Потому что из цикла `for` не было выхода по `break`.

Имплементируйте функцию `print_letters()`. На вход она принимает строку. Функция анализирует каждую букву строки: {.task_text}
- Если в строке содержится заглавная буква, то печатает эту букву. Для определения, что буква заглавная, используйте метод строки `isupper()`. Например, при переборе букв строки, где `c` — буква, вызов метода выглядит так: `c.isupper()`.
- Если найден пробел, то все следующие за ним символы пропускаются.
- Если в строке не было ни одного пробела (в том числе если строка пустая), функция печатает `"no spaces"`.
 {.task_text}
 
```python {.task_source #python_chapter_0050_task_0020}
def print_letters(s):
    # Your code here
```
Для выхода из цикла используйте `break`, для перехода на новую итерацию — `continue`, а для обработки условия не выхода по `break` — ветку `else`. {.task_hint}
```python {.task_answer}
def print_letters(s):
    for с in s:
        if с.isupper():
            print(с)
        if с == ' ':
            break            
    else:
        print("no spaces")
```

## Резюмируем
- В питоне есть два вида циклов: `for` и `while`. 
- Циклы поддерживают операторы `break`, `continue` и `else`.
- Ветка `else` цикла срабатывает, если из цикла не было выхода по `break`.
