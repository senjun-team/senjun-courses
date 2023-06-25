# senjun-courses
Opensource курсы для портала [senjun.ru.](https://senjun.ru)

## Структура проекта
Проект разбит на поддиректории, названия которых должны совпадать с id курса. Например, `python`.

В директории курса должны лежать:
- `description.md` - краткое описание курса
- `icon.svg` - иконка курса
- поддиректории с главами. Имя директории с главой должно совпадать с id главы. Например, `python_chapter_0010`.

В директории главы располагается единственный файл:
- `text.md` - текст главы вместе с текстами задачек и сэмплами кода задачек.

## Форматы данных
Тексты глав соответствуют формату [GitHub markdown.](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)

Для маркировки текста задач и исходников задач используется разметка [markdown-it-attrs.](https://github.com/arve0/markdown-it-attrs)
