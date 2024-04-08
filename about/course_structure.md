# Организация проекта
Проект разбит на поддиректории, названия которых должны совпадать с id курса. Например, курс по питону располагается в директории `python`.

## Форматы данных
Тексты глав соответствуют формату [GitHub markdown.](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax)

Для маркировки текста задач и исходников задач используется разметка [markdown-it-attrs.](https://github.com/arve0/markdown-it-attrs)

## Структура курса
В директории курса лежат:
- `description.md` - краткое описание курса.
- `icon.svg` - иконка курса.
- поддиректории с главами. Имя директории с главой должно совпадать с id главы. Например, `python_chapter_0010`.

В директории главы располагаются:
- `text.md` - файл с текстом главы. Текст главы содержит задачи.
- `tasks` - директория с тестами для задач главы.

Пример:
```
golang_chapter_0030/
├── tasks
│   ├── golang_chapter_0030_task_0010
│   │   ├── wrapper_run
│   │   └── wrapper_test
│   ├── golang_chapter_0030_task_0020
│   │   ├── wrapper_run
│   │   └── wrapper_test
│   └── golang_chapter_0030_task_0030
│       ├── wrapper_run
│       └── wrapper_test
└── text.md
```


