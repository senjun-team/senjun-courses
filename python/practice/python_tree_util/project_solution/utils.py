from enum import StrEnum
from pathlib import Path


class BoxDrawing(StrEnum):
    """
    Пробел и символы псевдографики для изображения в консоли
    древовидной иерархии
    https://en.wikipedia.org/wiki/Box-drawing_characters
    """

    SPACE = " "
    VERTICAL = "│"
    UP_AND_RIGHT = "└──"
    VERTICAL_AND_RIGHT = "├──"


def show_tree(dir, dirs_only, level):
    """
    Обходит директорию dir для вывода в консоль ее древовидной
    структуры, а также количества найденных директорий и файлов.

    Флаг dirs_only означает, что обрабатывать файлы не надо.
    level - это целое число либо None. Указывает, до какого уровня
    вложенности отображать иерархию.
    """

    print(dir)

    levels = -1 if level is None else level
    count_dirs, count_files = traverse(dir, levels, "", dirs_only)

    # Мы всегда учитываем корневую директорию, из которой запускается обход
    # иерархии директорий:
    count_dirs += 1

    print(f"\n{dir_str(count_dirs)}{file_str(count_files, dirs_only)}")


def dir_str(count_dirs):
    d = "directory" if count_dirs == 1 else "directories"
    return f"{count_dirs} {d}"


def file_str(count_files, dirs_only):
    if dirs_only:
        return ""

    f = "file" if count_files == 1 else "files"
    return f", {count_files} {f}"


def traverse(cur_dir, levels, prefix, dirs_only):
    """
    Рекурсивно обходит директорию cur_dir.
    Целое число levels нужно, чтобы завершить обход при заданном уровне вложенности.
    Строка prefix необходима, чтобы перед именем директории или файла вывести
    символы псевдографики для изображения древовидной иерархии.
    Флаг dirs_only означает, что нужно обрабатывать только директории (не файлы).

    Функция возвращает количество выведенных в консоль директорий и файлов.
    """

    count_dirs = 0
    count_files = 0

    if levels == 0:
        return count_dirs, count_files

    contents = sorted(Path(cur_dir).iterdir())

    for i, path in enumerate(contents):
        is_last = i == len(contents) - 1

        if path.is_dir() or not dirs_only and path.is_file():
            print(f"{prefix}{symbol_trailing(is_last)} {path.name}")

        if path.is_dir():
            count_dirs_child, count_files_child = traverse(
                path,
                levels - 1,
                f"{prefix}{symbols_indent(is_last)}",
                dirs_only,
            )
            count_dirs += count_dirs_child + 1
            count_files += count_files_child
        else:
            count_files += 1

    return count_dirs, count_files


def symbol_trailing(is_last):
    """
    Возвращает символ псевдографики в зависимости от того, является ли
    данный файл/директория последним внутри родительской директории.
    """

    return (
        BoxDrawing.UP_AND_RIGHT if is_last else BoxDrawing.VERTICAL_AND_RIGHT
    )


def symbols_indent(is_last):
    """
    Возвращает 4 символа: 4 пробела либо символ псевдографики и 3 пробела,
    если директория не последняя в списке. Эти символы используются для
    заполнения пространства слева от выводимого имени файла/директории и
    завершающего символа symbol_trailing().
    """

    return (
        BoxDrawing.SPACE * 4
        if is_last
        else f"{BoxDrawing.VERTICAL}{BoxDrawing.SPACE * 3}"
    )
