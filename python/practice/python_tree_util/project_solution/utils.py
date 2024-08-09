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

    levels = -1 if level is None else level
    count_dirs, count_files = traverse(dir, levels, "", dirs_only)

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

    print(f"{cur_dir if isinstance(cur_dir, str) else cur_dir.name}")

    count_dirs = 1  # количество директорий, включая cur_dir
    count_files = 0

    if levels == 0:
        return count_dirs, count_files

    contents = sorted(get_contents(cur_dir, dirs_only))

    for i, path in enumerate(contents):
        is_last = i == len(contents) - 1

        print(f"{prefix}{symbol_trailing(is_last)} ", end="")

        if path.is_file():
            print(path.name)
            count_files += 1
        elif path.is_dir():
            count_dirs_in_subtree, count_files_in_subtree = traverse(
                path,
                levels - 1,
                f"{prefix}{symbols_indent(is_last)}",
                dirs_only,
            )
            count_dirs += count_dirs_in_subtree
            count_files += count_files_in_subtree

    return count_dirs, count_files


def get_contents(dir, dirs_only):
    if dirs_only:
        return (p for p in Path(dir).iterdir() if p.is_dir())

    return Path(dir).iterdir()


def symbol_trailing(is_last):
    """
    Возвращает символ псевдографики в зависимости от того, является ли
    данный путь последним внутри родительской директории.
    """

    return (
        BoxDrawing.UP_AND_RIGHT if is_last else BoxDrawing.VERTICAL_AND_RIGHT
    )


def symbols_indent(is_last):
    """
    Возвращает 4 символа: 4 пробела либо символ псевдографики и 3 пробела,
    если путь не последний в списке. Эти символы используются для
    заполнения пространства слева от выводимого имени файла/директории и
    завершающего символа symbol_trailing().
    """

    return (
        BoxDrawing.SPACE * 4
        if is_last
        else f"{BoxDrawing.VERTICAL}{BoxDrawing.SPACE * 3}"
    )
