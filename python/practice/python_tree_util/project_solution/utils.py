import os
from pathlib import Path


def show_tree(dir, dir_only, level):
    """
    Обходит директорию dir для вывода в консоль ее древовидной
    структуры, а также количества найденных директорий и файлов.

    Флаг dir_only означает, что обрабатывать файлы не надо.
    level - это целое число либо None. Указывает, до какого уровня
    вложенности отображать иерархию.
    """

    print(dir)

    levels = -1 if level is None else level
    count_dirs, count_files = traverse(dir, levels, "", dir_only)

    print(f"\n{dir_str(count_dirs)}{file_str(count_files, dir_only)}")


def dir_str(count_dirs):
    count_dirs += 1
    d = "directory" if count_dirs == 1 else "directories"
    return f"{count_dirs} {d}"


def file_str(count_files, dir_only):
    if dir_only:
        return ""

    f = "file" if count_files == 1 else "files"
    return f", {count_files} {f}"


def traverse(cur_dir, levels, prefix, dir_only):
    """
    Рекурсивно обходит директорию cur_dir.
    Целое число levels нужно, чтобы завершить обход при заданном уровне вложенности.
    Строка prefix необходима, чтобы перед именем директории или файла вывести
    символы псевдографики для изображения древовидной иерархии.
    Флаг dir_only означает, что нужно обрабатывать только директории (не файлы).

    Функция возвращает количество выведенных в консоль директорий и файлов.
    """

    count_dirs = 0
    count_files = 0

    if levels == 0:
        return count_dirs, count_files

    contents = sorted(Path(cur_dir).iterdir())

    for i, path in enumerate(contents):
        is_last = i == len(contents) - 1
        is_dir = os.path.isdir(path)

        if is_dir or not dir_only and os.path.isfile(path):
            print(
                f"{prefix}{symbol_trailing(is_last)} {os.path.basename(path)}"
            )

        if is_dir:
            count_dirs_child, count_files_child = traverse(
                os.path.join(path),
                levels - 1,
                prefix + symbols_indent(is_last),
                dir_only,
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

    return "└──" if is_last else "├──"


def symbols_indent(is_last):
    """
    Возвращает 4 символа: 4 пробела либо символ псевдографики и 3 пробела,
    если директория не последняя в списке. Эти символы используются для
    заполнения пространства слева от выводимого имени файла/директории и
    завершающего символа symbol_trailing().
    """

    return " " * 4 if is_last else "│" + " " * 3
