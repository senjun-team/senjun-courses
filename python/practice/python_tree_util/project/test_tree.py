import unittest
import subprocess
from pathlib import Path


def run_tree_util(args):
    """
    Запускает скрипт tree.py и возвращает его консольный вывод
    в виде строки
    """

    cmd_line_args = ["python3", "tree.py", *args]
    res = subprocess.run(cmd_line_args, stdout=subprocess.PIPE)
    return res.stdout.decode("utf-8")


class TestTree(unittest.TestCase):
    def setUp(self):
        # Создаем пустую директорию
        self.empty_dir = Path("test_data/movies/historical")
        self.empty_dir.mkdir(parents=True, exist_ok=False)

        # Каждый тест-кейс состоит из названия, ожидаемого консольного вывода
        # и аргументов для запуска скрипта tree.py.
        self.cases = [
            ("dir without args", FULL_DIR, ["test_data"]),
            ("dir with -d", FULL_DIR_DIR_ONLY, ["./test_data/", "-d"]),
            ("dir with -L 1", DIR_LEVEL_1, ["test_data/", "-L 1"]),
            (
                "dir with -L 2 -d",
                DIR_LEVEL_2_DIR_ONLY,
                ["test_data", "-L 2", "-d"],
            ),
            (
                "dir with -L 3 -d",
                DIR_LEVEL_3_DIR_ONLY,
                ["test_data/", "-L 3", "-d"],
            ),
            ("subdir: books", DIR_BOOKS, ["test_data/books"]),
            (
                "subdir: fantasy, -L 1",
                DIR_FANTASY_LEVEL_1,
                ["./test_data/movies/fantasy/", "-L 1"],
            ),
            ("empty dir", DIR_EMPTY, [str(self.empty_dir)]),
        ]

    def tearDown(self):
        self.empty_dir.rmdir()

    def test_tree_util(self):
        for test_name, plan_output, args in self.cases:
            with self.subTest(test_name):
                fact_output = run_tree_util(args)
                self.assertEqual(fact_output, plan_output)


FULL_DIR = """test_data
├── books
│   ├── Dukaj_Other_songs.pdf
│   ├── Jonathan Strange & Mr Norrell.pdf
│   └── Piranesi.pdf
└── movies
    ├── fantasy
    │   ├── Edward Scissorhands.avi
    │   └── LOTR
    │       ├── The Fellowship of the Ring.avi
    │       ├── The Return of the King.avi
    │       └── The Two Towers.avi
    ├── historical
    └── sci-fy
        ├── 12 Monkeys.avi
        └── Blade runner.avi

7 directories, 9 files
"""

FULL_DIR_DIR_ONLY = """./test_data/
├── books
└── movies
    ├── fantasy
    │   └── LOTR
    ├── historical
    └── sci-fy

7 directories
"""

DIR_LEVEL_1 = """test_data/
├── books
└── movies

3 directories, 0 files
"""

DIR_LEVEL_2_DIR_ONLY = """test_data
├── books
└── movies
    ├── fantasy
    ├── historical
    └── sci-fy

6 directories
"""

DIR_LEVEL_3_DIR_ONLY = """test_data/
├── books
└── movies
    ├── fantasy
    │   └── LOTR
    ├── historical
    └── sci-fy

7 directories
"""

DIR_BOOKS = """test_data/books
├── Dukaj_Other_songs.pdf
├── Jonathan Strange & Mr Norrell.pdf
└── Piranesi.pdf

1 directory, 3 files
"""

DIR_FANTASY_LEVEL_1 = """./test_data/movies/fantasy/
├── Edward Scissorhands.avi
└── LOTR

2 directories, 1 file
"""

DIR_EMPTY = """test_data/movies/historical

1 directory, 0 files
"""

if __name__ == "__main__":
    unittest.main()
