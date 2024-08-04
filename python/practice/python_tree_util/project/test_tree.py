import unittest
import subprocess


class TestTree(unittest.TestCase):
    def setUp(self):
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
        ]

    def check_case(self, plan_output, args):
        cmd_line_args = ["python3", "tree.py", *args]

        res = subprocess.run(cmd_line_args, stdout=subprocess.PIPE)
        fact_output = res.stdout.decode("utf-8")

        self.assertEqual(fact_output, plan_output)

    def test_tree_util(self):
        for test_name, plan_output, args in self.cases:
            with self.subTest(test_name):
                self.check_case(plan_output, args)


if __name__ == "__main__":
    unittest.main()


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
