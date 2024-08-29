import unittest

from search_engine import split_to_words


class TestSplitToWords(unittest.TestCase):
    def test_empty_cases(self):
        texts = [
            "",
            " ",
            "   ",
            ".",
            ",,",
            r"""!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~""",
            "!  ,  ... .  ?",
        ]

        for text in texts:
            with self.subTest(f"text: [{text}]"):
                self.assertEqual(len(split_to_words(text)), 0)

    def test_spaces_handling(self):
        text_to_words = [
            ("1", ["1"]),
            ("single", ["single"]),
            ("  SPACES ", ["spaces"]),
            (" u p d a  t e", ["u", "p", "d", "a", "t", "e"]),
            (
                " collections     Container DATATYPES ",
                ["collections", "container", "datatypes"],
            ),
        ]

        for text, words_plan in text_to_words:
            with self.subTest(f"text: {text}"):
                self.assertEqual(words_plan, split_to_words(text))

    def test_mixed_cases(self):
        text_to_words = [
            ('"decorator"', ["decorator"]),
            ("-Okapi-BM25-", ["okapi", "bm25"]),
            ("BM =  best match ", ["bm", "best", "match"]),
            (
                "Inverted index (DATA STRUCTURE).",
                ["inverted", "index", "data", "structure"],
            ),
            ("101...", ["101"]),
            ("tape drives drives", ["tape", "drives", "drives"]),
            ("record blocks per record", ["record", "blocks", "per", "record"]),
        ]

        for text, words_plan in text_to_words:
            with self.subTest(f"text: {text}"):
                self.assertEqual(words_plan, split_to_words(text))


if __name__ == "__main__":
    unittest.main()
