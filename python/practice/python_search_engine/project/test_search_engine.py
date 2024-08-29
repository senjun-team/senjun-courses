import math
import unittest

from search_engine import SearchEngine


class TestSearchEngine(unittest.TestCase):
    def test_init_then_search(self):
        se = SearchEngine([])
        self.assertEqual(len(se.search("python decorators")), 0)

    def test_search_nonexistent_words(self):
        se = SearchEngine(["docs/a.txt"])

        self.assertEqual(len(se.search("python decorators")), 0)
        self.assertEqual(len(se.search("Lambda functions PEP")), 0)

    def test_search_existing_words(self):
        se = SearchEngine(["docs/b.txt", "docs/c.txt"])
        self.assertEqual(len(se.search("science")), 2)

    def test_search_single_word_present_in_two_docs(self):
        se = SearchEngine(["docs/a.txt", "docs/b.txt", "docs/c.txt"])

        search_results = se.search("computer")
        self.assertEqual(len(search_results), 2)

        best_match = search_results[0]
        self.assertEqual(best_match.name, "docs/c.txt")
        # Слово "computer" встречается в документе 1 раз. Всего в документе 4 слова
        tf = 1 / 4
        # Всего в коллекции 3 документа. Из них 2 содержат слово "computer"
        idf = math.log((3 + 1) / (2 + 1))
        self.assertAlmostEqual(best_match.score, tf * idf)

        second_match = search_results[1]
        self.assertEqual(second_match.name, "docs/b.txt")
        # Пересчитываем tf для документа b.txt. Слово "computer" встречается
        # в нем 1 раз. Всего в документе 25 слов
        tf = 1 / 25
        self.assertAlmostEqual(second_match.score, tf * idf)

    def test_search_single_word_present_in_one_doc(self):
        se = SearchEngine(["docs/a.txt", "docs/b.txt", "docs/c.txt"])

        search_results = se.search("Central")
        self.assertEqual(len(search_results), 1)

        match = search_results[0]
        self.assertEqual(match.name, "docs/a.txt")
        # Слово "central" встречается в документе 2 раза. Всего в документе 18 слов
        tf = 2 / 18
        # Всего в коллекции 3 документа. Из них 1 содержит слово "central"
        idf = math.log((3 + 1) / (1 + 1))
        self.assertAlmostEqual(match.score, tf * idf)


if __name__ == "__main__":
    unittest.main()
