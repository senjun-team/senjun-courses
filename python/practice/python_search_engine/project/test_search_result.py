import dataclasses
import unittest

from search_engine import SearchResult


class TestSearchResult(unittest.TestCase):
    def test_is_dataclass(self):
        self.assertTrue(dataclasses.is_dataclass(SearchResult))

    def test_empty(self):
        sr = SearchResult("", 0.0)
        self.assertEqual(sr.name, "")
        self.assertAlmostEqual(sr.score, 0.0)

    def test_not_empty(self):
        sr = SearchResult("collection/citations.txt", 0.345)
        self.assertEqual(sr.name, "collection/citations.txt")
        self.assertAlmostEqual(sr.score, 0.345)


if __name__ == "__main__":
    unittest.main()
