"""
Этот файл должен содержать:

- Класс SearchEngine.
- Класс данных SearchResult.
- Функцию split_to_words().
- Любые вспомогательные классы и функции.
"""


def main():
    docs = ["docs/a.txt", "docs/b.txt"]
    se = SearchEngine(docs)

    print(se.search("Inverted index (DATA STRUCTURE)."))


if __name__ == "__main__":
    main()