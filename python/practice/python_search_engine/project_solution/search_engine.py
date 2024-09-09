import math
import string
from collections import Counter, defaultdict
from dataclasses import dataclass


def split_to_words(text: str) -> list[str]:
    """
    Нормализация строки и разбиение ее на слова
    """

    translations = str.maketrans(
        string.punctuation, " " * len(string.punctuation)
    )

    return text.translate(translations).lower().split()


@dataclass
class SearchResult:
    """
    Элемент поисковой выдачи с именем документа и значением
    релевантности документа заданному запросу
    """

    name: str
    score: float


@dataclass
class DocWithWord:
    """
    Имя документа и частота, с которой в нем встречается
    конкретное слово
    """

    name: str
    word_freq: int


class SearchEngine:
    def __init__(self, docs: list[str]) -> None:
        """
        Инициализация и заполнение поискового индекса
        """

        # Обратный индекс. Ключ - слово, значение - список документов,
        # в которых это слово встречается
        self._index: dict[str, list[DocWithWord]] = defaultdict(list)

        # Ключ - имя документа, значение - количество слов в документе
        self._doc_wordcount: dict[str, int] = {}
        self._crawl(docs)

    def search(self, query: str) -> list[SearchResult]:
        """
        Выдача найденных по запросу документов, отсортированных
        по релевантности
        """

        # Ключ - имя документа, значение - релевантность документа конкретному
        # слову запроса
        scores: dict[str, float] = defaultdict(float)

        for word in split_to_words(query):
            if (docs_with_word := self._index.get(word)) is None:
                continue
            idf = self._idf(word)
            for doc_with_word in docs_with_word:
                tf = self._tf(doc_with_word)
                scores[doc_with_word.name] += tf * idf

        search_results = [
            SearchResult(name=k, score=v) for k, v in scores.items()
        ]

        search_results.sort(key=lambda sr: sr.score, reverse=True)
        return search_results

    def _crawl(self, docs: list[str]) -> None:
        """
        Построение обратного индекса по коллекции документов
        """

        for filepath in docs:
            with open(filepath) as f:
                self._add_to_index(filepath, f.read())

    def _add_to_index(self, doc: str, content: str) -> None:
        """
        Добавление документа в индекс
        """

        words = split_to_words(content)
        self._doc_wordcount[doc] = len(words)

        for word, freq in Counter(words).items():
            self._index[word].append(DocWithWord(doc, freq))

    def _idf(self, word) -> float:
        """
        inverse document frequency, обратная частота документа
        """
        return math.log(
            (len(self._doc_wordcount) + 1)
            / (len(self._index.get(word, [])) + 1)
        )

    def _tf(self, doc_with_word: DocWithWord) -> float:
        """
        term frequency, частота слова в документе
        """
        return (
            doc_with_word.word_freq / self._doc_wordcount[doc_with_word.name]
        )


def main():
    docs = ["docs/a.txt", "docs/b.txt"]
    se = SearchEngine(docs)

    print(se.search("Inverted index (DATA STRUCTURE)."))


if __name__ == "__main__":
    main()
