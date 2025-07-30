package lexemes

type LexemeType int
type Lexeme string

type Token struct {
	Lex Lexeme
	T   LexemeType
}

const (
	// 0 - no lexeme
	Delimiter LexemeType = iota + 1
	Operator
	NumberLexeme
	StartLexeme
)
