package lexemes

type LexemeType int
type Lexeme string

type Token struct {
	Lex Lexeme
	T   LexemeType
}

const (
	// type of the token
	NoToken = iota
	TokenStart
	TokenNumber
	TokenOperator
	TokenParanthesis
	// class of the symbol
	Digit
	Point
	Operator
	Paranthesis
	Other
	// states of the state machine
	NewToken
	NumberIntegerPart
	NumberFractionalPart
	Error
)
