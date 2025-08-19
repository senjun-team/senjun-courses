package command

import (
	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
	"unicode"
)

func (c *Command) saveToken(token *lexemes.Token) {
	if len(token.Lex) == 0 {
		return
	}
	c.Tokens = append(c.Tokens, token)
}

func startAccumulatingNumber(c *Command,
	char rune,
	token *lexemes.Token) (newToken *lexemes.Token) {
	c.saveToken(token)
	newToken = &lexemes.Token{}
	newToken.T = lexemes.TokenNumber
	newToken.Lex = lexemes.Lexeme(char)
	return newToken
}

func accumulateNumber(c *Command,
	char rune,
	token *lexemes.Token) (newToken *lexemes.Token) {
	token.Lex += lexemes.Lexeme(char)
	return token
}

func accumulateOperator(c *Command,
	char rune,
	token *lexemes.Token) (newToken *lexemes.Token) {
	c.saveToken(token)
	newToken = &lexemes.Token{}
	newToken.T = lexemes.TokenOperator
	newToken.Lex = lexemes.Lexeme(char)
	return newToken
}

func accumulateParanthesis(c *Command,
	char rune,
	token *lexemes.Token) (newToken *lexemes.Token) {
	c.saveToken(token)
	newToken = &lexemes.Token{}
	newToken.T = lexemes.TokenParanthesis
	newToken.Lex = lexemes.Lexeme(char)
	return newToken
}

// finite-state machine for the lexical analysis of the expression
type fsmT map[int]map[int]struct {
	state  int
	action func(*Command, rune, *lexemes.Token) *lexemes.Token
}

func analyzeSymbol(c rune) int {
	switch {
	case unicode.IsDigit(c):
		return lexemes.Digit
	case c == '.':
		return lexemes.Point
	case c == '+' || c == '-' || c == '*' || c == '/':
		return lexemes.Operator
	case c == ')' || c == '(':
		return lexemes.Paranthesis
	}
	return lexemes.Other
}

func (c *Command) Tokenize() error {
	fsm := fsmT{
		lexemes.Digit: {
			// now state
			lexemes.NewToken: {
				// new state
				lexemes.NumberIntegerPart,
				// action
				startAccumulatingNumber},

			lexemes.NumberIntegerPart: {
				lexemes.NumberIntegerPart,
				accumulateNumber},

			lexemes.NumberFractionalPart: {
				lexemes.NumberFractionalPart,
				accumulateNumber},
		},
		lexemes.Point: {
			lexemes.NewToken: {
				lexemes.NumberFractionalPart,
				startAccumulatingNumber,
			},
			lexemes.NumberIntegerPart: {
				lexemes.NumberFractionalPart,
				accumulateNumber,
			},
			lexemes.NumberFractionalPart: {
				lexemes.Error,
				nil,
			},
		},
		lexemes.Operator: {
			lexemes.NewToken: {
				lexemes.NewToken,
				accumulateOperator,
			},
			lexemes.NumberIntegerPart: {
				lexemes.NewToken,
				accumulateOperator,
			},
			lexemes.NumberFractionalPart: {
				lexemes.NewToken,
				accumulateOperator,
			},
		},
		lexemes.Paranthesis: {
			lexemes.NewToken: {
				lexemes.NewToken,
				accumulateParanthesis,
			},
			lexemes.NumberIntegerPart: {
				lexemes.NewToken,
				accumulateParanthesis,
			},
			lexemes.NumberFractionalPart: {
				lexemes.NewToken,
				accumulateParanthesis,
			},
		},
		lexemes.Other: {
			lexemes.NewToken: {
				lexemes.Error,
				nil,
			},
			lexemes.NumberIntegerPart: {
				lexemes.Error,
				nil,
			},
			lexemes.NumberFractionalPart: {
				lexemes.Error,
				nil,
			},
		},
	}

	if len(c.Input) == 0 {
		return cerrors.ErrNoToken
	}

	var token *lexemes.Token = &lexemes.Token{}

	state := lexemes.NewToken

	for _, ch := range c.Input {
		symbol := analyzeSymbol(ch)
		data := fsm[symbol][state]
		state = data.state
		action := data.action

		if action != nil {
			token = action(c, ch, token)
		}

		if state == lexemes.Error {
			return cerrors.ErrNoToken
		}
	}

	c.saveToken(token)
	return nil
}
