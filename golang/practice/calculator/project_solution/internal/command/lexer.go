package command

import (
	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
	"fmt"
	"unicode"
)

func (c *Command) saveToken(token *lexemes.Token) {
	c.Tokens = append(c.Tokens, token)
}

func startAccumulatingNumber(c *Command,
	char string,
	token *lexemes.Token) {
	c.saveToken(token)
	token.T = lexemes.TokenNumber
	token.Lex = lexemes.Lexeme(char)
}

func accumulateNumber(c *Command,
	char string,
	token *lexemes.Token) {
	token.Lex += lexemes.Lexeme(char)
}

func accumulateOperator(c *Command,
	char string,
	token *lexemes.Token) {
	c.saveToken(token)
	token.T = lexemes.TokenOperator
	token.Lex = lexemes.Lexeme(char)
}

func accumulateParanthesis(c *Command,
	char string,
	token *lexemes.Token) {
	c.saveToken(token)
	token.T = lexemes.TokenParanthesis
	token.Lex = lexemes.Lexeme(char)
}

// finite-state machine for the lexical analysis of the expression
type FsmT map[int]map[int]struct {
	int func(*Command, string, *lexemes.Token)
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

func (c *Command) LexicalAnalyze() error {

	var newToken lexemes.Token

	for newToken, err := c.nextToken(newToken); len(newToken.Lex) > 0; newToken,
		err = c.nextToken(newToken) {

		if err != nil {
			return fmt.Errorf("%s : %s", cerrors.ErrLexAnalysis, err)
		}

		c.Tokens = append(c.Tokens, newToken)
	}

	// expression has single negative number
	if len(c.Tokens) == 1 && string(c.Tokens[0].Lex[0]) == "-" &&
		len(c.Subinput) == 0 {
		return cerrors.ErrNoToken
	}

	return nil
}
