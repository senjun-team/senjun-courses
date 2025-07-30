package command

import (
	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
	"fmt"
)

// The lookAhead looks ahead to the next shortest token but does not remove the token from Subinput.
func (c *Command) lookAhead() (lexemes.Token, error) {
	var buf string

	for _, ch := range c.Subinput {
		buf += string(ch)
		bufType := Dictionary().Find(lexemes.Lexeme(buf))

		if bufType != 0 {
			return lexemes.Token{Lex: lexemes.Lexeme(buf), T: bufType}, nil
		}
	}

	if len(c.Subinput) == 0 {
		return lexemes.Token{}, nil
	}

	return lexemes.Token{Lex: lexemes.Lexeme(buf), T: 0}, cerrors.ErrNoToken
}

// The nextToken gets next longest token and removes this token from subinput.
func (c *Command) nextToken(prevToken lexemes.Token) (lexemes.Token, error) {

	var newToken lexemes.Token

loop:
	for aheadToken, err := c.lookAhead(); (err == cerrors.ErrNoToken || !Dictionary().IsStop(aheadToken.T)) &&
		len(aheadToken.Lex) != 0; aheadToken, err = c.lookAhead() {

		buf := string(newToken.Lex)

		for idx, ch := range c.Subinput {
			buf += string(ch)
			bufType := Dictionary().Find(lexemes.Lexeme(buf))

			if bufType != 0 {
				newToken.Lex = lexemes.Lexeme(buf)
				newToken.T = bufType
				c.Subinput = c.Subinput[idx+1:]
				break
			}

			if idx == len(c.Subinput)-1 {
				break loop
			}
		}
	}

	if len(newToken.Lex) == 0 {
		newToken, err := c.lookAhead()

		if err != nil {
			return newToken, err
		}

		c.Subinput = c.Subinput[len(newToken.Lex):]

		// unary minus
		if newToken.Lex == "-" {
			nextToken, err := c.lookAhead()

			if err != nil {
				return nextToken, err
			}

			if (prevToken.T == 0 || prevToken.Lex == "(") && nextToken.T == lexemes.NumberLexeme {
				newToken, err = c.nextToken(newToken)
				if err != nil {
					return newToken, err
				}
				newToken.Lex = "-" + newToken.Lex
			}
		}

		return newToken, err
	}

	return newToken, nil

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

	return nil
}
