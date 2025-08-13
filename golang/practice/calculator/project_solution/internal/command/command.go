package command

import (
	"calculator/internal/cast"
	"calculator/internal/lexemes"
)

type Command struct {
	Input    string
	Subinput string // to copy Input without spaces and work with it inside package
	Tokens   []*lexemes.Token
	Ast      cast.Ast
}

func (c *Command) ReadCommand(rawExpression string) {
	c.Input = rawExpression
	c.Subinput = rawExpression
}
