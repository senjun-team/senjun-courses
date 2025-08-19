package command

import (
	"calculator/internal/cast"
	"calculator/internal/lexemes"
)

type Command struct {
	Input  string
	Tokens []*lexemes.Token
	Ast    cast.Ast
}

func (c *Command) ReadCommand(rawExpression string) {
	c.Input = rawExpression
}
