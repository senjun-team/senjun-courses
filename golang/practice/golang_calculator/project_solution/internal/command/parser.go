package command

import (
	"calculator/internal/cast"
	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
	"fmt"
)

/*
grammar:

expr = expr "+" term | expr "-" term | term
term = term "*" factor | term "/" factor | factor
factor = "(" expr ")" | number

another words:
expr = term ["+","-"] term ["+","-"] ... term
term = factor ["*","/"] factor ["*","/"] ... factor
factor = number | "(" expr ")"
*/

type cparser struct {
	command   *Command
	pos       int
	tokenval  lexemes.Token
	lookahead lexemes.Token
}

func newCparser(c *Command) cparser {
	c.Ast = cast.NewAst()
	return cparser{command: c}
}

func (c *cparser) viewLex() {
	switch {
	case c.pos == 0:
		c.lookahead = *c.command.Tokens[c.pos]
		c.pos++
	case c.pos < len(c.command.Tokens):
		c.lookahead = *c.command.Tokens[c.pos]
		c.tokenval = *c.command.Tokens[c.pos-1]
		c.pos++
	case c.pos == len(c.command.Tokens):
		c.lookahead = lexemes.Token{}
		c.tokenval = *c.command.Tokens[c.pos-1]
		c.pos++
	}
}

func (c *cparser) match(t lexemes.Token) (err error) {
	if c.lookahead == t {
		c.viewLex()
		return nil
	}
	err = cerrors.ErrParse
	return
}

func (c *cparser) expr() (cast.Ast, error) {
	leftAst, err := c.term()
	if err != nil {
		return leftAst, err
	}
	for {
		switch {
		case c.lookahead.Lex == "+" || c.lookahead.Lex == "-":
			t := c.lookahead
			err := c.match(c.lookahead)
			if err != nil {
				return leftAst, err
			}
			ast := cast.NewAst()
			node := cast.NewNode(t)
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)
			ast.MustAppend(nodeId, &leftAst)
			rightAst, err := c.term()
			if err != nil {
				return rightAst, err
			}
			ast.MustAppend(nodeId, &rightAst)
			leftAst = ast
			continue
		default:
			return leftAst, nil
		}
	}
}

func (c *cparser) term() (cast.Ast, error) {
	leftAst, err := c.factor()
	if err != nil {
		return leftAst, err
	}
	for {
		switch {
		case c.lookahead.Lex == "*" || c.lookahead.Lex == "/":
			t := c.lookahead
			err := c.match(c.lookahead)
			if err != nil {
				return leftAst, err
			}
			ast := cast.NewAst()
			node := cast.NewNode(t)
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)
			ast.MustAppend(nodeId, &leftAst)
			rightAst, err := c.factor()
			if err != nil {
				return rightAst, err
			}
			ast.MustAppend(nodeId, &rightAst)
			leftAst = ast
			continue
		default:
			return leftAst, nil
		}
	}
}

func (c *cparser) factor() (cast.Ast, error) {

	switch {
	case c.lookahead.Lex == "(":
		err := c.match(c.lookahead)
		if err != nil {
			return cast.Ast{}, err
		}
		ast, err := c.expr()
		if err != nil {
			return ast, err
		}
		err = c.match(lexemes.Token{T: lexemes.TokenParanthesis, Lex: ")"})
		if err != nil {
			return ast, err
		}
		return ast, err
	case c.lookahead.T == lexemes.TokenNumber:
		ast := cast.NewAst()
		node := cast.NewNode(c.lookahead)
		ast.MustAppendNode(ast.Root.Id(), &node)
		err := c.match(c.lookahead)
		if err != nil {
			return ast, err
		}
		return ast, nil
	}
	return cast.Ast{}, cerrors.ErrNoFactor
}

func (c *cparser) parse() (err error) {
	c.viewLex()
	ast, err := c.expr()
	if err != nil {
		return err
	}
	if c.pos <= len(c.command.Tokens) {
		return cerrors.ErrNoToken
	}
	c.command.Ast = ast
	return nil
}

func (c *Command) Parse() (err error) {
	cparser := newCparser(c)
	err = cparser.parse()

	if err != nil {
		return fmt.Errorf("%s : %s", cerrors.ErrParse, err)
	}

	return nil
}
