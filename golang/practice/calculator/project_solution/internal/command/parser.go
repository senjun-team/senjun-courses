package command

/*
grammar

expression = term | expression "+" term | expression "-" term
term = factor | term "*" factor | term "/" factor
factor = number | "(" expression ")"
*/

import (
	"fmt"

	"calculator/internal/cast"
	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
)

/*
The factor makes ast of the factor in the in variable.
Returns error if in variable is not the factor.
*/
func factor(in []lexemes.Token) (ast cast.Ast, err error) {
	ast = cast.NewAst()

	if len(in) == 0 {
		return ast, cerrors.ErrNoFactor
	}

	if len(in) == 1 {
		if in[0].T == lexemes.NumberLexeme {
			node := cast.NewNode(in[0])
			ast.MustAppendNode(ast.Root.Id(), &node)

			return ast, nil
		}
	}

	if in[0].T == lexemes.Delimiter && in[0].Lex == "(" &&
		in[len(in)-1].T == lexemes.Delimiter && in[len(in)-1].Lex == ")" {
		return parse(in[1 : len(in)-1])
	}

	return ast, cerrors.ErrNoFactor
}

/*
The term makes ast of the term in the in variable.
Returns error if in variable is not the term.
Pass the startPos = len(in) - 1.
*/
func term(in []lexemes.Token, startPos int) (ast cast.Ast, err error) {
	ast = cast.NewAst()
	i := startPos
	err = cerrors.ErrParse
	var factorAst cast.Ast

	for err != nil && i >= 0 {
		factorAst, err = factor(in[i:])
		i--
	}

	i++

	if err != nil {
		return
	}

	if i > 0 {
		token := in[i-1]
		if token.T == lexemes.Operator && (token.Lex == "*" || token.Lex == "/") {
			t := in[:i-1]

			var termAst cast.Ast

			node := cast.NewNode(token)
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)
			termAst, err = term(t, len(t)-1)

			if err == nil {

				ast.MustAppend(nodeId, &termAst)
				ast.MustAppend(nodeId, &factorAst)
				return ast, nil
			}
		}

	}

	if i == 0 {
		return factorAst, nil
	}

	if i < len(in) {
		return term(in, i-1)
	}

	return ast, cerrors.ErrNoTerm
}

/*
The expr makes ast of the expr in the in variable.
Returns error if in variable is not the expr.
Pass the startPos = len(in) - 1.
*/
func expr(in []lexemes.Token, startPos int) (ast cast.Ast, err error) {
	ast = cast.NewAst()

	err = cerrors.ErrParse
	i := startPos
	var termAst cast.Ast

	for err != nil && i >= 0 {
		termAst, err = term(in[i:], len(in[i:])-1)
		i--
	}

	i++

	if err != nil {
		return
	}

	if i > 0 {
		token := in[i-1]
		if token.T == lexemes.Operator && (token.Lex == "+" || token.Lex == "-") {
			t := in[:i-1]

			var exprAst cast.Ast
			var node cast.Node
			node.Parent = ast.Root
			node.Value = token
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)

			exprAst, err = expr(t, len(t)-1)

			if err == nil {

				ast.MustAppend(nodeId, &exprAst)
				ast.MustAppend(nodeId, &termAst)
				return ast, nil
			}
		}

	}

	if i == 0 {
		return termAst, nil
	}

	if i < len(in) {
		return expr(in, i-1)
	}

	return ast, cerrors.ErrNoExpr
}

func parse(in []lexemes.Token) (ast cast.Ast, err error) {
	ast, err = expr(in, len(in)-1)
	return
}

func (c *Command) Parse() (err error) {
	c.Ast, err = parse(c.Tokens)

	if err != nil {
		return fmt.Errorf("%s : %s", cerrors.ErrParse, err)
	}

	return nil
}
