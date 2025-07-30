package command

/*
grammar

expression = term | expression "+" term | expression "-" term
term = factor | term "*" factor | term "/" factor
factor = number | "(" expression ")"
*/

import (
	"fmt"

	"calculator/internal/lexemes"
	"calculator/internal/myast"
	"calculator/internal/myerrors"
)

/*
The factor makes ast of the factor in the in variable.
Returns error if in variable is not the factor.
*/
func factor(in []lexemes.Token) (ast myast.Ast, err error) {
	ast = myast.NewAst()

	if len(in) == 0 {
		return ast, myerrors.ErrNoFactor
	}

	if len(in) == 1 {
		if in[0].T == lexemes.NumberLexeme {
			node := myast.NewNode(in[0])
			ast.MustAppendNode(ast.Root.Id(), &node)

			return ast, nil
		}
	}

	if in[0].T == lexemes.Delimiter && in[0].Lex == "(" &&
		in[len(in)-1].T == lexemes.Delimiter && in[len(in)-1].Lex == ")" {
		return parse(in[1 : len(in)-1])
	}

	return ast, myerrors.ErrNoFactor
}

/*
The term makes ast of the term in the in variable.
Returns error if in variable is not the term.
*/
func term(in []lexemes.Token) (ast myast.Ast, err error) {
	ast = myast.NewAst()

	factorAst, err := factor(in)
	i := 1

	for err != nil && i < len(in) {
		factorAst, err = factor(in[i:])
		i++
	}
	i--

	if err != nil {
		return
	}

	if i > 0 {
		token := in[i-1]
		if token.T == lexemes.Operator && (token.Lex == "*" || token.Lex == "/") {
			t := in[:i-1]

			var termAst myast.Ast

			node := myast.NewNode(token)
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)

			termAst, err = term(t)

			if err != nil {
				return
			}

			ast.MustAppend(nodeId, &termAst)
			ast.MustAppend(nodeId, &factorAst)

			return ast, nil
		}

	}

	if i == 0 {
		return factorAst, nil
	}

	return ast, myerrors.ErrNoTerm
}

/*
The expr makes ast of the expr in the in variable.
Returns error if in variable is not the expr.
*/
func expr(in []lexemes.Token) (ast myast.Ast, err error) {
	ast = myast.NewAst()

	termAst, err := term(in)
	i := 1

	for err != nil && i < len(in) {
		termAst, err = term(in[i:])
		i++
	}
	i--

	if err != nil {
		return
	}

	if i > 0 {
		token := in[i-1]
		if token.T == lexemes.Operator && (token.Lex == "+" || token.Lex == "-") {
			t := in[:i-1]

			var exprAst myast.Ast
			var node myast.Node
			node.Parent = ast.Root
			node.Value = token
			nodeId := ast.MustAppendNode(ast.Root.Id(), &node)

			exprAst, err = expr(t)

			if err != nil {
				return
			}

			ast.MustAppend(nodeId, &exprAst)
			ast.MustAppend(nodeId, &termAst)

			return ast, nil
		}

	}

	if i == 0 {
		return termAst, nil
	}

	return ast, myerrors.ErrNoExpr
}

func parse(in []lexemes.Token) (ast myast.Ast, err error) {
	ast, err = expr(in)
	return
}

func (c *Command) Parse() (err error) {
	c.Ast, err = parse(c.Tokens)

	if err != nil {
		return fmt.Errorf("%s : %s", myerrors.ErrParse, err)
	}

	return nil
}
