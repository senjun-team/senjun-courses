package command

import (
	"calculator/internal/lexemes"
	"calculator/internal/myast"
	"calculator/internal/myerrors"
	"fmt"
	"strconv"
)

func (c *Command) Solve() (float64, error) {
	token, err := solve(c.Ast.Root.Children[0])
	if err != nil {
		return 0, err
	}
	res, err := strconv.ParseFloat(string(token.Lex), 64)
	if err != nil {
		return 0, fmt.Errorf("%s : %s", myerrors.ErrSolve, err)
	}
	return res, nil
}

func solve(n *myast.Node) (lexemes.Token, error) {
	if len(n.Children) == 0 {
		return n.Value, nil
	}
	leftToken, err := solve(n.Children[0])
	if err != nil {
		return leftToken, err
	}
	rightToken, err := solve(n.Children[1])
	if err != nil {
		return rightToken, err
	}
	return compute(leftToken, rightToken, n.Value)
}

func compute(leftToken lexemes.Token,
	rightToken lexemes.Token, op lexemes.Token) (lexemes.Token, error) {
	a, err := strconv.ParseFloat(string(leftToken.Lex), 64)

	if err != nil {
		return lexemes.Token{}, fmt.Errorf("%s : %s", myerrors.ErrSolve, err)
	}

	b, err := strconv.ParseFloat(string(rightToken.Lex), 64)

	if err != nil {
		return lexemes.Token{}, fmt.Errorf("%s : %s", myerrors.ErrSolve, err)
	}

	if op.Lex == "+" {
		return lexemes.Token{Lex: lexemes.Lexeme(fmt.Sprint(a + b)),
			T: lexemes.Operator}, nil
	}
	if op.Lex == "-" {
		return lexemes.Token{Lex: lexemes.Lexeme(fmt.Sprint(a - b)),
			T: lexemes.Operator}, nil
	}
	if op.Lex == "*" {
		return lexemes.Token{Lex: lexemes.Lexeme(fmt.Sprint(a * b)),
			T: lexemes.Operator}, nil
	}
	if op.Lex == "/" {
		return lexemes.Token{Lex: lexemes.Lexeme(fmt.Sprint(a / b)),
			T: lexemes.Operator}, nil
	}
	return lexemes.Token{}, myerrors.ErrSolve
}
