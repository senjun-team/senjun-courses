package myerrors

import "errors"

var (
	ErrNoToken     = errors.New("no token found")
	ErrLexAnalysis = errors.New("lexical analysis failed")
	ErrRead        = errors.New("reading program failed")
	ErrCalculate   = errors.New("calculating failed")
	ErrNoFactor    = errors.New("no factor")
	ErrNoTerm      = errors.New("no term")
	ErrNoExpr      = errors.New("no expression")
	ErrParse       = errors.New("parsing failed")
	ErrNoNode      = errors.New("no node")
	ErrAppendNode  = errors.New("appending node failed")
	ErrSolve       = errors.New("solve failed")
)
