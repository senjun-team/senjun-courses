package cerrors

import "errors"

var (
	ErrNoToken      = errors.New("no token found")
	ErrPointError   = errors.New("the expression contains a number with more than one point")
	ErrInvalidToken = errors.New("the expression contains an invalid/incorrect token")
	ErrCalculate    = errors.New("calculating failed")
	ErrNoFactor     = errors.New("no factor")
	ErrParse        = errors.New("parsing failed")
	ErrNoNode       = errors.New("no node")
	ErrAppendNode   = errors.New("appending node failed")
	ErrSolve        = errors.New("solve failed")
)
