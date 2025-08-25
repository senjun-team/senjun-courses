package cerrors

import "errors"

var (
	ErrNoToken    = errors.New("no token found")
	ErrCalculate  = errors.New("calculating failed")
	ErrNoFactor   = errors.New("no factor")
	ErrParse      = errors.New("parsing failed")
	ErrNoNode     = errors.New("no node")
	ErrAppendNode = errors.New("appending node failed")
	ErrSolve      = errors.New("solve failed")
)
