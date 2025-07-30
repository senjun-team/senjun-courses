package cast

import (
	"fmt"

	"calculator/internal/cerrors"
	"calculator/internal/lexemes"
)

type Node struct {
	id       int
	Parent   *Node
	Children []*Node
	Value    lexemes.Token
}

type Ast struct {
	nextId int
	Root   *Node
}

func NewNode(v lexemes.Token) Node {
	var n Node
	n.Value = v
	return n
}

func NewAst() Ast {
	var ast Ast
	var node Node
	ast.Root = &node
	ast.Root.id = ast.nextId
	ast.nextId++

	ast.Root.Value.Lex = "start"
	ast.Root.Value.T = lexemes.StartLexeme

	return ast
}

/*
The MustAppendNode appends node v to the node with id=parentId of the ast a.
Returns new id of the node v in the ast.
Panics if fails to find the node with id=parentId in the ast a.
*/
func (a *Ast) MustAppendNode(parentId int, v *Node) (id int) {
	node, err := a.Node(parentId)
	if err != nil {
		panic(fmt.Errorf("%s : %s, id: %d", cerrors.ErrAppendNode, err, parentId))
	}

	node.Children = append(node.Children, v)
	v.Parent = node
	v.id = a.nextId
	a.nextId++

	return v.id
}

/*
The MustAppend appends the root of the ast v to the node with parentId of the ast a.
Panics is fails to find the node with id=parentId in the ast a.
*/
func (a *Ast) MustAppend(parentId int, v *Ast) {

	var parents []*Node
	parents = append(parents, v.Root)

	id := parentId

	for i := 0; i < len(parents); i++ {
		for _, child := range parents[i].Children {
			if child.Parent.id == v.Root.id {
				a.MustAppendNode(id, child)
			} else {
				child.id = a.nextId
				a.nextId++
			}
			parents = append(parents, child)
		}
	}
}

func (n *Node) Id() int {
	return n.id
}

func (a *Ast) Len() int {
	return a.nextId
}

func (a *Ast) Node(id int) (*Node, error) {
	return a.Root.findNodeById(id)
}

/*
The findNodeById returns *Node by it's id scanning the nodes from n as root.
Returns ErrNoNode if there is no node with such id.
*/
func (n *Node) findNodeById(id int) (res *Node, err error) {
	if n.id == id {
		return n, nil
	}
	for _, child := range n.Children {
		res, err = child.findNodeById(id)
		if err == nil {
			return
		}
	}

	return n, cerrors.ErrNoNode
}

/*
The printInLevel prints the lexeme of the node to the terminal with format of the level in the ast.
Returns branchLevels where ├─ or │ was printed.
Wants prevBranchLevels which is the branchLevels from the previos call.
Set prevBranchLevels = map[int]struct{}{} if there was no previous call.
*/
func (n *Node) printInLevel(level int, prevBranchLevels map[int]struct{}) (branchLevels map[int]struct{}) {
	branchLevels = make(map[int]struct{})

	for i := 0; i < level; i++ {
		_, ok := prevBranchLevels[i]
		if ok {
			fmt.Print("│  ")
			branchLevels[i] = struct{}{}
		} else {
			fmt.Print("   ")
		}
	}

	if level != 0 {
		if n.Parent == nil || n.Parent.Children[len(n.Parent.Children)-1].id == n.id {
			fmt.Print("└─ ")
		} else {
			fmt.Print("├─ ")
			branchLevels[level] = struct{}{}
		}
	}

	fmt.Println(n.Value.Lex)

	for _, child := range n.Children {
		branchLevels = child.printInLevel(level+1, branchLevels)
	}

	return
}

// The Print prints the ast in the terminal.
func (a *Ast) Print() {
	a.Root.printInLevel(0, map[int]struct{}{})
}

// The TerminalNodes returns all the terminal nodes from n as root.
func (n *Node) TerminalNodes() (res []*Node) {

	for _, child := range n.Children {
		res = append(res, child.TerminalNodes()...)
	}

	if len(n.Children) == 0 {
		res = append(res, n)
	}

	return res
}

// The NonTerminalNodes returns all the nonterminal nodes from n as root.
func (n *Node) NonTerminalNodes() (res []*Node) {

	if len(n.Children) > 0 {

		res = append(res, n)

		for _, child := range n.Children {
			res = append(res, child.NonTerminalNodes()...)
		}
	}

	return res
}
