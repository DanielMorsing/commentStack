// Copyright 2025 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// commentStack is an experiment that tries to represent comments
// as state transitions in the go parser, via Cursors. It is adapted from gotype
//
// Right now, all it does is parse the example files and spit out
// which pairs of cursors comments lie between and which construction
// "anchors them"
//
// I'm working on an example that shows modification
package main

import (
	"cmp"
	"flag"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/scanner"
	"go/token"
	"log"
	"math/rand/v2"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"

	"github.com/DanielMorsing/commentStack/go/printer"

	"golang.org/x/tools/go/ast/inspector"
)

var (
	fset = token.NewFileSet()
	demo = flag.String("demo", "parse", "which demo to run, one of [parse]")
)

func main() {
	flag.Parse()
	pkgpath := flag.Arg(0)
	// For this implementation we need the source file, but if this gets traction,
	// we should add a PreviousToken and NextToken to the comment AST
	files, srcs, err := parseDir(pkgpath)
	if err != nil {
		log.Fatalf("cannot parse: %s", err)
	}
	inspect := inspector.New(files)
	root := inspect.Root()
	fileRanges := make([][]*commentRange, 0, len(files))

	for fileidx, file := range files {
		sortComments(file.Comments)
		ranges := make([]*commentRange, 0, len(file.Comments))
		// This is extra work, but the easiest way to get the info I need
		scan := &scanner.Scanner{}
		scan.Init(fset.File(file.Package), srcs[fileidx], nil, 0)
		cmtIdx := 0
		prevToken := file.FileStart
		for cmtIdx < len(file.Comments) {
			cmt := file.Comments[cmtIdx]
			pos, tok, lit := scan.Scan()
			if tok == token.SEMICOLON && lit == "\n" {
				continue
			}
			for cmt.End() <= pos {
				ranges = append(ranges, &commentRange{
					comment:   cmt,
					prevToken: prevToken,
					nextToken: pos,
				})
				cmtIdx += 1
				if cmtIdx == len(file.Comments) {
					break
				}
				cmt = file.Comments[cmtIdx]
			}
			switch tok {
			case token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
				prevToken = pos + token.Pos(len(lit))
			default:
				prevToken = pos + token.Pos(len(tok.String()))
			}
			if tok == token.EOF {
				panic("reached eof before end of comments (how??)")
			}
		}
		for _, r := range ranges {
			r.findCursors(root)
		}
		fileRanges = append(fileRanges, ranges)
	}

	switch *demo {
	case "parse":
		for i, f := range files {
			fmt.Println(fset.Position(f.Name.Pos()).Filename)
			for _, r := range fileRanges[i] {
				fmt.Println(r.String())
			}
		}
	case "passthrough":
		// remove all comments from the AST
		// TODO: if the go printer has a change
		// then this should probably be a flag.
		clearComments(root)
		for fidx, f := range files {
			// remove free floating comments
			f.Comments = nil
			cfg := printer.Config{
				Mode:        0,
				Tabwidth:    8,
				Indent:      0,
				Transitions: newPassthrough(fileRanges[fidx]),
			}
			cfg.Fprint(os.Stdout, fset, f)
		}
	case "shuffle":
		clearComments(root)
		for _, f := range files {
			//fileCur, _ := root.FindNode(f)
			//shuffleFunc(fileCur)
			printOrder(f, func(n ast.Node) {
				fmt.Printf("%T, %s\n", n, line(n))
			})
			cfg := printer.Config{
				Mode:     0,
				Tabwidth: 8,
				Indent:   0,
			}
			cfg.Fprint(os.Stdout, fset, f)
		}
	}
}

func shuffleFunc(fileCur inspector.Cursor) {
	types := []ast.Node{
		(*ast.FuncDecl)(nil),
	}
	for fns := range fileCur.Preorder(types...) {
		fnsn := fns.Node().(*ast.FuncDecl)
		if fnsn.Body == nil {
			continue
		}
		perm := rand.Perm(len(fnsn.Body.List))
		newBody := make([]ast.Stmt, len(fnsn.Body.List))
		for i, n := range perm {
			newBody[i] = fnsn.Body.List[n]
		}
		fnsn.Body.List = newBody
	}
}

func printOrder(node ast.Node, visit func(ast.Node)) {
	ast.Inspect(node, adaptInspect(visit))
}

func adaptInspect(visit func(ast.Node)) func(n ast.Node) bool {
	return func(n ast.Node) bool {
		if n == nil {
			return false
		}
		if printVisit(visit, n) {
			return false
		}
		visit(n)
		return true
	}
}

func printVisit(visit func(ast.Node), n ast.Node) bool {
	switch n := n.(type) {
	case *ast.ArrayType:
		printVisit1(visit, n, tok(n.Lbrack), tok(token.RBRACK), nod(n.Len), nod(n.Elt))
	case *ast.AssignStmt:
		printVisit1(visit, n, delimList(n.Lhs), tok(n.Tok), delimList(n.Rhs))
	case *ast.BasicLit:
		printVisit1(visit, n, tok(n.Kind))
	case *ast.BinaryExpr:
		printVisit1(visit, n, nod(n.X), tok(n.Op), nod(n.Y))
	case *ast.BlockStmt:
		printVisit1(visit, n, tok(n.Lbrace), list(n.List), tok(n.Rbrace))
	case *ast.BranchStmt:
		printVisit1(visit, n, tok(n.Tok), nod(n.Label))
	case *ast.CallExpr:
		printVisit1(visit, n, nod(n.Fun), tok(n.Lparen), delimList(n.Args), tok(n.Ellipsis), tok(n.Rparen))
	case *ast.CaseClause:
		printVisit1(visit, n, tok(n.Case), delimList(n.List), tok(n.Colon), list(n.Body))
	case *ast.ChanType:
		printVisit1(visit, n, tok(n.Begin), tok(n.Arrow), nod(n.Value))
	case *ast.CommClause:
		printVisit1(visit, n, tok(n.Case), nod(n.Comm), tok(n.Colon), list(n.Body))
	case *ast.CompositeLit:
		printVisit1(visit, n, nod(n.Type), tok(n.Lbrace), delimList(n.Elts), tok(n.Rbrace))
	case *ast.DeclStmt:
		printVisit1(visit, n, nod(n.Decl))
	case *ast.DeferStmt:
		printVisit1(visit, n, tok(n.Defer), nod(n.Call))
	case *ast.Ellipsis:
		printVisit1(visit, n, tok(n.Ellipsis), nod(n.Elt))
	case *ast.EmptyStmt:
		panic("what do?")
	case *ast.ExprStmt:
		printVisit1(visit, n, nod(n.X))
	case *ast.Field:
		printVisit1(visit, n, delimList(n.Names), nod(n.Type), nod(n.Tag))
	case *ast.FieldList:
		panic("missed fieldlist")
	case *ast.File:
		printVisit1(visit, n, tok(n.Package), nod(n.Name), list(n.Decls))
	case *ast.ForStmt:
		printVisit1(visit, n,
			tok(n.For),
			opt(n.Init, tok(token.SEMICOLON)),
			nod(n.Cond),
			opt(n.Post, tok(token.SEMICOLON)),
			nod(n.Body))

	case *ast.FuncDecl:
		// figure out how these work with cursor ranges.
		printVisit1(visit, n,
			tok(n.Type.Func),
			fieldlist(n.Recv, false),
			nod(n.Name),
			fieldlist(n.Type.TypeParams, true),
			fieldlist(n.Type.Params, true),
			fieldlist(n.Type.Results, true),
			nod(n.Body),
		)
	case *ast.IfStmt:
		printVisit1(visit, n, tok(n.If), opt(n.Init, tok(token.SEMICOLON)), nod(n.Cond), nod(n.Body), nod(n.Else))
	default:
		return false
	}
	return true
}

func printVisit1(visit func(ast.Node), parent ast.Node, components ...[]ast.Node) {
	var r []ast.Node
	for _, c := range components {
		r = append(r, c...)
	}
	for _, n := range r {
		switch n.(type) {
		case nodeToken:
			visit(parent)
		case ast.Node:
			if !printVisit(visit, n) {
				ast.Inspect(n, adaptInspect(visit))
			}
		}
	}
}

func fieldlist(f *ast.FieldList, tokenbetween bool) []ast.Node {
	if f == nil {
		return nil
	}
	listFunc := list[[]*ast.Field]
	if tokenbetween {
		listFunc = delimList[[]*ast.Field]
	}
	r := make([]ast.Node, 0, len(f.List)+2)
	r = append(r, tok(f.Opening)...)
	r = append(r, listFunc(f.List)...)
	r = append(r, tok(f.Closing)...)
	return r
}

type nodeToken struct{}

func (n nodeToken) Pos() token.Pos { panic("call to position") }
func (n nodeToken) End() token.Pos { panic("call to end position") }

func nod(n ast.Node) []ast.Node {
	if isZero(n) {
		return nil
	}
	return []ast.Node{n}
}

func tok(t any) []ast.Node {
	switch n := t.(type) {
	case token.Pos:
		if !n.IsValid() {
			return nil
		}
	case token.Token:
		if n == token.ILLEGAL {
			return nil
		}
	default:
		panic("bad tok")
	}
	return []ast.Node{nodeToken{}}
}

func opt(n ast.Node, delim []ast.Node) []ast.Node {
	if isZero(n) {
		return nil
	}
	ret := make([]ast.Node, 0, len(delim)+1)
	ret = append(ret, n)
	return append(ret, delim...)
}

func delimList[S ~[]E, E ast.Node](l S) []ast.Node {
	if len(l) == 0 {
		return nil
	}
	r := make([]ast.Node, 0, 2*len(l)-1)
	for i, n := range l {
		r = append(r, n)
		if i != len(l)-1 {
			r = append(r, nodeToken{})
		}
	}
	return r
}

func list[S ~[]E, E ast.Node](l S) []ast.Node {
	var r []ast.Node
	for _, n := range l {
		r = append(r, n)
	}
	return r
}

func isZero(n ast.Node) bool {
	return n == nil || reflect.ValueOf(n).IsZero()
}

func clearComments(root inspector.Cursor) {
	for fcur := range root.Children() {
		f := fcur.Node().(*ast.File)
		f.Comments = nil
	}
	for cur := range root.Preorder((*ast.CommentGroup)(nil)) {
		parent := cur.Parent().Node()
		switch n := parent.(type) {
		case *ast.Field:
			n.Doc = nil
			n.Comment = nil
		case *ast.File:
			n.Doc = nil
		case *ast.FuncDecl:
			n.Doc = nil
		case *ast.GenDecl:
			n.Doc = nil
		case *ast.ImportSpec:
			n.Doc = nil
			n.Comment = nil
		case *ast.TypeSpec:
			n.Doc = nil
			n.Comment = nil
		case *ast.ValueSpec:
			n.Doc = nil
			n.Comment = nil
		}
	}
}

func (r *commentRange) findCursors(cur inspector.Cursor) {
	r.prevCursor, _ = cur.FindByPos(r.prevToken, r.prevToken)
	r.nextCursor, _ = cur.FindByPos(r.nextToken, r.nextToken)

	if f, ok := r.prevCursor.Node().(*ast.File); ok && r.prevToken < f.Package {
		r.prevCursor = cur.Inspector().Root()
	}
}

type passthrough struct {
	begin map[ast.Node][]*commentRange
	end   map[ast.Node][]*commentRange
}

func newPassthrough(rngs []*commentRange) *passthrough {
	begin := make(map[ast.Node][]*commentRange)
	end := make(map[ast.Node][]*commentRange)
	for _, r := range rngs {
		begin[r.prevCursor.Node()] = append(begin[r.prevCursor.Node()], r)
		end[r.nextCursor.Node()] = append(end[r.nextCursor.Node()], r)
	}
	return &passthrough{
		begin: begin,
		end:   end,
	}
}

func (p *passthrough) Step(before ast.Node, after ast.Node) []*ast.CommentGroup {
	beginlist := p.begin[before]
	var cmtlist []*ast.CommentGroup
	for _, r := range beginlist {
		if r.nextCursor.Node() == after {
			cmtlist = append(cmtlist, r.comment)
		}
	}
	return cmtlist
}

type commentRange struct {
	comment    *ast.CommentGroup
	prevCursor inspector.Cursor
	nextCursor inspector.Cursor
	prevToken  token.Pos
	nextToken  token.Pos
}

func (r *commentRange) String() string {
	var b strings.Builder
	fmt.Fprintln(&b, line(r.comment))
	fmt.Fprintf(&b, "\tprev %s\n", line(r.prevToken))
	cursorstr := "<ROOT>"
	cNode := r.prevCursor.Node()
	if cNode != nil {
		cursorstr = fmt.Sprintf("%T %s", cNode, line(cNode))
	}
	fmt.Fprintf(&b, "\tprev Cursor %s\n", cursorstr)
	fmt.Fprintf(&b, "\tnext %s\n", line(r.nextToken))
	fmt.Fprintf(&b, "\tnext Cursor %T %s\n", r.nextCursor.Node(), line(r.nextCursor.Node()))

	return b.String()
}

func line(iface any) string {
	var pos, end token.Pos
	switch n := iface.(type) {
	case ast.Node:
		pos, end = n.Pos(), n.End()
	case token.Pos:
		pos = n
	default:
		panic("bad line")
	}
	begin := fset.Position(pos)
	endstr := ""
	if end != token.NoPos {
		endPos := fset.Position(end)
		endstr = fmt.Sprintf(":%v:%v", endPos.Line, endPos.Column)
	}
	return fmt.Sprintf("./%s:%v:%v%s", begin.Filename, begin.Line, begin.Column, endstr)
}

func sortComments(comms []*ast.CommentGroup) {
	slices.SortFunc(comms, func(a, b *ast.CommentGroup) int {
		return cmp.Compare(a.Pos(), b.Pos())
	})
}

func parseDir(dir string) ([]*ast.File, [][]byte, error) {
	ctxt := build.Default
	pkginfo, err := ctxt.ImportDir(dir, 0)
	if _, nogo := err.(*build.NoGoError); err != nil && !nogo {
		return nil, nil, err
	}
	filenames := append(pkginfo.GoFiles, pkginfo.CgoFiles...)
	return parseFiles(dir, filenames)
}

func parseFiles(dir string, filenames []string) ([]*ast.File, [][]byte, error) {
	files := make([]*ast.File, len(filenames))
	srcs := make([][]byte, len(filenames))

	for i, filename := range filenames {
		var err error
		filepath := filepath.Join(dir, filename)
		srcs[i], err = os.ReadFile(filepath)
		if err != nil {
			return nil, nil, err
		}
		files[i], err = parse(filepath, srcs[i])
		if err != nil {
			return nil, nil, err
		}
	}
	return files, srcs, nil
}

// parse may be called concurrently
func parse(filename string, src any) (*ast.File, error) {
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments|parser.SkipObjectResolution) // ok to access fset concurrently
	//ast.Print(fset, file)
	return file, err
}
