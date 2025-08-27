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
	"go/token"
	"log"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"

	"github.com/DanielMorsing/commentStack/go/printer"

	"golang.org/x/tools/go/ast/edge"
	"golang.org/x/tools/go/ast/inspector"
)

var (
	fset = token.NewFileSet()
	demo = flag.String("demo", "parse", "which demo to run, one of [parse]")
)

func main() {
	flag.Parse()
	pkgpath := flag.Arg(0)
	files, err := parseDir(pkgpath)
	if err != nil {
		log.Fatalf("cannot parse: %s", err)
	}
	inspect := inspector.New(files)
	root := inspect.Root()
	fileRanges := make([][]*commentRange, 0, len(files))

	for _, file := range files {
		fileCur, ok := root.FindNode(file)
		if !ok {
			log.Panicf("no cursor for file")
		}
		sortComments(file.Comments)
		ranges := make([]*commentRange, len(file.Comments))
		// Each comment will be bounded by 2 cursors.
		// One cursor for the node that last produced a token before the comment
		// and one cursor for the node that will introduce a token after the comment
		for i, cmt := range file.Comments {
			outer, _ := root.FindByPos(cmt.Pos(), cmt.End())
			for commentKind(outer.ParentEdge()) {
				outer = outer.Parent()
			}
			if file == outer.Node() && cmt.Pos() < file.Package {
				// comments before the package keyword live in a special space between the
				// root cursor and the file
				r := &commentRange{
					comment:    cmt,
					prevCursor: root,
					nextCursor: fileCur,
					prevToken:  file.FileStart,
					nextToken:  file.Package,
				}
				ranges[i] = r
				continue
			}
			ranges[i] = findLimits(cmt, outer)
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
		for cur := range root.Preorder((*ast.CommentGroup)(nil)) {
			parent := cur.Parent().Node()
			switch n := parent.(type) {
			case *ast.Field:
				n.Doc = nil
			case *ast.File:
				n.Doc = nil
			case *ast.FuncDecl:
				n.Doc = nil
			case *ast.GenDecl:
				n.Doc = nil
			case *ast.ImportSpec:
				n.Doc = nil
			case *ast.TypeSpec:
				n.Doc = nil
			case *ast.ValueSpec:
				n.Doc = nil
			}
		}
		for _, f := range files {
			// remove free floating comments
			f.Comments = nil
			cfg := printer.Config{
				Transitions: &transitionsPrinter{},
			}
			cfg.Fprint(os.Stdout, fset, f)
			fcur, _ := root.FindNode(f)
			for c := range fcur.Preorder() {
				fmt.Printf("%T\n", c.Node())
			}
		}
	}
}

type transitionsPrinter struct{}

func (t *transitionsPrinter) Step(before ast.Node, after ast.Node) *ast.CommentGroup {
	if before == nil {
		return nil
	}
	if f, ok := before.(*ast.FieldList); ok {
		if !f.Opening.IsValid() {
			fmt.Printf("fieldlist %s\n", line(before))
		}
	}
	fmt.Printf("%T->%T %s\n", before, after, line(before))
	return nil
}

func findLimits(cmt *ast.CommentGroup, outer inspector.Cursor) *commentRange {
	// These are the specific tokens that bound the comment
	prevToken, nextToken := getTokens(outer, cmt)

	prevCursor, _ := outer.FindByPos(prevToken, prevToken)
	for {
		parent := prevCursor.Parent()
		if parent.Node().End() > cmt.Pos() {
			break
		}
		prevCursor = parent
	}
	nextCursor, _ := outer.FindByPos(nextToken, nextToken)
	for {
		parent := nextCursor.Parent()
		if parent.Node() == nil || cmt.End() > parent.Node().Pos() {
			break
		}
		nextCursor = parent
	}
	return &commentRange{
		comment:    cmt,
		prevToken:  prevToken,
		nextToken:  nextToken,
		prevCursor: prevCursor,
		nextCursor: nextCursor,
	}
}

func getTokens(cur inspector.Cursor, cmt *ast.CommentGroup) (prev, next token.Pos) {
	switch n := cur.Node().(type) {
	case *ast.ArrayType:
		if n.Len == nil {
			return n.Lbrack, n.Elt.Pos()
		}
		if cmt.End() < n.Len.Pos() {
			return n.Lbrack, n.Len.Pos()
		}
		return n.Len.Pos(), n.Elt.Pos()
	case *ast.AssignStmt:
		begin, end, ok := commentBetweenList(nt(token.NoPos), n.Lhs, nt(n.TokPos), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetweenList(nt(n.TokPos), n.Rhs, nt(token.NoPos), cmt)
		if ok {
			return begin, end
		}
		panic("did not find comment in assign stmt")
	case *ast.BasicLit:
		panic("comments cannot occur in literals")
	case *ast.BinaryExpr:
		// comment can only occur before or after the op
		// anything else, it would be found in the sub-expression
		if cmt.End() < n.OpPos {
			return n.X.End(), n.OpPos
		}
		return n.OpPos, n.Y.Pos()
	case *ast.BlockStmt:
		if len(n.List) == 0 {
			return n.Lbrace, n.Rbrace
		}
		begin, end, ok := commentBetweenList(nt(n.Lbrace), n.List, nt(n.Rbrace), cmt)
		if ok {
			return begin, end
		}
		panic("did not find stmt")
	case *ast.BranchStmt:
		return n.TokPos, n.Label.Pos()
	case *ast.CallExpr:
		if cmt.End() < n.Lparen {
			return n.Fun.End(), n.Lparen
		}
		endtok := n.Rparen
		if n.Ellipsis != token.NoPos {
			endtok = n.Ellipsis
		}
		begin, end, ok := commentBetweenList(nt(n.Lparen), n.Args, nt(endtok), cmt)
		if ok {
			return begin, end
		}
		return n.Ellipsis, n.Rparen
	case *ast.CaseClause:
		begin, end, ok := commentBetweenList(nt(n.Case), n.List, nt(n.Colon), cmt)
		if ok {
			return begin, end
		}
		// case clauses are terminated by the next caseclause or the end
		// of the enclosing switch body, we need the parent for either
		body := cur.Parent()
		endtok := token.NoPos
		lastchild, ok := body.LastChild()
		if !ok {
			panic("???")
		}
		if lastchild == cur {
			endtok = body.Node().End()
		} else {
			// get the next caseclause in this switch, it is our end token
			k, i := cur.ParentEdge()
			endtok = body.ChildAt(k, i+1).Node().Pos()
		}
		begin, end, ok = commentBetweenList(nt(n.Colon), n.Body, nt(endtok), cmt)
		if !ok {
			panic("did not find comment")
		}
		return begin, end

	case *ast.CompositeLit:
		begin, end, ok := commentBetween(n.Type, nt(n.Lbrace), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetweenList(nt(n.Lbrace), n.Elts, nt(n.Rbrace), cmt)
		if ok {
			return begin, end
		}
	case *ast.FieldList:
		begin, end, ok := commentBetweenList(nt(n.Opening), n.List, nt(n.Closing), cmt)
		if ok {
			return begin, end
		}
	case *ast.File:
		if cmt.End() < n.Name.Pos() {
			return n.Package, n.Name.Pos()
		}
		begin, end, ok := commentBetweenList(n.Name, n.Decls, nt(n.FileEnd), cmt)
		if ok {
			return begin, end
		}
		panic("did not find stmt")
	case *ast.FuncDecl:
		nextTok := n.Name.Pos()
		if n.Recv != nil {
			nextTok = n.Recv.Pos()
		}
		// only place before the receiver list is the func keywor
		if cmt.End() < nextTok {
			return n.Type.Func, nextTok
		}
	case *ast.GenDecl:
		begintok := n.TokPos
		if n.Lparen != token.NoPos {
			begintok = n.Lparen
			begin, end, ok := commentBetween(nt(n.TokPos), nt(n.Lparen), cmt)
			if ok {
				return begin, end
			}
		}
		begin, end, ok := commentBetweenList(nt(begintok), n.Specs, nt(n.Rparen), cmt)
		if ok {
			return begin, end
		}

	case *ast.IfStmt:
		beginNode := ast.Node(nt(n.If))
		if n.Init != nil {
			beginNode = n.Init
			begin, end, ok := commentBetween(nt(n.If), n.Init, cmt)
			if ok {
				return begin, end
			}
		}
		begin, end, ok := commentBetween(beginNode, n.Cond, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(n.Cond, n.Body, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(n.Body, n.Else, cmt)
		if ok {
			return begin, end
		}
		panic("did not find comment in if stmt")
	case *ast.KeyValueExpr:
		begin, end, ok := commentBetween(n.Key, nt(n.Colon), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(nt(n.Colon), n.Value, cmt)
		if ok {
			return begin, end
		}
	}
	log.Panicf("unhandled node %T, %s", cur.Node(), line(cmt))
	panic("unreachable")
}

type nt token.Pos

func (n nt) Pos() token.Pos { return token.Pos(n) }
func (n nt) End() token.Pos { return token.Pos(n + 1) }

func commentBetween(begin ast.Node, end ast.Node, cmt *ast.CommentGroup) (token.Pos, token.Pos, bool) {
	if begin.End() < cmt.Pos() && cmt.End() < end.Pos() {
		return begin.End(), end.Pos(), true
	}
	return token.NoPos, token.NoPos, false
}

func commentBetweenList[L ~[]N, N ast.Node](begin ast.Node, nodeList L, end ast.Node, cmt *ast.CommentGroup) (token.Pos, token.Pos, bool) {
	if begin.End().IsValid() && end.Pos().IsValid() && len(nodeList) == 0 {
		if cmt.End() < begin.End() || end.Pos() < cmt.Pos() {
			panic("bad call")
		}
		return begin.End(), end.Pos(), true
	}
	if begin.End().IsValid() && len(nodeList) > 0 && cmt.End() < nodeList[0].Pos() {
		return begin.End(), nodeList[0].Pos(), true
	}
	for i := 1; i < len(nodeList); i++ {
		pn := nodeList[i-1]
		nn := nodeList[i]
		if pn.End() < cmt.Pos() && cmt.End() < nn.Pos() {
			return pn.End(), nn.Pos(), true
		}
	}
	if cmt.End() < end.Pos() {
		return nodeList[len(nodeList)-1].End(), end.Pos(), true
	}
	return token.NoPos, token.NoPos, false
}

func commentKind(ek edge.Kind, _ int) bool {
	// TODO(dmo): line comments??
	switch ek {
	case edge.Field_Doc, edge.File_Doc, edge.FuncDecl_Doc,
		edge.ValueSpec_Doc, edge.TypeSpec_Doc, edge.ImportSpec_Doc,
		edge.GenDecl_Doc, edge.CommentGroup_List:
		return true
	}
	return false
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
	fmt.Fprintf(&b, "\tprev %s\n", line(nt(r.prevToken)))
	cursorstr := "<ROOT>"
	cNode := r.prevCursor.Node()
	if cNode != nil {
		cursorstr = fmt.Sprintf("%T %s", cNode, line(cNode))
	}
	fmt.Fprintf(&b, "\tprev Cursor %s\n", cursorstr)
	fmt.Fprintf(&b, "\tnext %s\n", line(nt(r.nextToken)))
	fmt.Fprintf(&b, "\tnext Cursor %T %s\n", r.nextCursor.Node(), line(r.nextCursor.Node()))
	return b.String()
}

func line(n ast.Node) string {
	begin := fset.Position(n.Pos())
	end := fset.Position(n.End())
	return fmt.Sprintf("./%s:%v:%v:%v:%v", begin.Filename, begin.Line, begin.Column, end.Line, end.Column)
}

func sortComments(comms []*ast.CommentGroup) {
	slices.SortFunc(comms, func(a, b *ast.CommentGroup) int {
		return cmp.Compare(a.Pos(), b.Pos())
	})
}

func parseDir(dir string) ([]*ast.File, error) {
	ctxt := build.Default
	pkginfo, err := ctxt.ImportDir(dir, 0)
	if _, nogo := err.(*build.NoGoError); err != nil && !nogo {
		return nil, err
	}
	filenames := append(pkginfo.GoFiles, pkginfo.CgoFiles...)
	return parseFiles(dir, filenames)
}

func parseFiles(dir string, filenames []string) ([]*ast.File, error) {
	files := make([]*ast.File, len(filenames))
	errors := make([]error, len(filenames))

	var wg sync.WaitGroup
	for i, filename := range filenames {
		wg.Add(1)
		go func(i int, filepath string) {
			defer wg.Done()
			files[i], errors[i] = parse(filepath, nil)
		}(i, filepath.Join(dir, filename))
		if true {
			wg.Wait()
		}
	}
	wg.Wait()

	// if there are errors, return the first one for deterministic results
	for _, err := range errors {
		if err != nil {
			return nil, err
		}
	}

	return files, nil
}

// parse may be called concurrently
func parse(filename string, src any) (*ast.File, error) {
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments|parser.SkipObjectResolution) // ok to access fset concurrently
	//ast.Print(fset, file)
	return file, err
}
