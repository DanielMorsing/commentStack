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
			outer, _ := root.FindByPos(cmt.Pos(), cmt.Pos())
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
	rang := &commentRange{
		comment: cmt,
	}
	rang.prevToken, rang.nextToken = getTokens(outer, cmt)
	return rang
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
		begin, end, ok := commentBetweenList(token.NoPos, n.Lhs, n.TokPos, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetweenList(n.TokPos, n.Rhs, token.NoPos, cmt)
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
		begin, end, ok := commentBetweenList(n.Lbrace, n.List, n.Rbrace, cmt)
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
		begin, end, ok := commentBetweenList(n.Lparen, n.Args, endtok, cmt)
		if ok {
			return begin, end
		}
		return n.Ellipsis, n.Rparen
	case *ast.CaseClause:
		begin, end, ok := commentBetweenList(n.Case, n.List, n.Colon, cmt)
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
		begin, end, ok = commentBetweenList(n.Colon, n.List, endtok, cmt)
		if !ok {
			panic("did not find comment")
		}
		return begin, end

	case *ast.File:
		if cmt.End() < n.Name.Pos() {
			return n.Package, n.Name.Pos()
		}
		begin, end, ok := commentBetweenList(n.Name.Pos(), n.Decls, n.FileEnd, cmt)
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
	case *ast.IfStmt:

	}
	log.Panicf("unhandled node %T, %s", cur.Node(), line(cmt))
	panic("unreachable")
}

func commentBetweenList[L ~[]N, N ast.Node](begin token.Pos, nodeList L, end token.Pos, cmt *ast.CommentGroup) (token.Pos, token.Pos, bool) {
	if begin.IsValid() && end.IsValid() && len(nodeList) == 0 {
		if cmt.End() < begin || end < cmt.Pos() {
			panic("bad call")
		}
		return begin, end, true
	}
	if begin.IsValid() && len(nodeList) > 0 && cmt.End() < nodeList[0].Pos() {
		return begin, nodeList[0].Pos(), true
	}
	for i := 1; i < len(nodeList); i++ {
		pn := nodeList[i-1]
		nn := nodeList[i]
		if pn.End() < cmt.Pos() && cmt.End() < nn.Pos() {
			return pn.End(), nn.Pos(), true
		}
	}
	if cmt.End() < end {
		return nodeList[len(nodeList)-1].End(), end, true
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

type LinePosition int

const (
	LinePrevious LinePosition = iota
	LineBetween
	LineNext
)

type commentRange struct {
	comment    *ast.CommentGroup
	prevCursor inspector.Cursor
	nextCursor inspector.Cursor
	prevToken  token.Pos
	nextToken  token.Pos
	position   LinePosition
}

func (r *commentRange) String() string {
	var b strings.Builder
	fmt.Fprintln(&b, line(r.comment))
	prev := r.prevCursor.Node()
	posStr := ""
	if r.position == LinePrevious {
		posStr = " <Comment>"
	}
	prevline := "<ROOT>"
	if prev != nil {
		prevline = line(prev)
	}
	fmt.Fprintf(&b, "\tprev %T %s%s\n", prev, prevline, posStr)
	if r.position == LineBetween {
		fmt.Fprintln(&b, "\t<Comment>")
	}

	posStr = ""
	if r.position == LineNext {
		posStr = "<Comment> "
	}
	next := r.nextCursor.Node()
	fmt.Fprintf(&b, "\t%snext %T %s", posStr, next, line(next))
	return b.String()
}

func (r *commentRange) adjust() {
	left := r.prevCursor
	right := r.nextCursor
	cmt := r.comment
	parent := left.Parent()

	// we ran off the edge of the node without finding any
	// sub-nodes. This happens when the comment occurs
	// inside empty postfix constructions (like function calls)
	call, ok := right.Node().(*ast.CallExpr)
	if left.Parent() == right && ok {
		// we have a comment inside the call parens
		lparen := call.Lparen
		if lparen < cmt.Pos() {
			r.prevCursor = right
			r.nextCursor = right
		}
	}
	if parent != right.Parent() {
		return
	}

	lp, _ := left.ParentEdge()
	rp, _ := right.ParentEdge()

	// binary constructions with tokens between them
	tok := token.NoPos
	switch lp {
	case edge.AssignStmt_Lhs:
		astmt := parent.Node().(*ast.AssignStmt)
		tok = astmt.TokPos
	case edge.KeyValueExpr_Key:
		kv := parent.Node().(*ast.KeyValueExpr)
		tok = kv.Colon
	case edge.BinaryExpr_X:
		be := parent.Node().(*ast.BinaryExpr)
		tok = be.OpPos
	default:
	}
	if tok != token.NoPos {
		if r.comment.Pos() < tok {
			r.nextCursor = parent
		} else {
			r.prevCursor = parent
		}
		return
	}
	// Keyword "func" between doc and the rest of the decl
	if lp == edge.FuncDecl_Doc {
		r.prevCursor = parent
		return
	}

	if tokenless[lp] {
		return
	}
	// The AST doesn't currently let us unambiguously locate a comment
	// within certain grammars.
	// For example CaseClause:
	//
	// case a, /* comment */ b:
	//
	// There is no way from looking at the ranges embedded
	// in the AST whether the comma occurs before or after the
	// comment.
	//
	// Luckily, this is what gofmt and go/printer has done since forever.
	// It uses linebreak heuristics to figure out when to emit the comment
	// that we can replicate here.
	//
	// TODO: This could be fixed by adding a special expression
	// node in go/ast whos ranges would cover syntactic elements like
	// the commas in caseclauses. This would obviously be a special mode
	// and would require a proposal
	switch lp {
	case edge.CaseClause_List, edge.FieldList_List:
		// the comma comes after the comment, unless there is a newline
		// between the 2 cases.
		pLine := fset.Position(r.prevCursor.Node().End()).Line
		nLine := fset.Position(r.nextCursor.Node().Pos()).Line
		if pLine == nLine {
			r.nextCursor = r.nextCursor.Parent()
		} else {
			r.prevCursor = r.prevCursor.Parent()
		}
		return
	case edge.SelectorExpr_X:
		// the dot always comes before the comment
		r.prevCursor = r.prevCursor.Parent()
		return
	}
	panic(fmt.Sprintf("unhandled adjust %T[%s,%s] %s", parent.Node(), lp, rp, line(cmt)))
}

func (r *commentRange) adjustLine() {
	prev := r.prevCursor.Node()
	next := r.nextCursor.Node()

	cLine := fset.Position(r.comment.Pos()).Line
	nLine := fset.Position(next.Pos()).Line
	if prev == nil {
		r.position = LineBetween
		if cLine == nLine {
			r.position = LineNext
		}
		return
	}
	pLine := fset.Position(prev.Pos()).Line
	if pLine == nLine || (cLine != pLine && cLine != nLine) {
		r.position = LineBetween
	} else if cLine == pLine {
		r.position = LinePrevious
	} else if cLine == nLine {
		r.position = LineNext
	} else {
		panic("got the logic wrong")
	}
}

var tokenless = map[edge.Kind]bool{
	edge.File_Decls:      true,
	edge.IfStmt_Cond:     true,
	edge.BlockStmt_List:  true,
	edge.CaseClause_Body: true,
	edge.GenDecl_Specs:   true,
}

func lineRange(fset *token.FileSet, pos token.Pos) (begin, end token.Pos) {
	tokfile := fset.File(pos)
	line := tokfile.Position(pos).Line
	begin = tokfile.LineStart(line)
	if tokfile.LineCount() < line+1 {
		end = token.Pos(tokfile.Size())
	} else {
		end = tokfile.LineStart(line + 1)
	}
	return begin, end

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
