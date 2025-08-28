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
			if commentKind(outer.ParentEdge()) {
				outer = outer.Parent().Parent()
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

// getTokens returns the specific tokens that bound the comment. Where astutil.PathEnclosingInterval
// and cursor.FindByPos finds the one Node that entirely encloses a position (and hence comment),
// gettoken can return tokens that belong to different AST nodes.
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
		begin, end, ok := commentBetweenList(nt(n.Lbrace), n.List, nt(n.Rbrace), cmt)
		if ok {
			return begin, end
		}
		panic("did not find stmt")
	case *ast.BranchStmt:
		return n.TokPos, n.Label.Pos()
	case *ast.CallExpr:
		begin, end, ok := commentBetween(n.Fun, nt(n.Lparen), cmt)
		if ok {
			return begin, end
		}
		endtok := n.Rparen
		if n.Ellipsis != token.NoPos {
			endtok = n.Ellipsis
		}
		begin, end, ok = commentBetweenList(nt(n.Lparen), n.Args, nt(endtok), cmt)
		if ok {
			return begin, end
		}
		return n.Ellipsis, n.Rparen
	case *ast.CaseClause:
		return caseClause(n, cmt, cur)
	case *ast.CommClause:
		return caseClause(n, cmt, cur)
	case *ast.Comment:
		panic("found comment")
	case *ast.CommentGroup:
		panic("found comment")

	case *ast.ChanType:
		begintok := n.Begin
		if n.Arrow != token.NoPos {
			begintok = n.Arrow
			begin, end, ok := commentBetween(nt(n.Begin), nt(n.Arrow), cmt)
			if ok {
				return begin, end
			}
		}
		begin, end, ok := commentBetween(nt(begintok), n.Value, cmt)
		if ok {
			return begin, end
		}

	case *ast.CompositeLit:
		begin, end, ok := commentBetween(n.Type, nt(n.Lbrace), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetweenList(nt(n.Lbrace), n.Elts, nt(n.Rbrace), cmt)
		if ok {
			return begin, end
		}
	case *ast.DeferStmt:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		begin, end, ok := commentBetween(nt(n.Defer), n.Call, cmt)
		if ok {
			return begin, end
		}
	case *ast.Ellipsis:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		begin, end, ok := commentBetween(nt(n.Ellipsis), n.Elt, cmt)
		if ok {
			return begin, end
		}
	case *ast.Field:
		begin, end, ok := commentBetweenList(nt(token.NoPos), n.Names, n.Type, cmt)
		if ok {
			return begin, end
		}
		if n.Tag != nil {
			begin, end, ok := commentBetween(n.Type, n.Tag, cmt)
			if ok {
				return begin, end
			}
		}
		// TODO(dmo): figure out line comments
		panic("unhandled line comment")
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
	case *ast.ForStmt:
		var list []ast.Node
		if n.Init != nil {
			list = append(list, n.Init)
		}
		if n.Cond != nil {
			list = append(list, n.Cond)
		}
		if n.Post != nil {
			list = append(list, n.Post)
		}
		begin, end, ok := commentBetweenList(nt(n.For), list, n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.FuncDecl:
		// n.Type covers the entire signature, can we ever get here?
		panic("questions")
	case *ast.FuncLit:
		// only one spot the comment can be in
		begin, end, ok := commentBetween(n.Type, n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.FuncType:
		var list []ast.Node
		if n.TypeParams != nil {
			list = append(list, n.TypeParams)
		}
		list = append(list, n.Params)
		if n.Results != nil {
			list = append(list, n.Results)
		}
		begin, end, ok := commentBetweenList(nt(n.Func), list, nt(token.NoPos), cmt)
		if ok {
			return begin, end
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
	case *ast.GoStmt:
		// only one spot the comment can be in
		begin, end, ok := commentBetween(nt(n.Go), n.Call, cmt)
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
	case *ast.ImportSpec:
		panic("what to do with endpos?")
	case *ast.IncDecStmt:
		// only one spot the comment can be in
		begin, end, ok := commentBetween(n.X, nt(n.TokPos), cmt)
		if ok {
			return begin, end
		}
	case *ast.IndexExpr:
		begin, end, ok := commentBetween(n.X, nt(n.Lbrack), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(nt(n.Lbrack), n.Index, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(n.Index, nt(n.Rbrack), cmt)
		if ok {
			return begin, end
		}
	case *ast.IndexListExpr:
		begin, end, ok := commentBetween(n.X, nt(n.Lbrack), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetweenList(nt(n.Lbrack), n.Indices, nt(n.Rbrack), cmt)
		if ok {
			return begin, end
		}
	case *ast.InterfaceType:
		// only one spot
		begin, end, ok := commentBetween(nt(n.Interface), n.Methods, cmt)
		if ok {
			return begin, end
		}
	case *ast.KeyValueExpr:
		begin, end, ok := commentBetween(n.Key, nt(n.Colon), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(nt(n.Colon), n.Value, cmt)
		if ok {
			return begin, end
		}
	case *ast.LabeledStmt:
		begin, end, ok := commentBetween(n.Label, nt(n.Colon), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(nt(n.Colon), n.Stmt, cmt)
		if ok {
			return begin, end
		}
	case *ast.MapType:
		// Another type where we don't have perfect position info
		begin, end, ok := commentBetween(nt(n.Map), n.Key, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(n.Key, n.Value, cmt)
		if ok {
			return begin, end
		}
	case *ast.ParenExpr:
		begin, end, ok := commentBetween(nt(n.Lparen), n.X, cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(n.X, nt(n.Rparen), cmt)
		if ok {
			return begin, end
		}
	case *ast.RangeStmt:
		var list []ast.Node
		if n.Key != nil {
			list = append(list, n.Key)
		}
		if n.Value != nil {
			list = append(list, n.Value)
		}
		if n.TokPos.IsValid() {
			list = append(list, nt(n.TokPos))
		}
		list = append(list, nt(n.Range))
		list = append(list, n.X)
		begin, end, ok := commentBetweenList(nt(n.For), list, n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.ReturnStmt:
		begin, end, ok := commentBetweenList(nt(n.Return), n.Results, nt(token.NoPos), cmt)
		if ok {
			return begin, end
		}
	case *ast.SelectStmt:
		begin, end, ok := commentBetween(nt(n.Select), n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.SelectorExpr:
		// do not have complete info here
		begin, end, ok := commentBetween(n.X, n.Sel, cmt)
		if ok {
			return begin, end
		}
	case *ast.SendStmt:
		begin, end, ok := commentBetween(n.Chan, nt(n.Arrow), cmt)
		if ok {
			return begin, end
		}
		begin, end, ok = commentBetween(nt(n.Arrow), n.Value, cmt)
		if ok {
			return begin, end
		}
	case *ast.SliceExpr:
		begin, end, ok := commentBetween(n.X, nt(n.Lbrack), cmt)
		if ok {
			return begin, end
		}
		var list []ast.Node
		if n.Low != nil {
			list = append(list, n.Low)
		}
		if n.High != nil {
			list = append(list, n.High)
		}
		if n.Max != nil {
			list = append(list, n.Max)
		}
		begin, end, ok = commentBetweenList(nt(n.Lbrack), list, nt(n.Rbrack), cmt)
		if ok {
			return begin, end
		}
	case *ast.StarExpr:
		// only one spot in this expression we can be
		begin, end, ok := commentBetween(nt(n.Star), n.X, cmt)
		if ok {
			return begin, end
		}
	case *ast.StructType:
		// only one spot in this grammar we can be
		begin, end, ok := commentBetween(nt(n.Struct), n.Fields, cmt)
		if ok {
			return begin, end
		}
	case *ast.SwitchStmt:
		var list []ast.Node
		if n.Init != nil {
			list = append(list, n.Init)
		}
		if n.Tag != nil {
			list = append(list, n.Tag)
		}
		begin, end, ok := commentBetweenList(nt(n.Switch), list, n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.TypeAssertExpr:
		// incomplete position information, where is the dot?
		begin, end, ok := commentBetween(n.X, nt(n.Lparen), cmt)
		if ok {
			return begin, end
		}
		startTok := ast.Node(nt(n.Lparen))
		if n.Type != nil {
			startTok = n.Type
			begin, end, ok := commentBetween(nt(n.Lparen), n.Type, cmt)
			if ok {
				return begin, end
			}
		}
		begin, end, ok = commentBetween(startTok, nt(n.Rparen), cmt)
		if ok {
			return begin, end
		}
	case *ast.TypeSpec:
		var list []ast.Node
		if n.TypeParams != nil {
			list = append(list, n.TypeParams)
		}
		if n.Assign.IsValid() {
			list = append(list, nt(n.Assign))
		}
		begin, end, ok := commentBetweenList(n.Name, list, n.Type, cmt)
		if ok {
			return begin, end
		}
	case *ast.TypeSwitchStmt:
		var list []ast.Node
		if n.Init != nil {
			list = append(list, n.Init)
		}
		begin, end, ok := commentBetweenList(nt(n.Switch), list, n.Body, cmt)
		if ok {
			return begin, end
		}
	case *ast.UnaryExpr:
		// only one spot in this grammar we can be
		begin, end, ok := commentBetween(nt(n.OpPos), n.X, cmt)
		if ok {
			return begin, end
		}
	}
	log.Panicf("unhandled node %T, %s", cur.Node(), line(cmt))
	panic("unreachable")
}

func caseClause(node ast.Node, cmt *ast.CommentGroup, cur inspector.Cursor) (token.Pos, token.Pos) {
	var cas, colon token.Pos
	var list []ast.Node
	var body []ast.Stmt
	switch n := node.(type) {
	case *ast.CommClause:
		list = []ast.Node{n.Comm}
		cas = n.Case
		colon = n.Colon
		body = n.Body
	case *ast.CaseClause:
		list = make([]ast.Node, len(n.List))
		for i, l := range n.List {
			list[i] = l
		}
		cas = n.Case
		colon = n.Colon
		body = n.Body
	default:
		panic("unhandled case")
	}
	begin, end, ok := commentBetweenList(nt(cas), list, nt(colon), cmt)
	if ok {
		return begin, end
	}
	// case clauses are terminated by the next caseclause or the end
	// of the enclosing switch body, we need the parent for either
	switchBody := cur.Parent()
	endtok := token.NoPos
	lastchild, ok := switchBody.LastChild()
	if !ok {
		panic("???")
	}
	if lastchild == cur {
		endtok = switchBody.Node().End()
	} else {
		// get the next caseclause in this switch, it is our end token
		k, i := cur.ParentEdge()
		endtok = switchBody.ChildAt(k, i+1).Node().Pos()
	}
	begin, end, ok = commentBetweenList(nt(colon), body, nt(endtok), cmt)
	if !ok {
		panic("did not find comment")
	}
	return begin, end
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
	switch ek {
	case edge.Field_Doc, edge.File_Doc, edge.FuncDecl_Doc,
		edge.ValueSpec_Doc, edge.TypeSpec_Doc, edge.ImportSpec_Doc,
		edge.GenDecl_Doc, edge.CommentGroup_List, edge.Field_Comment,
		edge.ImportSpec_Comment, edge.ValueSpec_Comment, edge.TypeSpec_Comment:
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
