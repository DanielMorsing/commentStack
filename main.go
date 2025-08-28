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
	"reflect"
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
		begin, end, ok := findComment(cmt, tok(n.Lbrack), nod(n.Len), nod(n.Elt))
		if ok {
			return begin, end
		}
	case *ast.AssignStmt:
		begin, end, ok := findComment(cmt, list(n.Lhs), tok(n.TokPos), list(n.Rhs))
		if ok {
			return begin, end
		}
		panic("did not find comment in assign stmt")
	case *ast.BasicLit:
		panic("comments cannot occur in literals")
	case *ast.BinaryExpr:
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.OpPos), nod(n.Y))
		if ok {
			return begin, end
		}
	case *ast.BlockStmt:
		begin, end, ok := findComment(cmt, tok(n.Lbrace), list(n.List), tok(n.Rbrace))
		if ok {
			return begin, end
		}
		panic("did not find stmt")
	case *ast.BranchStmt:
		return n.TokPos, n.Label.Pos()
	case *ast.CallExpr:
		begin, end, ok := findComment(cmt, nod(n.Fun), tok(n.Lparen), list(n.Args), tok(n.Ellipsis), tok(n.Rparen))
		if ok {
			return begin, end
		}

	case *ast.CaseClause:
		return caseClause(n, cmt, cur)
	case *ast.CommClause:
		return caseClause(n, cmt, cur)
	case *ast.Comment:
		panic("found comment")
	case *ast.CommentGroup:
		panic("found comment")

	case *ast.ChanType:
		begin, end, ok := findComment(cmt, tok(n.Begin), tok(n.Arrow), nod(n.Value))
		if ok {
			return begin, end
		}

	case *ast.CompositeLit:
		begin, end, ok := findComment(cmt, nod(n.Type), tok(n.Lbrace), list(n.Elts), tok(n.Rbrace))
		if ok {
			return begin, end
		}
	case *ast.DeferStmt:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		begin, end, ok := findComment(cmt, tok(n.Defer), nod(n.Call))
		if ok {
			return begin, end
		}
	case *ast.Ellipsis:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		begin, end, ok := findComment(cmt, tok(n.Ellipsis), nod(n.Elt))
		if ok {
			return begin, end
		}
	case *ast.Field:
		begin, end, ok := findComment(cmt, list(n.Names), nod(n.Type), nod(n.Tag))
		if ok {
			return begin, end
		}
		// TODO(dmo): figure out line comments
		panic("unhandled line comment")
	case *ast.FieldList:
		begin, end, ok := findComment(cmt, tok(n.Opening), list(n.List), tok(n.Closing))
		if ok {
			return begin, end
		}
	case *ast.File:
		begin, end, ok := findComment(cmt, tok(n.Package), nod(n.Name), list(n.Decls), tok(n.FileEnd))
		if ok {
			return begin, end
		}
		panic("did not find stmt")
	case *ast.ForStmt:
		begin, end, ok := findComment(cmt, tok(n.For), nod(n.Init), nod(n.Cond), nod(n.Post), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.FuncDecl:
		// the func token lives in the n.Type parameter, so inline all its fields into
		// this one
		begin, end, ok := findComment(cmt, tok(n.Type.Func), nod(n.Recv), nod(n.Name), nod(n.Type.Params), nod(n.Type.Results), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.FuncLit:
		// only one spot the comment can be in
		begin, end, ok := findComment(cmt, nod(n.Type), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.FuncType:
		begin, end, ok := findComment(cmt, tok(n.Func), nod(n.TypeParams), nod(n.Params), nod(n.Results))
		if ok {
			return begin, end
		}
	case *ast.GenDecl:
		begin, end, ok := findComment(cmt, tok(n.TokPos), tok(n.Lparen), list(n.Specs), tok(n.Rparen))
		if ok {
			return begin, end
		}
	case *ast.GoStmt:
		// only one spot the comment can be in
		begin, end, ok := findComment(cmt, tok(n.Go), nod(n.Call))
		if ok {
			return begin, end
		}
	case *ast.IfStmt:
		begin, end, ok := findComment(cmt, tok(n.If), nod(n.Init), nod(n.Cond), nod(n.Body), nod(n.Else))
		if ok {
			return begin, end
		}
	case *ast.ImportSpec:
		panic("what to do with endpos?")
	case *ast.IncDecStmt:
		// only one spot the comment can be in
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.TokPos))
		if ok {
			return begin, end
		}
	case *ast.IndexExpr:
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.Lbrack), nod(n.Index), tok(n.Rbrack))
		if ok {
			return begin, end
		}

	case *ast.IndexListExpr:
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.Lbrack), list(n.Indices), tok(n.Rbrack))
		if ok {
			return begin, end
		}
	case *ast.InterfaceType:
		// only one spot
		begin, end, ok := findComment(cmt, tok(n.Interface), nod(n.Methods))
		if ok {
			return begin, end
		}
	case *ast.KeyValueExpr:
		begin, end, ok := findComment(cmt, nod(n.Key), tok(n.Colon), nod(n.Value))
		if ok {
			return begin, end
		}
	case *ast.LabeledStmt:
		begin, end, ok := findComment(cmt, nod(n.Label), tok(n.Colon), nod(n.Stmt))
		if ok {
			return begin, end
		}
	case *ast.MapType:
		// Another type where we don't have perfect position info
		// there can be a comment between the brackets
		begin, end, ok := findComment(cmt, tok(n.Map), nod(n.Key), nod(n.Value))
		if ok {
			return begin, end
		}
	case *ast.ParenExpr:
		begin, end, ok := findComment(cmt, tok(n.Lparen), nod(n.X), tok(n.Rparen))
		if ok {
			return begin, end
		}
	case *ast.RangeStmt:
		begin, end, ok := findComment(cmt, tok(n.For), nod(n.Key), nod(n.Value), tok(n.TokPos), tok(n.Range), nod(n.X), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.ReturnStmt:
		begin, end, ok := findComment(cmt, tok(n.Return), list(n.Results))
		if ok {
			return begin, end
		}
	case *ast.SelectStmt:
		begin, end, ok := findComment(cmt, tok(n.Select), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.SelectorExpr:
		// do not have complete info here
		begin, end, ok := findComment(cmt, nod(n.X), nod(n.Sel))
		if ok {
			return begin, end
		}
	case *ast.SendStmt:
		begin, end, ok := findComment(cmt, nod(n.Chan), tok(n.Arrow), nod(n.Value))
		if ok {
			return begin, end
		}

	case *ast.SliceExpr:
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.Lbrack), nod(n.Low), nod(n.High), nod(n.Max), tok(n.Rbrack))
		if ok {
			return begin, end
		}
	case *ast.StarExpr:
		// only one spot in this expression we can be
		begin, end, ok := findComment(cmt, tok(n.Star), nod(n.X))
		if ok {
			return begin, end
		}
	case *ast.StructType:
		// only one spot in this grammar we can be
		begin, end, ok := findComment(cmt, tok(n.Struct), nod(n.Fields))
		if ok {
			return begin, end
		}
	case *ast.SwitchStmt:
		begin, end, ok := findComment(cmt, tok(n.Switch), nod(n.Init), nod(n.Tag), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.TypeAssertExpr:
		// incomplete position information, where is the dot?
		begin, end, ok := findComment(cmt, nod(n.X), tok(n.Lparen), nod(n.Type), tok(n.Rparen))
		if ok {
			return begin, end
		}
	case *ast.TypeSpec:
		begin, end, ok := findComment(cmt, nod(n.Name), nod(n.TypeParams), tok(n.Assign), nod(n.Type))
		if ok {
			return begin, end
		}
	case *ast.TypeSwitchStmt:
		begin, end, ok := findComment(cmt, tok(n.Switch), nod(n.Init), nod(n.Assign), nod(n.Body))
		if ok {
			return begin, end
		}
	case *ast.UnaryExpr:
		// only one spot in this grammar we can be
		begin, end, ok := findComment(cmt, tok(n.OpPos), nod(n.X))
		if ok {
			return begin, end
		}
	}
	log.Panicf("unhandled node %T, %s", cur.Node(), line(cmt))
	panic("unreachable")
}

func caseClause(node ast.Node, cmt *ast.CommentGroup, cur inspector.Cursor) (token.Pos, token.Pos) {
	var cas, colon token.Pos
	var elist []ast.Node
	var body []ast.Stmt
	switch n := node.(type) {
	case *ast.CommClause:
		elist = []ast.Node{n.Comm}
		cas = n.Case
		colon = n.Colon
		body = n.Body
	case *ast.CaseClause:
		elist = make([]ast.Node, len(n.List))
		for i, l := range n.List {
			elist[i] = l
		}
		cas = n.Case
		colon = n.Colon
		body = n.Body
	default:
		panic("unhandled case")
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

	begin, end, ok := findComment(cmt, tok(cas), elist, tok(colon), list(body), tok(endtok))
	if ok {
		return begin, end
	}
	panic("did not find comment")
}

func nod(node ast.Node) []ast.Node {
	// This is so that we can pass concrete values
	// through but they can still be ignored, e.g. Field.Tag
	if node == nil || reflect.ValueOf(node).IsZero() {
		return nil
	}
	return []ast.Node{node}
}

type nodeToken token.Pos

func (n nodeToken) Pos() token.Pos { return token.Pos(n) }
func (n nodeToken) End() token.Pos { return token.Pos(n + 1) }

func tok(p token.Pos) []ast.Node {
	if !p.IsValid() {
		return nil
	}
	return []ast.Node{nodeToken(p)}
}

func list[List ~[]N, N ast.Node](n List) []ast.Node {
	if n == nil {
		return nil
	}
	ret := make([]ast.Node, len(n))
	for i, x := range n {
		ret[i] = x
	}
	return ret
}

func findComment(cmt *ast.CommentGroup, list ...[]ast.Node) (token.Pos, token.Pos, bool) {
	var nodelist []ast.Node
	for _, n := range list {
		if n == nil {
			continue
		}
		for _, nn := range n {
			nodelist = append(nodelist, nn)
		}
	}
	for i := 1; i < len(nodelist); i++ {
		pn := nodelist[i-1]
		nn := nodelist[i]
		if pn.End() < cmt.Pos() && cmt.End() < nn.Pos() {
			return pn.End(), nn.Pos(), true
		}
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
	fmt.Fprintf(&b, "\tprev %s\n", line(nodeToken(r.prevToken)))
	cursorstr := "<ROOT>"
	cNode := r.prevCursor.Node()
	if cNode != nil {
		cursorstr = fmt.Sprintf("%T %s", cNode, line(cNode))
	}
	fmt.Fprintf(&b, "\tprev Cursor %s\n", cursorstr)
	fmt.Fprintf(&b, "\tnext %s\n", line(nodeToken(r.nextToken)))
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
