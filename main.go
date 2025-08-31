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
	"bytes"
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
	"unicode"

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
	// For this implementation we need the source file, but if this gets traction,
	// we should add a NextToken to the comment AST
	files, srcs, err := parseDir(pkgpath)
	if err != nil {
		log.Fatalf("cannot parse: %s", err)
	}
	inspect := inspector.New(files)
	root := inspect.Root()
	fileRanges := make([][]*commentRange, 0, len(files))

	for fileidx, file := range files {
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
			// Doc and Comment nodes occur either before a syntax
			// or after (to the side of it). Walk up to make sure we get the bounding AST
			if commentKind(outer.ParentEdge()) {
				outer = outer.Parent()
				if outer.Node() != file {
					outer = outer.Parent()
				}
			}
			if file == outer.Node() && cmt.End() < file.Package {
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
			ranges[i] = findLimits(cmt, srcs[fileidx], outer)
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

func findLimits(cmt *ast.CommentGroup, file []byte, outer inspector.Cursor) *commentRange {
	// We need to find the actual ending position of this commentgroup
	//
	// This is real ugly and should be abstracted away with a comment.NextTokenPos or something
	endComment := cmt.List[len(cmt.List)-1]
	text := endComment.Text
	end := []byte{'\n'}
	if text[1] == '*' {
		end = []byte{'*', '/'}
	}
	endpos := fset.Position(endComment.End())
	filebase := fset.File(endComment.End()).Base()
	idx := bytes.Index(file[endpos.Offset-len(end):], end)
	if idx == -1 {
		fmt.Println(string(file[endpos.Offset:]))
		log.Panicf("did not find end of comment %s", line(endComment))
	}
	var tokPos token.Pos
	for i := idx + endpos.Offset; i < len(file); i++ {
		b := file[i]
		if !unicode.IsSpace(rune(b)) {
			tokPos = token.Pos(filebase + i)
			break
		}
	}

	prevNode, nextNode := getTokens(outer, cmt)

	prevToken := prevNode.End()
	nextToken := nextNode.Pos()
	prevCursor := findOwningCursor(outer, prevNode)
	nextCursor := findOwningCursor(outer, nextNode)

	return &commentRange{
		comment:          cmt,
		prevToken:        prevToken,
		nextToken:        nextToken,
		prevCursor:       prevCursor,
		nextCursor:       nextCursor,
		TestCommentToken: tokPos,
	}
}

func findOwningCursor(outer inspector.Cursor, node ast.Node) inspector.Cursor {
	if _, ok := node.(nodeToken); ok {
		return outer
	}
	c, ok := outer.FindNode(node)
	if !ok {
		panic("bad findowner")
	}
	return c
}

// getTokens returns the specific nodes that bound the comment. Where astutil.PathEnclosingInterval
// and cursor.FindByPos finds the one Node that entirely encloses a position,
// gettoken returns the 2 nodes that contain the position. That can either be
// 2 different AST grammars or a token wrapped in a nodeToken.
//
// TODO(dmo): could this be done with reflect and carve-outs
func getTokens(cur inspector.Cursor, cmt *ast.CommentGroup) (ast.Node, ast.Node) {
	switch n := cur.Node().(type) {
	case *ast.ArrayType:
		// To figure out where specific comment is, need n.RBrack
		return findComment(cmt, tok(n.Lbrack), nod(n.Len), nod(n.Elt))
	case *ast.AssignStmt:
		return findComment(cmt, list(n.Lhs), tok(n.TokPos), list(n.Rhs))
	case *ast.BasicLit:
		panic("comments cannot occur in literals")
	case *ast.BinaryExpr:
		return findComment(cmt, nod(n.X), tok(n.OpPos), nod(n.Y))
	case *ast.BlockStmt:
		return findComment(cmt, tok(n.Lbrace), list(n.List), tok(n.Rbrace))
	case *ast.BranchStmt:
		// only one spot for comment
		return findComment(cmt, tok(n.TokPos), nod(n.Label))
	case *ast.CallExpr:
		return findComment(cmt, nod(n.Fun), tok(n.Lparen), list(n.Args), tok(n.Ellipsis), tok(n.Rparen))
	case *ast.CaseClause:
		return caseClause(cmt, n, cur)
	case *ast.CommClause:
		return caseClause(cmt, n, cur)
	case *ast.Comment:
		panic("found comment")
	case *ast.CommentGroup:
		panic("found comment")

	case *ast.ChanType:
		// cannot distinguish comment position here, no chan keyword position
		return findComment(cmt, tok(n.Begin), tok(n.Arrow), nod(n.Value))

	case *ast.CompositeLit:
		// cannot distinguish comment position, comma after elements not positioned
		return findComment(cmt, nod(n.Type), tok(n.Lbrace), list(n.Elts), tok(n.Rbrace))
	case *ast.DeferStmt:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		return findComment(cmt, tok(n.Defer), nod(n.Call))
	case *ast.Ellipsis:
		// TODO(dmo): can this just be removed, there's no other place for the comment to be
		return findComment(cmt, tok(n.Ellipsis), nod(n.Elt))
	case *ast.Field:
		// cannot distinguish comment position, comma between names not positioned
		return findComment(cmt, list(n.Names), nod(n.Type), nod(n.Tag))
	case *ast.FieldList:
		// cannot distinguish comment position, comma or semi after elements not positioned
		return findComment(cmt, tok(n.Opening), list(n.List), tok(n.Closing))
	case *ast.File:
		return findComment(cmt, tok(n.Package), nod(n.Name), list(n.Decls), tok(n.FileEnd))
	case *ast.ForStmt:
		// cannot distinguish comment position, semicolon between clauses not positioned
		return findComment(cmt, tok(n.For), nod(n.Init), nod(n.Cond), nod(n.Post), nod(n.Body))
	case *ast.FuncDecl:
		// the func token lives in the n.Type parameter, so inline all its fields into
		// this one
		return findComment(cmt, tok(n.Type.Func), nod(n.Recv), nod(n.Name), nod(n.Type.Params), nod(n.Type.Results), nod(n.Body))
	case *ast.FuncLit:
		// only one spot the comment can be in
		return findComment(cmt, nod(n.Type), nod(n.Body))
	case *ast.FuncType:
		return findComment(cmt, tok(n.Func), nod(n.TypeParams), nod(n.Params), nod(n.Results))
	case *ast.GenDecl:
		return findComment(cmt, tok(n.TokPos), tok(n.Lparen), list(n.Specs), tok(n.Rparen))
	case *ast.GoStmt:
		// only one spot the comment can be in
		return findComment(cmt, tok(n.Go), nod(n.Call))
	case *ast.IfStmt:
		return findComment(cmt, tok(n.If), nod(n.Init), nod(n.Cond), nod(n.Body), nod(n.Else))
	case *ast.ImportSpec:
		panic("what to do with endpos?")
	case *ast.IncDecStmt:
		// only one spot the comment can be in
		return findComment(cmt, nod(n.X), tok(n.TokPos))
	case *ast.IndexExpr:
		return findComment(cmt, nod(n.X), tok(n.Lbrack), nod(n.Index), tok(n.Rbrack))
	case *ast.IndexListExpr:
		// cannot distinguish comment position, comma after elements not positioned
		return findComment(cmt, nod(n.X), tok(n.Lbrack), list(n.Indices), tok(n.Rbrack))
	case *ast.InterfaceType:
		return findComment(cmt, tok(n.Interface), nod(n.Methods))
	case *ast.KeyValueExpr:
		return findComment(cmt, nod(n.Key), tok(n.Colon), nod(n.Value))
	case *ast.LabeledStmt:
		return findComment(cmt, nod(n.Label), tok(n.Colon), nod(n.Stmt))
	case *ast.MapType:
		// Another type where we don't have perfect position info
		// there can be a comment between the brackets
		return findComment(cmt, tok(n.Map), nod(n.Key), nod(n.Value))
	case *ast.ParenExpr:
		return findComment(cmt, tok(n.Lparen), nod(n.X), tok(n.Rparen))
	case *ast.RangeStmt:
		// cannot distinguish comment position, semicolon between clauses not positioned
		return findComment(cmt, tok(n.For), nod(n.Key), nod(n.Value), tok(n.TokPos), tok(n.Range), nod(n.X), nod(n.Body))
	case *ast.ReturnStmt:
		// cannot distinguish comment position, comma between results not positioned
		return findComment(cmt, tok(n.Return), list(n.Results))
	case *ast.SelectStmt:
		return findComment(cmt, tok(n.Select), nod(n.Body))
	case *ast.SelectorExpr:
		// cannot distinguish comment position, dot not positioned
		return findComment(cmt, nod(n.X), nod(n.Sel))
	case *ast.SendStmt:
		return findComment(cmt, nod(n.Chan), tok(n.Arrow), nod(n.Value))
	case *ast.SliceExpr:
		// cannot distinguish comment position, colons positioned
		return findComment(cmt, nod(n.X), tok(n.Lbrack), nod(n.Low), nod(n.High), nod(n.Max), tok(n.Rbrack))
	case *ast.StarExpr:
		// only one spot in this expression we can be
		return findComment(cmt, tok(n.Star), nod(n.X))
	case *ast.StructType:
		// only one spot in this grammar we can be
		return findComment(cmt, tok(n.Struct), nod(n.Fields))
	case *ast.SwitchStmt:
		return findComment(cmt, tok(n.Switch), nod(n.Init), nod(n.Tag), nod(n.Body))
	case *ast.TypeAssertExpr:
		// incomplete position information, where is the dot?
		return findComment(cmt, nod(n.X), tok(n.Lparen), nod(n.Type), tok(n.Rparen))
	case *ast.TypeSpec:
		return findComment(cmt, nod(n.Name), nod(n.TypeParams), tok(n.Assign), nod(n.Type))
	case *ast.TypeSwitchStmt:
		return findComment(cmt, tok(n.Switch), nod(n.Init), nod(n.Assign), nod(n.Body))
	case *ast.UnaryExpr:
		// only one spot in this grammar we can be
		return findComment(cmt, tok(n.OpPos), nod(n.X))
	}
	log.Panicf("unhandled node %T, %s", cur.Node(), line(cmt))
	panic("unreachable")
}

func caseClause(cmt *ast.CommentGroup, node ast.Node, cur inspector.Cursor) (ast.Node, ast.Node) {
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

	return findComment(cmt, tok(cas), elist, tok(colon), list(body), tok(endtok))
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

// TODO(dmo): care about token length?
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

func findComment(cmt *ast.CommentGroup, list ...[]ast.Node) (ast.Node, ast.Node) {
	var nodelist []ast.Node
	for _, n := range list {
		if n == nil {
			continue
		}
		nodelist = append(nodelist, n...)
	}
	for i := 1; i < len(nodelist); i++ {
		pn := nodelist[i-1]
		nn := nodelist[i]
		if pn.End() < cmt.Pos() && cmt.End() < nn.Pos() {
			return pn, nn
		}
	}
	log.Panicf("could not find comment in node %s", line(cmt))
	panic("panic")
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

	TestCommentToken token.Pos
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

	fmt.Fprintf(&b, "\tcommentPos %s\n", line(nodeToken(r.TestCommentToken)))
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
