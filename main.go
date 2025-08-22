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
	"go/types"
	"log"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"time"

	"github.com/DanielMorsing/commentStack/typeindex"
	"golang.org/x/tools/go/ast/edge"
	"golang.org/x/tools/go/ast/inspector"
)

var (
	fset      = token.NewFileSet()
	parseonly = flag.Bool("parseonly", false, "parse and dump http example (no typecheck)")
)

func main() {
	flag.Parse()
	pkgpath := "./testdata/src/a/smol"
	if *parseonly {
		pkgpath = "./testdata/src/a/http"
	}
	files, err := parseDir(pkgpath)
	if err != nil {
		log.Fatalf("cannot parse: %s", err)
	}
	inspect := inspector.New(files)
	root := inspect.Root()

	var check *types.Checker
	var tindex *typeindex.Index
	if !*parseonly {
		pkg := types.NewPackage("./testdata/src/a/smol/", "a")
		check = types.NewChecker(nil, fset, pkg, nil)
		err = check.Files(files)
		if err != nil {
			log.Fatal("typecheck", err.Error())
		}
		tindex = typeindex.New(inspect, pkg, check.Info)
		_ = tindex
	}

	for fileCur := range root.Preorder((*ast.File)(nil)) {
		file := fileCur.Node().(*ast.File)

		fmt.Println(fset.Position(file.Name.Pos()).Filename)
		sortComments(file.Comments)
		ranges := make([]*commentRange, len(file.Comments))
		// Each comment will be bounded by 2 cursors.
		// One cursor for the node that last produced a token before the comment
		// and one cursor for the node that will introduce a token after the comment
		for i, cmt := range file.Comments {
			outer, _ := fileCur.FindByPos(cmt.Pos(), cmt.Pos())
			prev := outer
			next := outer
			for cur := range outer.Children() {
				if cur.Node().End() < cmt.Pos() {
					prev = cur
					continue
				}
				next = cur
				break
			}
			r := &commentRange{
				comment: cmt,
				prev:    prev,
				next:    next,
			}
			r.adjust()
			ranges[i] = r
			if *parseonly {
				fmt.Println(r.String())
			}
		}
		if *parseonly {
			continue
		}
		anchors := make(map[int32]*commentRange)
		for _, r := range ranges {
			anchor := r.prev
			if r.anchor == AnchorRight {
				anchor = r.next
			}
			anchors[anchor.Index()] = r
		}
		// toy example of finding go:fix inline directives
		for declCur := range fileCur.Preorder((*ast.FuncDecl)(nil)) {
			idx := declCur.Index()
			cmt, ok := anchors[idx]
			if !ok {
				continue
			}
			if cmt.comment.List[0].Text != "//go:fix inline" {
				continue
			}
			retCur := slices.Collect(declCur.Preorder((*ast.ReturnStmt)(nil)))
			if len(retCur) != 1 {
				panic("multiple returns in inline")
			}
			ret := retCur[0].Node().(*ast.ReturnStmt)
			if len(ret.Results) != 1 {
				panic("multiple results to return")
			}
			_ = ret.Results[0].(*ast.CallExpr)
			decl := declCur.Node().(*ast.FuncDecl)
			obj := check.ObjectOf(decl.Name)
			_ = obj
		}
	}
}

type Anchor int

const (
	AnchorNone Anchor = iota
	AnchorLeft
	AnchorRight
)

type commentRange struct {
	comment *ast.CommentGroup
	prev    inspector.Cursor
	next    inspector.Cursor
	anchor  Anchor
}

func (r *commentRange) String() string {
	var b strings.Builder
	fmt.Fprintln(&b, line(r.comment))
	prev := r.prev.Node()
	anchor := ""
	if r.anchor == AnchorLeft {
		anchor = " (ANCHOR)"
	}
	fmt.Fprintf(&b, "\tprev %T %s%s\n", prev, line(prev), anchor)

	anchor = ""
	if r.anchor == AnchorRight {
		anchor = " (ANCHOR)"
	}
	next := r.next.Node()
	fmt.Fprintf(&b, "\tnext %T %s%s\n", next, line(next), anchor)
	return b.String()
}

func (r *commentRange) adjust() {
	left := r.prev
	right := r.next
	cmt := r.comment
	parent := left.Parent()

	cLine := fset.Position(cmt.Pos()).Line
	lLine := fset.Position(left.Node().Pos()).Line
	if cLine == lLine {
		r.anchor = AnchorLeft
	} else {
		r.anchor = AnchorRight
	}

	// we ran off the edge of the node without finding any
	// sub-nodes. This happens when the comment occurs
	// inside empty postfix constructions (like function calls)
	call, ok := right.Node().(*ast.CallExpr)
	if left.Parent() == right && ok {
		// we have a comment inside the call parens
		// we represent this as 2 cursors to the call
		// expression with the anchoring based whether
		// the comment is on the same line as the opening
		// or closing paren
		lparen := call.Lparen
		if lparen < cmt.Pos() {
			r.prev = right
			r.next = right
			line := fset.Position(lparen).Line
			if line == cLine {
				r.anchor = AnchorLeft
			} else {
				r.anchor = AnchorRight
			}
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
			r.next = parent
		} else {
			r.prev = parent
		}
		return
	}
	// Keyword "func" between doc and the rest of the decl
	if lp == edge.FuncDecl_Doc {
		r.prev = parent
		return
	}

	if tokenless[lp] {
		return
	}
	// The AST doesn't currently let us inspect certain grammars.
	// For example CaseClause:
	//
	// case a, /* comment */ b:
	//
	// There is no way from looking at the ranges embedded
	// in the AST whether the comma occurs before or after the
	// comment. In this case, we keep the siblings as the bounding
	// cursors and rely on anchoring to disambiguate which cursor
	// the comment belongs to.
	//
	// Luckily, this is what gofmt has done since forever.
	// gofmt'ed code will always have the comma after the comment
	// if the 2 edges occur on the same line
	switch lp {
	case edge.CaseClause_List, edge.FieldList_List:
		return
	case edge.SelectorExpr_X:
		return
	}
	panic(fmt.Sprintf("unhandled adjust %T[%s,%s]", parent.Node(), lp, rp))
}

var tokenless = map[edge.Kind]bool{
	edge.File_Decls:     true,
	edge.IfStmt_Cond:    true,
	edge.BlockStmt_List: true,
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
	begin := time.Now()
	slices.SortFunc(comms, func(a, b *ast.CommentGroup) int {
		return cmp.Compare(a.Pos(), b.Pos())
	})
	fmt.Println(time.Since(begin))
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
