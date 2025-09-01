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
	"os"
	"path/filepath"
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
		for fidx, f := range files {
			// remove free floating comments
			f.Comments = nil
			cfg := printer.Config{
				Mode:        0,
				Tabwidth:    8,
				Indent:      0,
				Transitions: newPassthrough(root, fileRanges[fidx]),
			}
			cfg.Fprint(os.Stdout, fset, f)
		}
	}
}

func (r *commentRange) findCursors(cur inspector.Cursor) {
	r.prevCursor, _ = cur.FindByPos(r.prevToken, r.prevToken)
	r.nextCursor, _ = cur.FindByPos(r.nextToken, r.nextToken)
}

type passthrough struct {
	cursor inspector.Cursor
	begin  map[inspector.Cursor][]*commentRange
	end    map[inspector.Cursor][]*commentRange
}

func newPassthrough(cur inspector.Cursor, rngs []*commentRange) *passthrough {
	begin := make(map[inspector.Cursor][]*commentRange)
	end := make(map[inspector.Cursor][]*commentRange)
	for _, r := range rngs {
		begin[r.prevCursor] = append(begin[r.prevCursor], r)
		end[r.nextCursor] = append(end[r.nextCursor], r)
	}
	return &passthrough{
		cursor: cur,
		begin:  begin,
		end:    end,
	}
}

func (p *passthrough) Step(before ast.Node, after ast.Node) *ast.CommentGroup {
	// This is super inefficient, but good enough for proof of concept
	beginCur, ok := p.cursor.FindNode(before)
	if !ok && before != nil {
		panic("ARGH")
	}
	endCur, ok := p.cursor.FindNode(after)
	if !ok {
		panic("ARGH")
	}
	beginlist := p.begin[beginCur]
	for i, r := range beginlist {
		if r.nextCursor == endCur {
			return r.comment
		}
	}
	return nil
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
