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
	"iter"
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
		prevPos := file.FileStart
		prevToken := token.ILLEGAL
		for cmtIdx < len(file.Comments) {
			cmt := file.Comments[cmtIdx]
			pos, tok, lit := scan.Scan()
			if tok == token.SEMICOLON && lit == "\n" {
				continue
			}
			for cmt.End() <= pos {
				ranges = append(ranges, &commentRange{
					comment: cmt,

					prevPos: prevPos,
					prevTok: prevToken,

					nextPos: pos,
					nextTok: tok,
				})
				cmtIdx += 1
				if cmtIdx == len(file.Comments) {
					break
				}
				cmt = file.Comments[cmtIdx]
			}
			switch tok {
			case token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING:
				prevPos = pos + token.Pos(len(lit))
			default:
				prevPos = pos + token.Pos(len(tok.String()))
			}
			prevToken = tok
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
			for t := range tokenOrder(f) {
				fmt.Printf("%T %s %s\n", t.Node, line(t.Pos), t.Tok)
			}
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

// TODO(dmo): better name
type Token struct {
	Node ast.Node
	Pos  token.Pos
	Tok  token.Token
}

func tokenOrder(root ast.Node) iter.Seq[Token] {
	return func(yield func(Token) bool) {
		tokVisit(root, yield)
	}
}

// in the case where we have no position for a token
// but we know it exists, use this
const UnknownPos token.Pos = -1

func tokVisit(n ast.Node, visit func(Token) bool) bool {
	ok := true
	switch n := n.(type) {
	case *ast.ArrayType:
		// Don't have a token position for
		ok = tokenvisit1(visit, n, tok(n.Lbrack, token.LBRACK), tok(UnknownPos, token.RBRACK), nod(n.Len), nod(n.Elt), eor)
	case *ast.AssignStmt:
		ok = tokenvisit1(visit, n, list(n.Lhs, token.COMMA), tok(n.TokPos, n.Tok), list(n.Rhs, token.COMMA), eor)
	case *ast.BasicLit:
		ok = tokenvisit1(visit, n, tok(n.ValuePos, n.Kind))
	case *ast.BinaryExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(n.OpPos, n.Op), nod(n.Y), eor)
	case *ast.BlockStmt:
		ok = tokenvisit1(visit, n, tok(n.Lbrace, token.LBRACE), list(n.List, token.ILLEGAL), tok(n.Rbrace, token.RBRACE), eor)
	case *ast.BranchStmt:
		ok = tokenvisit1(visit, n, tok(n.TokPos, n.Tok), nod(n.Label), eor)
	case *ast.CallExpr:
		ok = tokenvisit1(visit, n, nod(n.Fun), tok(n.Lparen, token.LPAREN), list(n.Args, token.COMMA), tok(n.Ellipsis, token.ELLIPSIS), tok(n.Rparen, token.RPAREN), eor)
	case *ast.CaseClause:
		ok = tokenvisit1(visit, n, tok(n.Case, token.CASE), list(n.List, token.COMMA), tok(n.Colon, token.COLON), list(n.Body, token.ILLEGAL), eor)
	case *ast.ChanType:
		switch n.Dir {
		case ast.RECV | ast.SEND:
			ok = tokenvisit1(visit, n, tok(n.Begin, token.CHAN))
		case ast.RECV:
			ok = tokenvisit1(visit, n, tok(n.Begin, token.ARROW), tok(UnknownPos, token.CHAN))
		case ast.SEND:
			ok = tokenvisit1(visit, n, tok(UnknownPos, token.CHAN), tok(n.Arrow, token.ARROW))
		}
		ok = ok && tokenvisit1(visit, n, nod(n.Value), eor)
	case *ast.CommClause:
		t := token.CASE
		if n.Comm == nil {
			t = token.DEFAULT
		}
		ok = tokenvisit1(visit, n, tok(n.Case, t), nod(n.Comm), tok(n.Colon, token.COLON), list(n.Body, token.ILLEGAL), eor)
	case *ast.CompositeLit:
		ok = tokenvisit1(visit, n, nod(n.Type), tok(n.Lbrace, token.LBRACE), list(n.Elts, token.COMMA), tok(n.Rbrace, token.RBRACE), eor)
	case *ast.DeclStmt:
		ok = tokenvisit1(visit, n, nod(n.Decl), eor)
	case *ast.DeferStmt:
		ok = tokenvisit1(visit, n, tok(n.Defer, token.DEFER), nod(n.Call), eor)
	case *ast.Ellipsis:
		ok = tokenvisit1(visit, n, tok(n.Ellipsis, token.ELLIPSIS), nod(n.Elt), eor)
	case *ast.EmptyStmt:
		semi := []ast.Node(nil)
		if !n.Implicit {
			semi = tok(n.Semicolon, token.SEMICOLON)
		}
		ok = tokenvisit1(visit, n, semi, eor)

	case *ast.ExprStmt:
		ok = tokenvisit1(visit, n, nod(n.X), eor)
	case *ast.Field:
		ok = tokenvisit1(visit, n, list(n.Names, token.COMMA), nod(n.Type), nod(n.Tag), eor)
	case *ast.FieldList:
		panic("missed fieldlist")
	case *ast.File:
		ok = tokenvisit1(visit, n, tok(n.Package, token.PACKAGE), nod(n.Name), list(n.Decls, token.ILLEGAL), eor)
	case *ast.ForStmt:
		ok = tokenvisit1(visit, n,
			tok(n.For, token.FOR),
			opt(n.Init, tok(UnknownPos, token.SEMICOLON)),
			nod(n.Cond),
			opt(n.Post, tok(UnknownPos, token.SEMICOLON)),
			nod(n.Body), eor)

	case *ast.FuncDecl:
		// TODO(dmo): explain what's up with the strange ranges of
		// funcdecls and functypes
		ok = tokenvisit1(visit, n,
			tok(n.Type.Func, token.FUNC),
			fieldlist(n.Recv, token.LPAREN, token.COMMA, token.RPAREN),
			nod(n.Name))
		ok = ok && tokenvisit1(visit, n.Type.TypeParams, fieldlist(n.Type.TypeParams, token.LBRACK, token.COMMA, token.RBRACK), eor)
		ok = ok && tokenvisit1(visit, n.Type.Params, fieldlist(n.Type.Params, token.LPAREN, token.COMMA, token.RPAREN), eor)
		ok = ok && tokenvisit1(visit, n.Type.Results, fieldlist(n.Type.Results, token.LPAREN, token.COMMA, token.RPAREN), eor)
		ok = ok && tokenvisit1(visit, n, nod(n.Body), eor)
	case *ast.FuncType:
		ok = tokenvisit1(visit, n, tok(n.Func, token.FUNC))
		ok = ok && tokenvisit1(visit, n.TypeParams, fieldlist(n.TypeParams, token.LBRACK, token.COMMA, token.RBRACK), eor)
		ok = ok && tokenvisit1(visit, n.Params, fieldlist(n.Params, token.LPAREN, token.COMMA, token.RPAREN), eor)
		ok = ok && tokenvisit1(visit, n.Results, fieldlist(n.Results, token.LPAREN, token.COMMA, token.RPAREN), eor)
		ok = ok && tokenvisit1(visit, n, eor)
	case *ast.FuncLit:
		ok = tokenvisit1(visit, n, nod(n.Type), nod(n.Body), eor)
	case *ast.GenDecl:
		ok = tokenvisit1(visit, n, tok(n.TokPos, n.Tok), tok(n.Lparen, token.LPAREN), list(n.Specs, token.ILLEGAL), tok(n.Rparen, token.RPAREN), eor)
	case *ast.GoStmt:
		ok = tokenvisit1(visit, n, tok(n.Go, token.GO), nod(n.Call), eor)
	case *ast.Ident:
		ok = tokenvisit1(visit, n, tok(n.NamePos, token.IDENT))
	case *ast.IfStmt:
		ok = tokenvisit1(visit, n, tok(n.If, token.IF), opt(n.Init, tok(UnknownPos, token.SEMICOLON)), nod(n.Cond), nod(n.Body))
		if n.Else != nil {
			ok = ok && tokenvisit1(visit, n, tok(UnknownPos, token.ELSE), nod(n.Else))
		}
		ok = ok && tokenvisit1(visit, n, eor)
	case *ast.ImportSpec:
		// TODO(dmo): how does n.EndPos, work here?
		ok = tokenvisit1(visit, n, nod(n.Name), nod(n.Path), eor)
	case *ast.IncDecStmt:
		ok = tokenvisit1(visit, n, nod(n.X), tok(n.TokPos, n.Tok), eor)
	case *ast.IndexExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(n.Lbrack, token.LBRACK), nod(n.Index), tok(n.Rbrack, token.RBRACK), eor)
	case *ast.IndexListExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(n.Lbrack, token.LBRACK), list(n.Indices, token.COMMA), tok(n.Rbrack, token.RBRACK), eor)
	case *ast.InterfaceType:
		ok = tokenvisit1(visit, n, tok(n.Interface, token.INTERFACE), fieldlist(n.Methods, token.LBRACE, token.ILLEGAL, token.RBRACE), eor)
	case *ast.KeyValueExpr:
		ok = tokenvisit1(visit, n, nod(n.Key), tok(n.Colon, token.COLON), nod(n.Value), eor)
	case *ast.LabeledStmt:
		ok = tokenvisit1(visit, n, nod(n.Label), tok(n.Colon, token.COLON), nod(n.Stmt), eor)
	case *ast.MapType:
		ok = tokenvisit1(visit, n, tok(n.Map, token.MAP), tok(UnknownPos, token.LBRACK), nod(n.Key), tok(UnknownPos, token.RBRACK), nod(n.Value), eor)
	case *ast.ParenExpr:
		ok = tokenvisit1(visit, n, tok(n.Lparen, token.LPAREN), nod(n.X), tok(n.Rparen, token.RPAREN), eor)
	case *ast.RangeStmt:
		if n.Value != nil {
			ok = tokenvisit1(visit, n, tok(n.For, token.FOR), nod(n.Key), tok(UnknownPos, token.COMMA), nod(n.Value), tok(n.TokPos, n.Tok))
		} else {
			ok = tokenvisit1(visit, n, tok(n.For, token.FOR), nod(n.Key), tok(n.TokPos, n.Tok))
		}
		ok = ok && tokenvisit1(visit, n, tok(n.Range, token.RANGE), nod(n.X), nod(n.Body), eor)
	case *ast.ReturnStmt:
		ok = tokenvisit1(visit, n, tok(n.Return, token.RETURN), list(n.Results, token.COMMA), eor)
	case *ast.SelectStmt:
		ok = tokenvisit1(visit, n, tok(n.Select, token.SELECT), nod(n.Body), eor)
	case *ast.SelectorExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(UnknownPos, token.PERIOD), nod(n.Sel), eor)
	case *ast.SendStmt:
		ok = tokenvisit1(visit, n, nod(n.Chan), tok(n.Arrow, token.ARROW), nod(n.Value), eor)
	case *ast.SliceExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(n.Lbrack, token.LBRACK), nod(n.Low), tok(UnknownPos, token.COLON), nod(n.High))
		if n.Slice3 {
			ok = ok && tokenvisit1(visit, n, tok(UnknownPos, token.COLON), nod(n.High))
		}
		ok = ok && tokenvisit1(visit, n, tok(n.Rbrack, token.RBRACK), eor)
	case *ast.StarExpr:
		ok = tokenvisit1(visit, n, tok(n.Star, token.MUL), nod(n.X), eor)
	case *ast.StructType:
		ok = tokenvisit1(visit, n, tok(n.Struct, token.STRUCT), fieldlist(n.Fields, token.LBRACE, token.ILLEGAL, token.RBRACE), eor)
	case *ast.SwitchStmt:
		ok = tokenvisit1(visit, n, tok(n.Switch, token.SWITCH), opt(n.Init, tok(UnknownPos, token.SEMICOLON)), nod(n.Tag), nod(n.Body), eor)
	case *ast.TypeAssertExpr:
		ok = tokenvisit1(visit, n, nod(n.X), tok(UnknownPos, token.PERIOD), tok(n.Lparen, token.LPAREN), nod(n.Type), tok(n.Rparen, token.RPAREN), eor)
	case *ast.TypeSpec:
		ok = tokenvisit1(visit, n, nod(n.Name), fieldlist(n.TypeParams, token.LBRACE, token.COMMA, token.RBRACE), tok(n.Assign, token.ASSIGN), nod(n.Type), eor)
	case *ast.TypeSwitchStmt:
		ok = tokenvisit1(visit, n, tok(n.Switch, token.SWITCH), opt(n.Init, tok(UnknownPos, token.SEMICOLON)), nod(n.Assign), nod(n.Body), eor)
	case *ast.UnaryExpr:
		ok = tokenvisit1(visit, n, tok(n.OpPos, n.Op), nod(n.X), eor)
	case *ast.ValueSpec:
		ok = tokenvisit1(visit, n, list(n.Names, token.COMMA), nod(n.Type), list(n.Values, token.COMMA), eor)
	default:
		panic("unhandled node")
	}
	return ok
}

func tokenvisit1(visit func(Token) bool, parent ast.Node, components ...[]ast.Node) bool {
	var r []ast.Node
	for _, c := range components {
		r = append(r, c...)
	}
	for _, n := range r {
		switch nn := n.(type) {
		case nodeToken:
			pos := token.NoPos
			if nn.TokPos != UnknownPos {
				pos = nn.TokPos
			}
			tok := Token{
				Tok:  nn.Tok,
				Pos:  pos,
				Node: parent,
			}
			if !visit(tok) {
				return false
			}
		case ast.Node:
			if !tokVisit(n, visit) {
				return false
			}
		}
	}
	return true
}

func fieldlist(f *ast.FieldList, ltok token.Token, between token.Token, rtok token.Token) []ast.Node {
	if f == nil {
		return nil
	}
	var r []ast.Node
	r = append(r, tok(f.Opening, ltok)...)
	r = append(r, list(f.List, between)...)
	r = append(r, tok(f.Closing, rtok)...)
	return r
}

type nodeToken struct {
	TokPos token.Pos
	Tok    token.Token
}

func (n nodeToken) Pos() token.Pos { return n.TokPos }
func (n nodeToken) End() token.Pos { panic("TODO nodeToken.End") }

func nod(n ast.Node) []ast.Node {
	if isZero(n) {
		return nil
	}
	return []ast.Node{n}
}

func tok(pos token.Pos, tok token.Token) []ast.Node {
	if !pos.IsValid() || tok == token.ILLEGAL {
		return nil
	}
	return []ast.Node{nodeToken{TokPos: pos, Tok: tok}}
}

func opt(n ast.Node, delim []ast.Node) []ast.Node {
	if isZero(n) {
		return nil
	}
	ret := make([]ast.Node, 0, len(delim)+1)
	ret = append(ret, n)
	return append(ret, delim...)
}

func list[S ~[]E, E ast.Node](l S, tok token.Token) []ast.Node {
	if len(l) == 0 {
		return nil
	}
	listlen := len(l)
	if tok != token.ILLEGAL {
		listlen += len(l) - 1
	}
	r := make([]ast.Node, 0, listlen)
	for i, n := range l {
		r = append(r, n)
		if i != len(l)-1 && tok != token.ILLEGAL {
			r = append(r, nodeToken{TokPos: UnknownPos, Tok: tok})
		}
	}
	return r
}

var eor = []ast.Node{nodeToken{
	TokPos: UnknownPos,
	Tok:    token.EOF,
}}

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
	r.prevCursor, _ = cur.FindByPos(r.prevPos, r.prevPos)
	r.nextCursor, _ = cur.FindByPos(r.nextPos, r.nextPos)

	if f, ok := r.prevCursor.Node().(*ast.File); ok && r.prevPos < f.Package {
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
	prevPos    token.Pos
	nextPos    token.Pos
	prevTok    token.Token
	nextTok    token.Token
}

func (r *commentRange) String() string {
	var b strings.Builder
	fmt.Fprintln(&b, line(r.comment))
	fmt.Fprintf(&b, "\tprev %s %s\n", line(r.prevPos), r.prevTok)
	cursorstr := "<ROOT>"
	cNode := r.prevCursor.Node()
	if cNode != nil {
		cursorstr = fmt.Sprintf("%T %s", cNode, line(cNode))
	}
	fmt.Fprintf(&b, "\tprev Cursor %s\n", cursorstr)
	fmt.Fprintf(&b, "\tnext %s %s\n", line(r.nextPos), r.nextTok)
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
