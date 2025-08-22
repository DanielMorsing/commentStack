package main

import (
	"cmp"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"log"
	"path/filepath"
	"slices"
	"sync"
	"time"

	"golang.org/x/tools/go/ast/edge"
	"golang.org/x/tools/go/ast/inspector"
)

var (
	fset = token.NewFileSet()
)

func main() {
	files, err := parseDir("./testdata/src/a/http")
	if err != nil {
		log.Fatalf("cannot parse: %s", err)
	}
	inspect := inspector.New(files)
	root := inspect.Root()

	for fileCur := range root.Preorder((*ast.File)(nil)) {
		file := fileCur.Node().(*ast.File)

		fmt.Println(fset.Position(file.Name.Pos()).Filename)
		sortComments(file.Comments)
		ranges := make([]commentRange, len(file.Comments))
		// Each comment will be bounded by 2 cursors.
		// One cursor for the node that last produced a token before the comment
		// and one cursor for the node that will introduce a token after the comment
		for i, cmt := range file.Comments {
			fmt.Printf("looking for %s\n", line(cmt))
			outer, _ := fileCur.FindByPos(cmt.Pos(), cmt.Pos())
			n := outer.Node()
			fmt.Printf("starting at %T %s\n", n, line(n))
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
			r := commentRange{cmt, prev, next}
			r.adjust()
			ranges[i] = r
		}
		for i, r := range ranges {
			fmt.Println(line(file.Comments[i]))
			prev := r.prev.Node()
			fmt.Printf("\tprev %T %s\n", prev, line(prev))
			next := r.next.Node()
			fmt.Printf("\tnext %T %s\n", next, line(next))
		}
	}
}

type commentRange struct {
	comment *ast.CommentGroup
	prev    inspector.Cursor
	next    inspector.Cursor
}

func (r *commentRange) adjust() {
	left := r.prev
	right := r.next
	parent := left.Parent()
	// the boundary between a parent and a child
	// is always unambiguous
	if right.Parent() != parent {
		return
	}
	lk, _ := left.ParentEdge()
	rk, _ := right.ParentEdge()
	if lk == edge.KeyValueExpr_Key && rk == edge.KeyValueExpr_Value {
		kv := parent.Node().(*ast.KeyValueExpr)
		if r.comment.Pos() < kv.Colon {
			r.next = parent
		} else {
			r.prev = parent
		}
	}
}

func GetBounds(fset *token.FileSet, prev ast.Node, node ast.Node, next ast.Node) (up, left, right token.Pos) {
	if f, ok := node.(*ast.File); ok {
		up, right = lineRange(fset, f.Package)
		left = f.Package
		return up, left, right
	}
	panic("unreachable")
}

type CommentRoute struct {
	up    []*ast.CommentGroup
	down  []*ast.CommentGroup
	left  []*ast.CommentGroup
	right []*ast.CommentGroup
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
