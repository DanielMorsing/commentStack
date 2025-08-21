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
	// TODO(dmo): if we get "maxevent" from the inspector, this could be a slice
	route := make(map[int32]CommentRoute)
	_ = route

	for fileCur := range root.Preorder((*ast.File)(nil)) {
		file := fileCur.Node().(*ast.File)

		fmt.Println(fset.Position(file.Name.Pos()).Filename)
		sortComments(file.Comments)
		ranges := make([]cursorRange, len(file.Comments))
		var idx int
		prev := fileCur
		fmt.Printf("looking for %s\n", line(file.Comments[idx]))
		fileCur.Inspect(nil, func(c inspector.Cursor) bool {
			// should probably filter with the cursor, but
			// these are leaf nodes, so no biggie
			if _, ok := c.Node().(*ast.CommentGroup); ok {
				return false
			}
			if idx == len(file.Comments) {
				return false
			}
			fmt.Printf("\t%T\t%s\n", c.Node(), line(c.Node()))
			cmt := file.Comments[idx]
			n := c.Node()
			// comment within this AST node, descend
			if n.Pos() < cmt.Pos() && cmt.End() < n.End() {
				prev = c
				return true
			}
			if cmt.End() < c.Node().Pos() {
				ranges[idx] = cursorRange{prev, c}
				idx++
				prev = c
				if idx == len(file.Comments) {
					return false
				} else {
					fmt.Printf("looking for %s\n", line(file.Comments[idx]))
				}
				return true
			}
			return false
		})
	}
}

type cursorRange struct {
	prev inspector.Cursor
	next inspector.Cursor
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
	return fmt.Sprintf("./%s:%v:%v;%v:%v", begin.Filename, begin.Line, begin.Column, end.Line, end.Column)
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
