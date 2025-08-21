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
	for fileCur := range root.Preorder((*ast.File)(nil)) {
		file := fileCur.Node().(*ast.File)

		fmt.Println(fset.Position(file.Name.Pos()).Filename)
		sortComments(file.Comments)
		// File is special, since there is no previous ast node to anchor to
		var idx int
		var fileroute CommentRoute
		if len(file.Comments) > 0 {
			up, left, right := GetBounds(fset, nil, file, nil)
			cmt := file.Comments[idx]
			for {
				switch {
				case cmt.Pos() < up:
					fileroute.up = append(fileroute.up, cmt)
				case cmt.Pos() < left:
					fileroute.left = append(fileroute.left, cmt)
				case cmt.Pos() < right:
					fileroute.right = append(fileroute.right, cmt)
				}
				idx++
				if idx >= len(file.Comments) || cmt.Pos() >= right {
					break
				}
				cmt = file.Comments[idx]
			}
		}
		route[fileCur.Index()] = fileroute
		fmt.Println(fileroute.up, fileroute.left, fileroute.right)

		for _, cmt := range file.Comments[idx-1:] {
			// Could probably do something clever with Inspect, but FindByPos is
			// fast and we can easily walk to the important nodes
			cur, ok := fileCur.FindByPos(cmt.Pos(), cmt.Pos())
			if !ok {
				panic("huh?")
			}
			var next inspector.Cursor
			for next = range cur.Preorder() {
				if next.Node().Pos() > cmt.End() {
					break
				}
			}
			prev, ok := next.PrevSibling()
			if !ok {
				prev = next.Parent()
			}
			// comment between the end of the previous
			if cmt.Pos() > prev.Node().End() {

			}

			fmt.Printf("%s\n%s\n", fset.Position(cmt.Pos()), fset.Position(cmt.End()))
			fmt.Printf("\t CUR %s %T\n", fset.Position(cur.Node().Pos()), cur.Node())
			fmt.Printf("\t PREV %s %T\n", fset.Position(prev.Node().Pos()), prev.Node())
			fmt.Printf("\t NEXT %s %T\n", fset.Position(next.Node().Pos()), next.Node())
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

func line(pos token.Pos) string {
	position := fset.Position(pos)
	return fmt.Sprintf("./%s:%v", position.Filename, position.Line)
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
