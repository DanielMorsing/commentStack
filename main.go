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
	for fileCur := range root.Preorder((*ast.File)(nil)) {
		file := fileCur.Node().(*ast.File)

		fmt.Println(fset.Position(file.Name.Pos()).Filename)
		sortComments(file.Comments)
		for i, c := range file.Comments {
			if i != 0 && file.Comments[i-1].Pos() > c.Pos() {
				panic("unordered comments")
			}
		}
	}
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
	fmt.Println(filename)
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments|parser.SkipObjectResolution) // ok to access fset concurrently
	//ast.Print(fset, file)
	return file, err
}
