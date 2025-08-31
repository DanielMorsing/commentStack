package a

func bar() bool

//go:fix inline
func foobar() bool {
	return bar()
}

var a, b bool

func x() {
	if x /*something*/ {

	}
	switch {
	case foobar() /* for fluuxing */, a || b:
		stmt()
		// because Something
		stmt()
	case somethingelse:
		// something
	}

	foobar.v( /* jfkl */
	)
	foobar.v() // fjkls
	foobar.v /*fjkl*/ ()
}

func something( /*argument*/ a int, /*something*/
	b int) {
}

type s struct {
	b/* int because?? */ int /*intls*/
	y                        int
}
