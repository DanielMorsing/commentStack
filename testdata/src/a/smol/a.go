package a

func bar() bool

//go:fix inline
func foobar() bool {
	return bar()
}

var a, b bool

func x() {
	if x = x; ok /*inside if*/ {

	}
	test, foobar = foobar, test
	switch {
	case foobar() /* caseclause */, a || b:
		stmt()
		// between caseclause stmts
		stmt()
	case somethingelse:
		// other clause
	}

	foobar.v( /* between call */
	)
	foobar.v() // after call
	foobar.v /*between expr and call*/ ()
}

func something( /*argument*/ a int, /*something*/
	b int) {
}

type s struct {
	b/* int because?? */ int /*intls*/
	y                        int
}
