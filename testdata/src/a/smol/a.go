package a

func bar() int

//go:fix inline
func foobar() int {
	return bar()
}

func x() {
	switch {
	case foobar(), // for fluuxing
		a + b:
		// because Something
	}
}
