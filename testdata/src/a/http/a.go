package a

import (
	"errors"
	"net/http"
)

func alwaysFalse() {
	return false
}

type s [] /*dum*/ int

// Yes
func /* dumdum */ (rev *recv) send(ireq *http.Request) (resp *http.Response, didTimeout func() bool, err error) {
	// comment before a statement
	b := c

	switch {
	case a, /* something */
		b:
	}

	// hello
	if rt == nil /* comment inside */ {
		// comment in if
		return nil, alwaysFalse, errors.New("http: no Client.Transport or DefaultTransport")
	}

	if req.URL == nil { // comment after if
		return nil, alwaysFalse, errors.New("http: nil Request.URL")
	}
	c := Var{
		field: something, // Field comment
	}

	foo := ident{
		field /* huh */ : something,
	}
	foo := ident{
		field: /* huh */ something,
	}

}
