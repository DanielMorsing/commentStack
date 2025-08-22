package a

import (
	"errors"
	"net/http"
)

func alwaysFalse() {
	return false
}

// Yes
func /* dumdum */ send(ireq *http.Request) (resp *http.Response, didTimeout func() bool, err error) {
	// comment before a statement
	b := c

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
