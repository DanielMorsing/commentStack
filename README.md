tokenorder is an experiment that tries to represent comments
as state transitions in the go parser, via pairs of ast.Node.
It is adapted from gotype

Right now, all it does is parse the example files and spit out
which pairs of ast.Node comments lie between and has some rudimentary
logic for manipulating and re-anchoring the ASTs. In its current form, it exists mostly
for the purposes of discussion.

One of the goals I have with the approach is to minimize the amount of changes to the standard library, while still allowing arbitrary comment positions to be represented. That part I have down pat. I'm currently working on what modification would look like under this scheme. Having fleshed out the idea a bit, here's how I see the approach (now known as [`tokenorder`](https://github.com/DanielMorsing/tokenorder), for reasons I hope will be apparent):

1. When parsing go source code, make a note of which non-comment tokens appear before or after the comments and associate them with AST nodes. For the tokenorder example, I run the lexer over the source code after it's been parsed to get this information. This obviously won't work for gopls, since it gets its ASTs from the parse cache, but extending the `go/parser` and the `go/ast` package to expose the information would be fairly simple.
2. Perform manipulations on the AST, making note of which subtrees have been modified.
3. Before printing, map the comments to new AST pairs. This can be done by walking the subtrees in token order. Token order is essentially mimicking what the `go/printer` package does, but instead of emiting tokens, it yields `ast.Node`s in the order that tokens would appear in a printed source file.
4. With the updated information gathered from the token order walk, print the comments when the printer encounters those pairs. In the current tokenorder implementation, I've had this as a callback via an interface, but I'm working on ways to simplify the interface

An example for how this would work in a modernizer context, take this code:

```
func b(a int) bool {
	if a < 0 {
		panic("invalid arg")
	}
	// some calculation
	...
}
...
switch {
case a, b(x) /* comment */:
}
```
In this case, the comment would be marked as occuring between the `CallExpr` and the `CaseClause`

Imagine that we then make a modernizer that changes b to return an error instead of panicking
```
val, err := b(x) /* comment */
if err != nil {
	panic("err")
}
switch {
case a, val:
}
```
Walking the statement list in token order will at some point yield the `CallExpr` but with a different following node. We can at that point change the node mapping for the comment to be between the `CallExpr` and the `IfStmt`. The specifics are still a bit "to be determined" on how I do this mapping under arbitrary changes, but for grafting nodes to different parts of the AST, it generally works well.

Couple of caveats to this plan:
- It works on pointer equality. If you modify the AST in a way that adds new nodes in place of old ones rather than modifying them in place, the associations breaks. We could do some clever things to map newly allocated nodes (e.g. build a merkle tree and map the comments to hashes instead of nodes), but I don't think these will hold up over multiple simultaneous changes. Adding information in the `ast.Node` is possible and would somewhat fix the problem, but the nature of this approach means encoding a lot of cross-cutting information into the AST that would be hard to parse and unhelpful for most users.
- It does almost nothing to help with adding comments to the AST that didn't occur in a previous parsed go file. You could technically make a mapping to new nodes during the token walk, but I think a much simpler approach is to keep CommentedNode and extend it to have hints about where a comment occurs (before/after/above).
