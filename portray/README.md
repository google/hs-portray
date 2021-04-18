# portray

A pseudo-Haskell syntax type and typeclass producing it.

This provides a single place to describe how a type should be formatted as
pseudo-Haskell syntax, independently of the actual pretty-printing library (e.g.
"pretty", "ansi-wl-pprint", or "prettyprinter") that will ultimately be used to
render it. This means packages can cheaply provide integration with all
pretty-printers by providing an instance of this class, without needing to
depend on any of them (or their layout algorithms).

Of course, this comes at the cost of targeting a predeclared set of formatting
choices. If there are any egregious omissions, we can always add more
constructors.

This library is explicitly not intended to provide a way to express /all
documents/; only a particular flavor of pseudo-Haskell syntax representing
values and types.
