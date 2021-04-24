# portray-diff-quickcheck

Equality assertion functions for `QuickCheck` based on `portray-diff`.

Rather than reporting errors by printing the entirety of both the expected and
actual value, these print a stylized diff between the two that omits equal parts
of the value.  This can be particularly useful when test assertions involve
large data structures.
