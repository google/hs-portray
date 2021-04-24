# portray-diff

Implements Generics-based diffing of structured values, primarily for testing.

By using `diff` on large values in tests rather than `==`, your tests can
pinpoint what part of a value differs from the expected value rather than just
dumping the entirety of both values.
