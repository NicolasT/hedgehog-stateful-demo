# hedgehog-stateful-demo

This repository contains some code demonstrating testing of a stateful,
database-backed application using [Hedgehog][hedgehog], a library
similar in spirit to the well-known [QuickCheck][quickcheck] library.
Hedgehog comes with
[built-in functionality to test state machines][hedgehog-fsm] but the
API may be a slightly daunting, hence this demo.

It's based on a blog-post by [Jacob Stanley][jacob-stanley]:
"[How to use Hedgehog to test a real world, large scale, stateful app][blog]".
Make sure to read it first!

This commit imports all code from the original blog-post up to the
**Generate commands** section.

[hedgehog]: https://hedgehog.qa/
[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[hedgehog-fsm]: https://hackage.haskell.org/package/hedgehog-1.2/docs/Hedgehog.html#g:5
[jacob-stanley]: https://jacobstanley.io
[blog]: https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/

