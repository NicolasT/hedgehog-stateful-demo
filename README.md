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

[The first commit][a7f2e4b32f4] imports all code from the original blog-post up
to the **Generate commands** section.

[The second commit][8a7fefdab86] adds the code from the **Generate commands**
and **Execute commands** sections.

[The third commit][48ec8df136e] adds code from the **Delete some users**
section, which causes the tests to fail, as expected and explained in the
original article.

[The fourth commit][7341460c0e7] applies the changes from the
**Fix the model** section, which causes the tests to succeed again.

This commit adds test-case labelling, as explained in the
**Check commands get used** section of the original article.

[hedgehog]: https://hedgehog.qa/
[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[hedgehog-fsm]: https://hackage.haskell.org/package/hedgehog-1.2/docs/Hedgehog.html#g:5
[jacob-stanley]: https://jacobstanley.io
[blog]: https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/

[a7f2e4b32f4]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/a7f2e4b32f4fb1ad6ecb46537c5752b734a8156e
[8a7fefdab86]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/8a7fefdab86635915402b1bada48d99b61c04aab
[48ec8df136e]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/48ec8df136e3f54c9a82bb0f39aa42cd55c6cf3f
[7341460c0e7]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/7341460c0e7cbf8681b68fe9ca893d3ea6e95d91
