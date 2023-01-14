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

## Importing the Original Code
[The first commit][a7f2e4b32f4] imports all code from the original blog-post up
to the **Generate commands** section.

[The second commit][8a7fefdab86] adds the code from the **Generate commands**
and **Execute commands** sections.

[The third commit][48ec8df136e] adds code from the **Delete some users**
section, which causes the tests to fail, as expected and explained in the
original article.

[The fourth commit][7341460c0e7] applies the changes from the
**Fix the model** section, which causes the tests to succeed again.

[The fifth commit][4489784f61e] adds test-case labelling, as explained in the
**Check commands get used** section of the original article.

## Reworking using Hedgehog State-Machine Testing
The above tests are re-implemented using the Hedgehog State-Machine testing
functionality in similar commits:

- The **Generate commands** and **Execute commands** functionality originally
  implemented in [here][8a7fefdab86] is [here][a7f2e-130aef]
  ([compare][8a7fe-130ae]). Note these changes require a slight modification of
  the `abort` function, which clutters the diff somewhat.

- The **Delete some user** functionality originally implemented
  [here][48ec8df136e] is [here][130aef-5a539] ([compare][48ec8-5a539]).

- The fixes from the **Fix the model** section originally implemented
  [here][7341460c0e7] is [here][5a539-b9f906] ([compare][73414-b9f90]).

- The code changes to label test-cases from the **Check commands get used**
  section originally implemented [here][4489784f61e] is [here][b9f906-e6a5e]
  ([compare][44897-e6a5e]).

[The final diff][a7f2e-e6a5e] resembles [the original one][a7f2e-956dd] quite
well!

[hedgehog]: https://hedgehog.qa/
[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[hedgehog-fsm]: https://hackage.haskell.org/package/hedgehog-1.2/docs/Hedgehog.html#g:5
[jacob-stanley]: https://jacobstanley.io
[blog]: https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/

[a7f2e4b32f4]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/a7f2e4b32f4fb1ad6ecb46537c5752b734a8156e
[8a7fefdab86]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/8a7fefdab86635915402b1bada48d99b61c04aab
[48ec8df136e]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/48ec8df136e3f54c9a82bb0f39aa42cd55c6cf3f
[7341460c0e7]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/7341460c0e7cbf8681b68fe9ca893d3ea6e95d91
[4489784f61e]: https://github.com/NicolasT/hedgehog-stateful-demo/commit/4489784f61e2956992ee4dcdb7b194387c33e1a2

[a7f2e-130aef]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/a7f2e4b32f4fb1ad6ecb46537c5752b734a8156e...130aef61081f8eb430faca609b5fd9f7731eb821
[130aef-5a539]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/130aef61081f8eb430faca609b5fd9f7731eb821...5a5395a25f99f9ce177bbcf950f653528885e6ee
[5a539-b9f906]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/5a5395a25f99f9ce177bbcf950f653528885e6ee...b9f90643df745cccb6e12587b0fe94609cb5ca2d
[b9f906-e6a5e]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/b9f90643df745cccb6e12587b0fe94609cb5ca2d...e6a5e95e18cd23ce982bea7c6ca1e6126b47a5b3

[8a7fe-130ae]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/8a7fefdab86635915402b1bada48d99b61c04aab...130aef61081f8eb430faca609b5fd9f7731eb821
[48ec8-5a539]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/48ec8df136e3f54c9a82bb0f39aa42cd55c6cf3f...5a5395a25f99f9ce177bbcf950f653528885e6ee
[73414-b9f90]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/7341460c0e7cbf8681b68fe9ca893d3ea6e95d91...b9f90643df745cccb6e12587b0fe94609cb5ca2d
[44897-e6a5e]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/4489784f61e2956992ee4dcdb7b194387c33e1a2...e6a5e95e18cd23ce982bea7c6ca1e6126b47a5b3

[a7f2e-e6a5e]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/a7f2e4b32f4fb1ad6ecb46537c5752b734a8156e...e6a5e95e18cd23ce982bea7c6ca1e6126b47a5b3
[a7f2e-956dd]: https://github.com/NicolasT/hedgehog-stateful-demo/compare/a7f2e4b32f4fb1ad6ecb46537c5752b734a8156e...956dd5d59f7887f3562f9741e3a87b19c9d4f37b
