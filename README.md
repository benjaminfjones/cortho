# Cortho

![Travis CI Badge](https://travis-ci.org/benjaminfjones/cortho.svg?branch=master)

Cortho is a compiler project for a small, core functional language. The
design is heavily influenced by the [book] by Simon Peyton Jones and David
Lester.


## Implementation Goals

One goal is to use pure Haskell and only those libraries that ship with GHC.
This has already been thrown by the wayside: the parser depends on `parsec`.
We think this is a modest dependency.


## License

Cortho version 0.1.0 is released under the 3-clause BSD license. See the [LICENSE]
file.


## References

[book]: S. Jones and D. Lester, "Implementing Functional Languages: A Tutorial" (1992). http://research.microsoft.com/en-us/um/people/simonpj/Papers/pj-lester-book

[LICENSE]: LICENSE
