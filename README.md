# FFP

Our solution for the [advanced functional programming](https://tiss.tuwien.ac.at/course/educationDetails.xhtml?semester=2024S&courseNr=185A05) lecture at [TU Wien](https://www.tuwien.at/en/).

## Run the examples and tests:

```bash
$ ghci
GHCi, version 9.2.8: https://www.haskell.org/ghc/  :? for help
ghci> :load exercise1.hs
[1 of 1] Compiling Main             ( exercise1.hs, interpreted )
Ok, one module loaded.
ghci> runTests
passed gierig 2/3
[...]
ghci>
```

Of course your ghc version might vary, and you will need to replace the exercise
version with the one you want to run.

To enable time tracking inside ghci, you can use `:set +s`.

## Assignment 3

In order to run assignment 3, the library QuickCheck is needed. You can install it with:

```bash
cabal update
cabal install --lib QuickCheck
```

## Project

The last assignment was the miniCheck project and since it is quite large it 
has it's own subdirectory with it's own README.