# Building Projects in Haskell

## Modules

Haskell programs are organized into modules. Modules contain the datatypes, type synoyms, typeclasses, typeclass instances, and values you've defined at the top-level.

## Managing projects with Cabal

The Haskell Cabal, Common Architecture for Building Applications and Libaries, is a package and dependency manager.

The recommended basic structure:
* Make a `src` directory for source code.
* Make a file called `Main.hs` taht is the main Haskell source code file.
* Use `cabal init` automatically generate the configuration and licensing files.
* Initialize git for version control.
* Your project directory will also contain the test suites and any other files that your `Main` module depends on.
