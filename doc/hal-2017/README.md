# Tutorial

## Graphical User Interfaces With Threepenny

Heinrich Apfelmus

Haskell in Leipzig, 27 October 2017

----

This folder contains various materials for the tutorial:

* [slides.pdf](slides.pdf) — presentation slides
* [src/](src/) — examples presented during tutorial

----

To install the library and run the first example,
go into the `src/` folder and run either

    stack build threepenny-gui
    ./runhaskell E01_hello_world.hs

or

    cabal install threepenny-gui
    ./runhaskell E01_hello_world.hs

depending on whether you prefer to use [stack][]
or [cabal][] for installing Haskell libraries.
The first command works best if you have GHC-8.0.2 installed.

Then, point your web browser to

  <http://localhost:8023>


  [stack]: https://docs.haskellstack.org/en/stable/README/
  [cabal]: https://www.haskell.org/cabal/

