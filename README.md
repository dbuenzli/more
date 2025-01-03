More – A minimal toolkit for everyday OCaml programs
====================================================

More is a toolkit for everyday OCaml programs. It focuses on providing
an ergonomic system library and the minimal tools you end up needing
in most of your programs.

More **is not** an alternative or extended OCaml standard library.
Despite the persistent myths, the OCaml standard library is fine and
improving. More sometimes extends the standard library modules but,
most of the time, only with the aim to test and eventually upstream
the addition.

More is distributed under the ISC license. It depends on the `unix`
library. It optionally depends on the [`cmdliner`] library.

Homepage: <https://erratique.ch/software/more/>

[`cmdliner`]: <http://erratique.ch/software/cmdliner>
  
## Installation

More can be installed with `opam`

    opam install more
    opam install more cmdliner # With cmdliner support

## Documentation

The documentation can be consulted [online] or via `odig doc more`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker. 

[online]: https://erratique.ch/software/more/doc
[OCaml forum]: https://discuss.ocaml.org/

## Examples

A few examples can be found in the [test](test/) directory. 

