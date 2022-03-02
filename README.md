<img src="assets/logo.svg" height="50px"/> The Futhark Programming Language
==========

[![Join the chat at https://gitter.im/futhark-lang/Lobby](https://badges.gitter.im/futhark-lang/Lobby.svg)](https://gitter.im/futhark-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![CI](https://github.com/diku-dk/futhark/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark/actions)[![DOI](https://zenodo.org/badge/7960131.svg)](https://zenodo.org/badge/latestdoi/7960131)

Futhark is a purely functional data-parallel programming language in
the ML family.  It can be compiled to typically very efficient
parallel code, running on either a CPU or GPU.  The language is
developed at [DIKU](http://diku.dk) at the University of Copenhagen,
originally as part of the [HIPERFIT centre](http://hiperfit.dk).  It
is quite stable and suitable for practical programming.

For more information, see:

* [A collection of code examples](https://futhark-lang.org/examples.html)

* [Installation instructions](http://futhark.readthedocs.io/en/latest/installation.html)

* [The main website](http://futhark-lang.org)

* [Parallel Programming in
  Futhark](https://futhark-book.readthedocs.io/en/latest/), an
  extensive introduction and guide

* [The Futhark User's Guide](http://futhark.readthedocs.io)

* [Documentation for the built-in prelude](https://futhark-lang.org/docs/prelude)

* [Futhark libraries](https://futhark-lang.org/pkgs/)

[![Packaging status](https://repology.org/badge/vertical-allrepos/futhark.svg)](https://repology.org/project/futhark/versions)

Hacking
=======

We welcome all contributions!

Issues tagged with [good first
issue](https://github.com/diku-dk/futhark/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
do not require deep knowledge of the code base.

For contributing code, see also [the style guide](STYLE.md).

Method?
=======

https://github.com/google/mlir-hs

Sample Code?
============
    func @foo(%arg0: i32, %arg1: i64) -> (i32, i64) {
      return %arg0, %arg1 : i32, i64
    }
    func @bar() {
      %0 = arith.constant 42 : i32
      %1 = arith.constant 17 : i64
      %2:2 = call @foo(%0, %1) : (i32, i64) -> (i32, i64)
      "use_i32"(%2#0) : (i32) -> ()
      "use_i64"(%2#1) : (i64) -> ()
    }

    // is transformed into

    llvm.func @foo(%arg0: i32, %arg1: i64) -> !llvm.struct<(i32, i64)> {
      // insert the vales into a structure
      %0 = llvm.mlir.undef : !llvm.struct<(i32, i64)>
      %1 = llvm.insertvalue %arg0, %0[0] : !llvm.struct<(i32, i64)>
      %2 = llvm.insertvalue %arg1, %1[1] : !llvm.struct<(i32, i64)>

      // return the structure value
      llvm.return %2 : !llvm.struct<(i32, i64)>
    }
    llvm.func @bar() {
      %0 = llvm.mlir.constant(42 : i32) : i32
      %1 = llvm.mlir.constant(17) : i64

      // call and extract the values from the structure
      %2 = llvm.call @bar(%0, %1)
         : (i32, i32) -> !llvm.struct<(i32, i64)>
      %3 = llvm.extractvalue %2[0] : !llvm.struct<(i32, i64)>
      %4 = llvm.extractvalue %2[1] : !llvm.struct<(i32, i64)>

      // use as before
      "use_i32"(%3) : (i32) -> ()
      "use_i64"(%4) : (i64) -> ()
    }
