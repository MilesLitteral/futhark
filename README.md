<img src="assets/logo.svg" height="50px"/> The Futhark Programming Language
==========

[![Join the chat at https://gitter.im/futhark-lang/Lobby](https://badges.gitter.im/futhark-lang/Lobby.svg)](https://gitter.im/futhark-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![CI](https://github.com/diku-dk/futhark/workflows/CI/badge.svg)](https://github.com/diku-dk/futhark/actions)[![DOI](https://zenodo.org/badge/7960131.svg)](https://zenodo.org/badge/latestdoi/7960131)

Futhark is a purely functional data-parallel programming language in
the ML family.  It can be compiled to typically very efficient
parallel code, running on either a CPU or GPU.  The language and
compiler are developed at [DIKU](http://diku.dk) at the University of
Copenhagen, originally as part of the [HIPERFIT
centre](http://hiperfit.dk).  The language and compiler are quite
stable and suitable for practical programming.

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

We try to make use of GitHub issues for organising our work.  Issues
tagged with
[good first issue](https://github.com/diku-dk/futhark/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
do not require deep knowledge of the code base.


SO WHAT IS FUTHARK-METAL?
=============

In Brief: this fork of the main Futhark Project hopes to accomplish execution of Futhark on M1 Apple GPU/APUs by creating an addon module which can then create and compile Metal Shader Language Code along with Objective-C++ Code (Instead of CUDA or OpenCL) on Mac, therefore opening the floodgates on platform agnostic GPU support that Futhark provides, development is currently very hot. 

This project has further, future, steps which includes creating an MLIR (LLVM Dialect) backend for Futhark though this is a future objective.

Issues and progress are tracked through this repository, it's Project page on @MilesLitteral GitHub.

Discussion, technical talks, brainstorming and drafting are all posted to the FM Gist, feel free to read or contribute to the conversation: 
https://gist.github.com/MilesLitteral/3ae5c427eab97b32d87b7e311d028efe

A handy test command for the build (in root) is `futhark metal dotproduct.fut`

## Further Readings and Project Notes
https://gist.github.com/MilesLitteral/3ae5c427eab97b32d87b7e311d028efe
