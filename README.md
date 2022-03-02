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

SO WHAT IS THIS?
================
This is the Metal branch of Futhark, a version of Futhark which culls OpenCL/CUDA and implements Metal support for Apple/Mac GPUs
enabling Futhark to compile in the OS X ecosystem. There are a lot of features planned for this branch, some of which are addressed below but an exhaustive list is in progress. Below is also a collection of writing about the experience, goals, and milestones; any help is appreciated as it's currently maintained by just me (Miles J. Litteral/SasoriZero) but I look forward to showing you the progress!

Current Working Version: v0.0.2(Alpha)
Past Versions: v0.0.1(Pre-Alpha)

Gists on the Project(Planning, theory, How Metal is Different from CUDA/OpenCL, the Method/Mechanisms behind Futhark-Metal that make it possible):
https://gist.github.com/MilesLitteral/3ae5c427eab97b32d87b7e311d028efe

Keep an eye out for/on the mlir llvm branch which is coming soon!
I have plans to also tackle the Vulkan Branch at some point (currently stale, long-term future) 

Roadmap
=======
## Step 0

* Connect mtlpp or metal-cpp to Futhark Where it can see main library header (mtlpp.hpp choosen) ✔️

* Make Proof of Concept experiments in Metal demonstrating Objective-C++ and how it can be used
For Futhark's purposes, in this case, potential use-cases (mtlAdder, mtlDot, mtlLib, mtlRenderer) ✔️

* Build MetalEngine Class based off this research, implement as main mechanism of Futhark-Metal ✔️

## Step 1

* Create and Implmement Futhark.Backend.Metal and all necessary Haskell Code (2/20/22)✔️

* Build/Compile Prototypes of Futhark-Metal (v001/v002) ✔️ 

## Step 2

* Stress Test Prototypes <- You are Here!(3/1/22) 🔵

* Refine Output Code of Prototypes ✖️

* Implement build flags (Metal Script, Metallib, MetalDyLib) ✖️

* Unreliable Output ✖️

## Release

* Feature Parity with Futhark OpenCL/CUDA       ✖️

* Platform agnostic creation of Metallib/DyLib  ✖️

* Reliable Output   ✖️


## Optional
Futhask-Metal

MoltenVK/MoltenGL bridge:
MVKCreateInstance
MVKBuffer
MVKPipeline
MVKExecuteOn

Platform-Free Execution (subject to being removed potentially, depends on how development goes)
Quartz++
