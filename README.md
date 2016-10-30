# Blox
This repository contains the source code for the Blox programming language. 

#### Directory Structure

* __src__  : source files 
* __out__  : generated files
* __amf__  : generated AMF files
* __tests__: language tests

#### Usage

* Running __make__ creates the `scanner.ml` file, compiles the `ast.ml` file into the `ast.cmo` bytecode and `ast.cmi` compiled interface. All generated files are placed in `gen/`.
* Running __make clean__ removes all auxiliary files in `gen/`.


#### TODO 

* Update/complete scanner.mll.
* Update/complete parser.mly.
* Update/complete ast.ml.
* Create tests for Scanner and Parser.


#### Data Type Definitions
* The programmer cannot access individual Block objects, but they are defined
within the compiler to be an array of 6 tuples. Each tuple represents one face
of the Block. Each tuple contains first a single bit that is: 1 if the face is
joined, or 0 if it is available. Each tuple contains second a floating-point
number that represents the color of that face. The tuples are arranged in the
array in the order North (+y), East (+x), South (-y), West (-x), Top (+z),
and Bottom (-z). An example Block looks like:
[(0, 0.00), (0, 0.00), (0, 0.00), (0, 0.00), (0, 0.00), (0, 0.00)]
