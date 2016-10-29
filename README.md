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

