# Blox
This repository contains the source code for the Blox programming language. 

#### Directory Structure

* __src__  : source files 
* __out__  : generated files
* __amf__  : generated AMF files
* __tests__: language tests
* __res__  : resource files
* __BloxTest__ : Working proof-of-concept Java program

#### Usage

* Running __make__ creates the `scanner.ml` source file, compiles the `ast.ml` 
file  into the `ast.cmo` bytecode and `ast.cmi` compiled interface, and creates 
the `parser.ml` source file and `parser.mli` interface. All generated files are 
placed in `gen/`
* Running __make clean__ removes all auxiliary files in `gen/`

#### TODO 

* Update/complete parser.mly
* Update/complete ast.ml
* Complete the compiler back-end
* Create tests for Scanner and Parser

#### Block Orientation
![Orient](res/orient.png?raw=true)
