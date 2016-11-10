# Blox
This repository contains the source code for the Blox programming language. 

#### Directory Structure

* __src__  : source files 
* __out__  : generated files
* __amf__  : generated AMF files
* __tests__: language tests
* __res__  : resource files
* __BloxTest__ : proof-of-concept Java program

#### Usage

* Running __make__ creates the `scanner.ml` source file, compiles the `ast.ml` 
file  into the `ast.cmo` bytecode and `ast.cmi` compiled interface, and creates 
the `parser.ml` source file and `parser.mli` interface. All generated files are 
placed in `gen/`.
* Running __make test__ runs the `testAll` shell script and displays the 
contents of the generated `testall.log` file.
* Running __make menhirtest__ runs the `menhir` interpreter for debugging.
* Running __make clean__ removes all auxiliary files in `gen/`.


#### TODO 

* Complete the compiler back-end
* Complete Hello World
* Create test cases
* Optimize front-end

#### Block Orientation
![Orient](res/orient.png?raw=true)
