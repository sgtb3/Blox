# Blox
This repository contains the source code for the Blox programming language. 

#### Directory Structure

* __src__  : source files 
* __out__  : compiled interface and generated files
* __obj__  : compiled object files
* __tests__: language tests

#### Makefile

* __make__ creates the Blox compiler
* __make AST-Test__ tests the Blox front-end using the sample `HelloWorld.blox` file in `src/` 
* __make Run-Test-Script__ runs the test suite

#### Usage

`make`
`./blox -c <inputFile.blox>`