# Blox
This repository contains the source code for the Blox programming language. 

#### Directory Structure

* __src__  : source files 
* __out__  : generated files
* __obj__  : compiled object files
* __amf__  : AMF resources
* __tests__: language tests
* __res__  : resource files
* __BloxTest__ : proof-of-concept Java program

#### Usage

* Running __make__ creates the `scanner.ml` source file, compiles the `ast.ml` 
file  into the `ast.cmo` bytecode and `ast.cmi` compiled interface, and creates 
the `parser.ml` source file and `parser.mli` interface. All generated files are 
placed in `gen/` and all compiled object files are places in `obj/`.
* Running __make test__ runs the `testAll` shell script and displays the 
contents of the generated `testall.log` file.
* Running __make menhirtest__ runs the `menhir` interpreter for debugging.
* Running __make clean__ removes all auxiliary files in `gen/`.


#### TODO 

* Complete the compiler back-end
* Finish the remainder of Hello World
* Create test suite
* Optimize front-end

#### Block Orientation
![Orient](res/orient.png?raw=true)

#### Joins
Join(Frame A, [Set of optional Faces to Join on in A], Frame B, [Set of optional Faces to Join on in B] )

Join takes two frames and the set of faces (1,1,1,E) (rules) that the join can take place on, this relationship can be one to one, or many to many.

	/* Create an H */
	Frame H = five;
	Join(H,(1,1,3,E), two, (1,1,1,W));
	Join(H,(3,1,3,E), five, (1,1,3,W));