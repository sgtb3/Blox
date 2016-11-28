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
	
### Converting a Frame to AMF

* AMF needs 8 vertices to represent a cuboid and 12 vertices to represent volume (i.e. 2 triangles for each of the 6 faces). 

#### The 7 points will be represented in a table `(Point, <x,y,z>)`
```
AMF Vertex #  |   AMF Vertex coordinates <1, 1, 1>
---------------------------------------------------
  0           |   <1,1,1>
  1           |   <1,0,1>
  2           |   <0,1,1>
  3           |   <0,0,1>
  4           |   <1,1,0>
  5           |   <1,0,0>
  6           |   <0,1,0>
  7           |   <0,0,0>
```

* The important thing to note here is that we can stretch this cuboid in any possible direction(s) and the AMF coordinates
  will only change my a multiple of of x and/or y and/or z. Therefore we need to hard code this matrix into our compiler.
* For example, `Frame <1,1,1> A;` has the table represented above.
```
Frame <1,3,1> B;  (which differs from A only in y direction) has the the following table:
AMF Vertex #  |   AMF Vertex coordinates <1, 3, 1>
---------------------------------------------------
  0           |   <1,3,1>
  1           |   <1,0,1>
  2           |   <0,3,1>
  3           |   <0,0,1>
  4           |   <1,3,0>
  5           |   <1,0,0>
  6           |   <0,3,0>
  7           |   <0,0,0>

Frame <1,1,3> C;  (which differs from A only in z direction) has the the following table:
AMF Vertex #  |   AMF Vertex coordinates <1, 1, 3>
---------------------------------------------------
  0           |   <1,1,3>
  1           |   <1,0,3>
  2           |   <0,1,3>
  3           |   <0,0,3>
  4           |   <1,1,0>
  5           |   <1,0,0>
  6           |   <0,1,0>
  7           |   <0,0,0>
  
Frame <3,3,1> D;  (which differs from A only in x and y directions) has the the following table:
AMF Vertex #  |   AMF Vertex coordinates <3, 3, 1>
---------------------------------------------------
  0           |   <3,3,1>
  1           |   <3,0,1>
  2           |   <0,3,1>
  3           |   <0,0,1>
  4           |   <3,3,0>
  5           |   <3,0,0>
  6           |   <0,3,0>
  7           |   <0,0,0>

etc.
```

* Basically you're just multiplying the frames x, y, and z dimensions in only their respective columns against the "base" `<1,1,1>` block. 
* Side note: if we represented this as `<z,x,y>` we would have a truth table for 3 values 

#### The AMF volume vertices for each frame will never change since frames will always be a cuboid
* So this table needs to be hard coded also:
```
Volume Triangles   |   AMF coordinates
----------------------------------------
  Front  Volume    |   <1,2,3>, <0,1,2> 
  Back   Volume    |   <5,6,7>, <4,5,6> 
  Top    Volume    |   <0,4,6>, <0,2,6> 
  Bottom Volume    |   <1,5,7>, <1,3,7> 
  Left   Volume    |   <2,6,7>, <2,3,7> 
  Right  Volume    |   <0,4,5>, <0,1,5> 
```
