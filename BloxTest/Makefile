JFLAGS = -g
JC = javac

.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
    Block.java \
    Frame.java \
    Test.java \

default: clean classes test

classes: $(CLASSES:.java=.class)

test: 
	java Test

.PHONY: clean
clean:
	rm -f *.class
