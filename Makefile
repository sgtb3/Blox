# phony targets
.PHONY: all clean scanner parser test asttypes parsertypes 
	parsercomp scannercomp bloxcomp scannertest parsertest 
	test showtestlog

# name of output file
EXEC = blox_exec.amf

# name of test shell script
TESTSH = testAll

# bytecode object file extension
OBJEXT = .cmo

# lexer (scanner) and parser generators
# 	-v : verbose output option for ocamlyacc
LEXGEN = ocamllex
PARSGEN = ocamlyacc
PARSGENFLAG = -v

# ocaml compiler and flags
# 	-c : compile without producing executable files
# 	-o : specify name of output file produced by compiler
OCC = ocamlc
OCCFLAGS1 = -c
OCCFLAGS2 = -o 

# source files, generated files, testing, and AMF/output directories
SRCDIR = src
GENDIR = gen
TSTDIR = tests
AMFDIR = amf

VPATH = src:gen
testfiles := $(wildcard $(TSTDIR)/*)

# default makefile target
all: scanner asttypes parser

# creates scanner.ml and moves it to gen/
scanner:
	@echo "\n====================================================="	
	@echo "Creating scanner.ml ..."
	$(LEXGEN) $(SRCDIR)/scanner.mll
	@mv $(SRCDIR)/scanner.ml $(GENDIR)/scanner.ml
	@echo "=====================================================\n"	

# creates ast.cmi and ast.cmp and moves them to gen/
asttypes:
	@echo "\n====================================================="	
	@echo "Compiling AST types (ast.cmi and ast.cmo) ..."
	$(OCC) $(OCCFLAGS1) $(SRCDIR)/ast.ml
	@mv $(SRCDIR)/ast.cmi $(GENDIR)/ast.cmi
	@mv $(SRCDIR)/ast.cmo $(GENDIR)/ast.cmo
	@echo "=====================================================\n"

# creates parser.ml and parser.mli and moves them to gen/
parser: 
	@echo "\n====================================================="	
	@echo "Creating parser.ml and parser.mli ..."
	$(PARSGEN) $(PARSGENFLAG) $(SRCDIR)/parser.mly
	@mv $(SRCDIR)/parser.ml $(GENDIR)/parser.ml
	@mv $(SRCDIR)/parser.mli $(GENDIR)/parser.mli
	@mv $(SRCDIR)/parser.output $(GENDIR)/parser.output
	@cat $(GENDIR)/parser.output
	@echo "=====================================================\n"

test:
	@echo "\n====================================================="	
	@echo "Running test script ..."
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(GENDIR)/$(TESTSH).log
	@echo "\n====================================================="
	@echo "Opening $(TESTSH).sh log ..."
	@cat $(GENDIR)/$(TESTSH).log
	@echo "=====================================================\n"

# removes all files in gen/
clean:
	@echo "\n====================================================="	
	@echo "Cleaning up auxiliary files ..."
	rm -rf $(GENDIR)/*
	@echo "=====================================================\n"









# ======================================================== #
# Following targets are for tests
#$(LEXGEN) $(SRCDIR)/scanner.mll
#$(OCC) $(OCCFLAGS2) scanner $(SRCDIR)/scanner.ml ; \
#	@for file in $(testfiles) ; do \
#		./scanner < "$$file" ; \
#	done
scannertest: scanner
	@echo "\n====================================================="	
	@echo "Testing scanner ..."
	ocamlc -o scannertest src/scanner.ml
	./scannertest < src/scanner.mll
	@echo "=====================================================\n"	

# ======================================================== #
# Following commands are not completed.
# Need to see what kind of file extensions the generated
# files have, then we can move to appropriate directories.
# ======================================================== #

parsertypes: 
	@echo "\n====================================================="	
	@echo "Compiling Parser types ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/parser.mli
	@echo "=====================================================\n"

scannercomp: 
	@echo "\n====================================================="	
	@echo "Compiling the Scanner ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/scanner.ml
	@echo "=====================================================\n"

parsercomp:
	@echo "\n====================================================="	
	@echo "Compiling the Parser ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/parser.ml
	@echo "=====================================================\n"	

bloxcomp: 
	@echo "\n====================================================="	
	@echo "Compiling the Blox compiler ..."
	$(OCC) $(OCCFLAGS2) $(EXEC) $(AMF)/$(EXEC).
	@echo "=====================================================\n"

