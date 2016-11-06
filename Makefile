# phony targets
.PHONY: all clean scanner parser test asttypes parsertypes 
	parsercomp scannercomp bloxcomp scannertest parsertest

# name of output file
EXEC = blox_exec.amf

# bytecode object file extension
OBJEXT = .cmo

# lexer(scanner) and parser generators
LEXGEN = ocamllex
PARSGEN = ocamlyacc

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


#$(LEXGEN) $(SRCDIR)/scanner.mll
#$(OCC) $(OCCFLAGS2) scanner $(SRCDIR)/scanner.ml ; \
#	@for file in $(testfiles) ; do \
#		./scanner < "$$file" ; \
#	done
scannertest: scanner asttypes parser 
	@echo "\n============================================="	
	@echo "Testing scanner ..."
	ocamllex src/scanner.mll
	ocamlc -o scanner src/scanner.ml
	./scanner < src/scanner.mll
	@echo "=============================================\n"	

scanner:
	@echo "\n============================================="	
	@echo "Creating scanner.ml ..."
	$(LEXGEN) $(SRCDIR)/scanner.mll
	@mv $(SRCDIR)/scanner.ml $(GENDIR)/scanner.ml
	@echo "=============================================\n"	

parser: 
	@echo "\n============================================="	
	@echo "Creating parser.ml and parser.mli ..."
	$(PARSGEN) $(SRCDIR)/parser.mly
	@mv $(SRCDIR)/parser.ml $(GENDIR)/parser.ml
	@mv $(SRCDIR)/parser.mli $(GENDIR)/parser.mli
	@echo "=============================================\n"

asttypes:
	@echo "\n============================================="	
	@echo "Compiling AST types ..."
	$(OCC) $(OCCFLAGS1) $(SRCDIR)/ast.ml
	@mv $(SRCDIR)/ast.cmi $(GENDIR)/ast.cmi
	@mv $(SRCDIR)/ast.cmo $(GENDIR)/ast.cmo
	@echo "=============================================\n"

clean:
	@echo "\n============================================="	
	@echo "Cleaning up auxiliary files ..."
	@rm -rf $(GENDIR)/*
	@echo "=============================================\n"


# ======================================================== #
# Following commands are not completed.
# Need to see what kind of file extensions the generated
# files have, then we can move to appropriate directories.
# ======================================================== #

parsertypes: 
	@echo "\n============================================="	
	@echo "Compiling Parser types ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/parser.mli
	@echo "=============================================\n"

scannercomp: 
	@echo "\n============================================="	
	@echo "Compiling the Scanner ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/scanner.ml
	@echo "=============================================\n"

parsercomp:
	@echo "\n============================================="	
	@echo "Compiling the Parser ..."
	$(OCC) $(OCCFLAGS1) $(GENDIR)/parser.ml
	@echo "=============================================\n"	

bloxcomp: 
	@echo "\n============================================="	
	@echo "Compiling the Blox compiler ..."
	$(OCC) $(OCCFLAGS2) $(EXEC) $(AMF)/$(EXEC).
	@echo "=============================================\n"

