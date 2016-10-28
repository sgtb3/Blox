# phony targets
.PHONY: all clean scanner parser test asttypes parsertypes parsercomp scannercomp bloxcomp

# lexer and parser generators
LEXGEN = ocamllex
PARSGEN = ocamlyacc

# ocaml compiler and flags
OCC = ocamlc
OCFLAGS = -c

# source files, generated files, testing, and AMF/output directories
SRCDIR = src
GENDIR = gen
TSTDIR = tests
AMFDIR = amf

# default makefile target
all: scanner 

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
	$(OCC) $(OCFLAGS) $(SRCDIR)/ast.ml
	@mv $(SRCDIR)/ast.mli $(GENDIR)/ast.mli
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
	$(OCC) $(OCFLAGS) $(GENDIR)/parser.mli
	@echo "=============================================\n"

scannercomp: 
	@echo "\n============================================="	
	@echo "Compiling the Scanner ..."
	$(OCC) $(OCFLAGS) $(GENDIR)/parser.ml
	@echo "=============================================\n"

bloxcomp: 
	@echo "\n============================================="	
	@echo "Compiling the Blox compiler ..."
	$(OCC) $(OCFLAGS) $(GENDIR)/blox.ml
	@echo "=============================================\n"

