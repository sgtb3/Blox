.PHONY: all clean scanner parser ast

# lexer generator
LEXGEN = ocamllex

# parser generator
PARSGEN = ocamlyacc

# ocaml compiler
OCC = ocamlc

# ocaml compiler flags
OCFLAGS = -c

# source file directory
SRCDIR = src

# testing directory
TSTDIR = tests

# default makefile target
all: scanner 

# create scanner.ml
scanner:
	@echo "\n============================================="	
	@echo "Creating scanner.ml ..."
	$(LEXGEN) $(SRCDIR)/scanner.mll
	@echo "=============================================\n"	

# create parser.ml and parser.mli
parser: 
	@echo "\n============================================="	
	@echo "Creating parser.ml and parser.mli ..."
	$(PARSGEN) $(SRCDIR)/parser.mly
	@echo "=============================================\n"

# create parser.ml and parser.mli
parser: 
	@echo "\n============================================="	
	@echo "Creating parser.ml and parser.mli ..."
	$(OCC) $(SRCDIR)/parser.mly
	@echo "=============================================\n"

# clean up all auxiliary files
clean:
	@echo "\n============================================="	
	@echo "Cleaning up ..."
	@rm -rf $(SRCDIR)/*scanner.ml
	@echo "=============================================\n"








