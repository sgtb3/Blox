# phony targets
.PHONY: all clean scanner parser ast sast scancomp parscomp anlyzcomp gencomp bloxcomp build test menhirtest 

# name of blox binary file
EXEC = blox

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
OCCFLAGS3 = -absname

# source files, generated files, testing, and AMF/output directories
SRCDIR = src
GENDIR = gen
OBJDIR = obj
TSTDIR = tests
AMFDIR = amf

# add src, gen, and obj directories to Make path when looking for files
VPATH = src:gen:obj
testfiles := $(wildcard $(TSTDIR)/*)

# default makefile target
all: clean scanner ast parser sast scancomp parscomp anlyzcomp gencomp bloxcomp build

# create the Scanner
scanner:
	@echo "\n====================================================="	
	@echo "Using ocamllex to generate the Scanner ..."
	$(LEXGEN) $(SRCDIR)/scanner.mll
	@mv $(SRCDIR)/scanner.ml $(GENDIR)/scanner.ml
	@echo "=====================================================\n"	

# create the Parser
parser: 
	@echo "\n====================================================="	
	@echo "Using ocamlyacc to generate the Parser ..."
	$(PARSGEN) $(PARSGENFLAG) $(SRCDIR)/parser.mly
	@mv $(SRCDIR)/parser.ml $(GENDIR)/parser.ml
	@mv $(SRCDIR)/parser.mli $(GENDIR)/parser.mli
	@mv $(SRCDIR)/parser.output $(GENDIR)/parser.output
	@cat $(GENDIR)/parser.output
	@echo "=====================================================\n"

# compile the Abstract Syntax Tree 
ast:
	@echo "\n====================================================="	
	@echo "Compiling the Abstract Syntax Tree ..."
	$(OCC) $(OCCFLAGS1) $(SRCDIR)/ast.ml
	@mv $(SRCDIR)/ast.cmi $(GENDIR)/ast.cmi
	@mv $(SRCDIR)/ast.cmo $(OBJDIR)/ast.cmo
	@echo "=====================================================\n"


# creates the sematically checked AST (sast). 
# scanner, ast, and parser should already before created making sast
sast:
	@echo "\n====================================================="	
	@echo "Creating SAST ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(SRCDIR)/sast.ml
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/parser.mli
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/scanner.ml
	@mv $(SRCDIR)/sast.cmi $(GENDIR)/sast.cmi
	@mv $(SRCDIR)/sast.cmo $(OBJDIR)/sast.cmo
	@echo "=====================================================\n"

# compile the Scanner
scancomp:
	@echo "\n====================================================="	
	@echo "Compiling the Scanner ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/scanner.ml
	@mv $(GENDIR)/scanner.cmo $(OBJDIR)/scanner.cmo 
	@echo "=====================================================\n"

# compile the Parser
parscomp:
	@echo "\n====================================================="	
	@echo "Compiling the Parser ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/parser.ml
	@mv $(GENDIR)/parser.cmo $(OBJDIR)/parser.cmo 
	@echo "=====================================================\n"

# compile the Semantic Analyzer
anlyzcomp:
	@echo "\n====================================================="	
	@echo "Compiling the Semantic Analyzer ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(SRCDIR)/analyzer.ml
	@mv $(SRCDIR)/analyzer.cmo $(OBJDIR)/analyzer.cmo 
	@mv $(SRCDIR)/analyzer.cmi $(OBJDIR)/analyzer.cmi
	@echo "=====================================================\n"

# compiler the Code Generator
gencomp:
	@echo "\n====================================================="	
	@echo "Compiling the Code Generator ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(SRCDIR)/generator.ml
	@mv $(SRCDIR)/generator.cmo $(OBJDIR)/generator.cmo 
	@echo "=====================================================\n"

# not yet complete
bloxcomp: 
	@echo "\n====================================================="	
	@echo "Compiling the Blox top-level ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(SRCDIR)/blox.ml
	@mv $(SRCDIR)/blox.cmo $(OBJDIR)/blox.cmo 
	@echo "=====================================================\n"

build:
	@echo "\n====================================================="	
	@echo "Creating the Blox compiler ..."
	$(OCC) $(OCCFLAGS2) $(EXEC) \
	$(OBJDIR)/parser.cmo \
	$(OBJDIR)/scanner.cmo \
	$(OBJDIR)/ast.cmo \
	$(OBJDIR)/sast.cmo \
	$(OBJDIR)/analyzer.cmo \
	$(OBJDIR)/generator.cmo \
	$(OBJDIR)/blox.cmo 
	@echo "=====================================================\n"

# run the test script and display the test log
test:
	@echo "\n====================================================="	
	@echo "Running test script ..."
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(GENDIR)/$(TESTSH).log
	@echo "\n-----------------------------------------------------"
	@echo "Opening $(TESTSH).sh log ..."
	@cat $(GENDIR)/$(TESTSH).log
	@echo "=====================================================\n"

# run menhir's interpreter to show concrete syntax tree
# enter token identifiers to see if they're accepted or rejected
# example of "Frame<1,1,3> A;" : ID LT INT COMMA INT COMMA INT GT ID SEMI EOF
menhirtest:
	menhir --interpret --interpret-show-cst $(SRCDIR)/parser.mly
	
# remove all files in gen/ and obj/
clean:
	@echo "\n====================================================="	
	@echo "Cleaning up auxiliary files ..."
	@rm -rf $(GENDIR)/*
	@rm -rf $(OBJDIR)/*
	@echo "=====================================================\n"
