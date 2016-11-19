# phony targets
.PHONY: all clean scanner parser ast sast test menhirtest bloxcomp scancomp parsercomp

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
OCCFLAGS3 = -absname

# source files, generated files, testing, and AMF/output directories
SRCDIR = src
GENDIR = gen
OBJDIR = obj
TSTDIR = tests
AMFDIR = amf

VPATH = src:gen:obj
testfiles := $(wildcard $(TSTDIR)/*)

# default makefile target
all: clean scanner ast parser scancomp parsercomp

# creates scanner.ml and moves it to gen/
scanner:
	@echo "\n====================================================="	
	@echo "Creating scanner.ml ..."
	$(LEXGEN) $(SRCDIR)/scanner.mll
	@mv $(SRCDIR)/scanner.ml $(GENDIR)/scanner.ml
	@echo "=====================================================\n"	

# creates ast compiled interface (ast.cmi) and ast compiled object code 
# (ast.cmo) files and moves them to the appropriate directories
ast:
	@echo "\n====================================================="	
	@echo "Compiling AST (ast.cmi and ast.cmo) ..."
	$(OCC) $(OCCFLAGS1) $(SRCDIR)/ast.ml
	@mv $(SRCDIR)/ast.cmi $(GENDIR)/ast.cmi
	@mv $(SRCDIR)/ast.cmo $(OBJDIR)/ast.cmo
	@echo "=====================================================\n"

# creates parser source code (parser.ml), parser module interface 
# (parser.mli), and parser generator verbose output files and moves them 
# to appropriate directory
parser: 
	@echo "\n====================================================="	
	@echo "Creating parser.ml and parser.mli ..."
	$(PARSGEN) $(PARSGENFLAG) $(SRCDIR)/parser.mly
	@mv $(SRCDIR)/parser.ml $(GENDIR)/parser.ml
	@mv $(SRCDIR)/parser.mli $(GENDIR)/parser.mli
	#@mv $(SRCDIR)/parser.output $(GENDIR)/parser.output
	#@cat $(GENDIR)/parser.output
	@echo "=====================================================\n"

scancomp:
	@echo "\n====================================================="	
	@echo "Compiling the Scanner ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/scanner.ml
	@mv $(GENDIR)/scanner.cmo $(OBJDIR)/scanner.cmo 
	@echo "=====================================================\n"

parsercomp:
	@echo "\n====================================================="	
	@echo "Compiling the Parser ..."
	$(OCC) -I $(GENDIR) $(OCCFLAGS1) $(GENDIR)/parser.ml
	@mv $(GENDIR)/parser.cmo $(OBJDIR)/parser.cmo 
	@echo "=====================================================\n"

# runs the test script and displays the test log
test:
	@echo "\n====================================================="	
	@echo "Running test script ..."
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(GENDIR)/$(TESTSH).log
	@echo "\n-----------------------------------------------------"
	@echo "Opening $(TESTSH).sh log ..."
	@cat $(GENDIR)/$(TESTSH).log
	@echo "=====================================================\n"

# runs the menhir's interpreter as an alternative debugging tool
menhirtest:
	menhir --interpret --interpret-show-cst $(SRCDIR)/parser.mly

# removes all files in gen/ and obj/
clean:
	@echo "\n====================================================="	
	@echo "Cleaning up auxiliary files ..."
	@rm -rf $(GENDIR)/*
	@rm -rf $(OBJDIR)/*
	@echo "=====================================================\n"

# not yet complete
bloxcomp: 
	@echo "\n====================================================="	
	@echo "Compiling the Blox compiler ..."
	$(OCC) $(OCCFLAGS2) $(EXEC) $(AMF)/$(EXEC).
	@echo "=====================================================\n"
