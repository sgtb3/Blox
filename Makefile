.PHONY: Create-Scanner Create-Parser Create-AST \
		Create-SAST Compile-Scanner \
		Compile-Parser Compile-Analyzer Compile-Generator Compile-Executor \ 
		Compile-Blox Link-Objects \
		AST-Test Executor-Test Compile-Test \
		Run-Menhir-Test Blox.tar.gz compiler

# name of blox binary file
EXEC = blox

#name of HelloWorld testfile
HELLO = HelloWorld

# name of test shell script
TESTSH = testAll

# lexer (scanner) and parser generators
# -v : verbose output option for ocamlyacc
LEXGEN = ocamllex
PARSGEN = ocamlyacc -v 

# ocaml compiler and flags
# -c : compile without producing executable files
# -o : specify name of output file produced by compiler
OCC1 = ocamlc -c
OCC2 = ocamlc -o

# source files, generated files, testing, and AMF/output directories
SRC = src
GEN = gen
OBJ = obj
TST = tests
AMF = amf
TARFILES = $(SRC) $(GEN) $(OBJ) $(TST) $(TESTSH).sh README.md Makefile \
		   $(TESTFILES:%=tests/%)

# add src, gen, and obj directories to Make path when looking for files
VPATH = src:gen:obj
testfiles := $(wildcard $(TST)/*)

NO_COLOR   = \033[0m
OK_COLOR   = \033[32;01m
OK_STR     = $(OK_COLOR)[OK]$(NO_COLOR)
SUC_STR    = $(OK_COLOR)[BUILD-SUCCESSFUL]$(NO_COLOR)
AWK_CMD    = awk '{ printf "\n%-50s %-10s\n",$$1, $$2; }'
PRINT_OK   = printf "$@ $(OK_STR)"  | $(AWK_CMD)
BUILD_OK   = printf "$@ $(SUC_STR)" | $(AWK_CMD)

compiler:	Clean Create-Scanner Create-AST Create-Parser Create-SAST \
		Compile-Scanner Compile-Parser Compile-Analyzer Compile-Executor \
		Compile-Generator Compile-Blox Link-Objects  

Create-Scanner:
	@$(LEXGEN) $(SRC)/scanner.mll
	@mv $(SRC)/scanner.ml $(GEN)/scanner.ml
	@sleep .12
	@$(PRINT_OK)

Create-Parser:
	@$(PARSGEN) $(SRC)/parser.mly
	@mv $(SRC)/parser.ml $(GEN)/parser.ml
	@mv $(SRC)/parser.mli $(GEN)/parser.mli
	@mv $(SRC)/parser.output $(GEN)/parser.output
	@cat $(GEN)/parser.output
	@sleep .12
	@$(PRINT_OK)

Create-AST:
	@$(OCC1) $(SRC)/ast.ml 
	@mv $(SRC)/ast.cmi $(GEN)/ast.cmi
	@mv $(SRC)/ast.cmo $(OBJ)/ast.cmo
	@sleep .12
	@$(PRINT_OK)

Create-SAST:
	@$(OCC1) -I $(GEN) $(SRC)/sast.ml $(GEN)/parser.mli $(GEN)/scanner.ml
	@mv $(SRC)/sast.cmi $(GEN)/sast.cmi
	@mv $(SRC)/sast.cmo $(OBJ)/sast.cmo
	@sleep .12
	@$(PRINT_OK)

Compile-Scanner:
	@$(OCC1) -I $(GEN) $(GEN)/scanner.ml
	@mv $(GEN)/scanner.cmo $(OBJ)/scanner.cmo
	@sleep .12
	@$(PRINT_OK)

Compile-Parser:
	@$(OCC1) -I $(GEN) $(GEN)/parser.ml
	@mv $(GEN)/parser.cmo $(OBJ)/parser.cmo
	@sleep .12
	@$(PRINT_OK)

Compile-Analyzer:
	@$(OCC1) -I $(GEN) $(SRC)/analyzer.ml
	@mv $(SRC)/analyzer.cmo $(OBJ)/analyzer.cmo
	@mv $(SRC)/analyzer.cmi $(GEN)/analyzer.cmi
	@sleep .12
	@$(PRINT_OK)

Compile-Executor:
	@$(OCC1) -I $(GEN) $(SRC)/executor.ml
	@mv $(SRC)/executor.cmo $(OBJ)/executor.cmo
	@mv $(SRC)/executor.cmi $(GEN)/executor.cmi
	@sleep .12
	@$(PRINT_OK)

Compile-Generator:
	@$(OCC1) -I $(GEN) $(SRC)/generator.ml
	@mv $(SRC)/generator.cmo $(OBJ)/generator.cmo
	@mv $(SRC)/generator.cmi $(GEN)/generator.cmi
	@sleep .12
	@$(PRINT_OK)

Compile-Blox:
	@$(OCC1) -I $(GEN) $(SRC)/blox.ml
	@mv $(SRC)/blox.cmo $(OBJ)/blox.cmo
	@mv $(SRC)/blox.cmi $(GEN)/blox.cmi
	@sleep .12
	@$(PRINT_OK)

Link-Objects:
	@$(OCC2) $(EXEC) $(OBJ)/parser.cmo $(OBJ)/scanner.cmo $(OBJ)/ast.cmo $(OBJ)/sast.cmo \
	$(OBJ)/analyzer.cmo $(OBJ)/executor.cmo $(OBJ)/generator.cmo $(OBJ)/blox.cmo 
	@$(BUILD_OK)
	@echo "\n-------------------------------------------------------\n"
	@sleep .8

AST-Test: compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -e $(SRC)/$(HELLO).blox
	@sleep .12
	@$(PRINT_OK)

Executor-Test: compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -e $(SRC)/$(HELLO).blox
	@sleep .12
	@$(PRINT_OK)

Compile-Test: compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -c $(SRC)/$(HELLO).blox
	@sleep .12
	@$(PRINT_OK)

Run-Test-Script:
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(GEN)/$(TESTSH).log
	@sleep .12
	@echo "[Opening $(TESTSH).sh log ...]"
	@cat $(GEN)/$(TESTSH).log
	@sleep .12
	@$(PRINT_OK)

Run-Menhir-Test:
	menhir --interpret --interpret-show-cst $(SRC)/parser.mly
	@$(PRINT_OK)

Clean:
	@echo "\n-------------------------------------------------------\n"
	@rm -rf $(GEN)/*
	@rm -rf $(OBJ)/*
	@rm -rf $(EXEC)
	@sleep .12
	@$(PRINT_OK)

Blox.tar.gz : $(TARFILES) Clean
	@cd .. && tar czf Blox/Blox.tar.gz $(TARFILES:%=Blox/%)
	@$(PRINT_OK)
