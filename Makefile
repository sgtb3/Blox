.PHONY:	Clean Compiler Demo AST-Test Blox.tar.gz \
		AST-Test IR-Test Compile-Test Run-Menhir-Test \
		Compile-AST Compile-Printer \
		Create-Scanner Create-Parser Compile-Scanner Compile-Parser \
		Compile-Analyzer Compile-IR Compile-Transltor Compile-Executor \
		Compile-Generator Compile-Blox Link-Objects

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

Compiler:	Clean Compile-AST Compile-Printer \
			Create-Scanner Create-Parser Compile-Scanner Compile-Parser \
			Compile-Analyzer Compile-IR Compile-Transltor Compile-Executor \
			Compile-Generator Compile-Blox Link-Objects

Compile-AST:
	@$(OCC1) $(SRC)/ast.ml
	@mv $(SRC)/ast.cmi $(GEN)/ast.cmi
	@mv $(SRC)/ast.cmo $(OBJ)/ast.cmo
	@$(PRINT_OK)

Compile-Printer:
	@$(OCC1) -I $(GEN) $(SRC)/pprint.ml
	@mv $(SRC)/pprint.cmo $(OBJ)/pprint.cmo
	@mv $(SRC)/pprint.cmi $(GEN)/pprint.cmi
	@$(PRINT_OK)

Create-Scanner:
	@$(LEXGEN) $(SRC)/scanner.mll
	@mv $(SRC)/scanner.ml $(GEN)/scanner.ml
	@$(PRINT_OK)

Create-Parser:
	@$(PARSGEN) $(SRC)/parser.mly
	@mv $(SRC)/parser.ml $(GEN)/parser.ml
	@mv $(SRC)/parser.mli $(GEN)/parser.mli
	@mv $(SRC)/parser.output $(GEN)/parser.output
	@#cat $(GEN)/parser.output
	@$(PRINT_OK)

Compile-Scanner:
	@$(OCC1) -I $(GEN) $(GEN)/parser.mli $(GEN)/scanner.ml
	@mv $(GEN)/scanner.cmo $(OBJ)/scanner.cmo
	@$(PRINT_OK)

Compile-Parser:
	@$(OCC1) -I $(GEN) $(GEN)/parser.ml
	@mv $(GEN)/parser.cmo $(OBJ)/parser.cmo
	@$(PRINT_OK)

Compile-Analyzer:
	@$(OCC1) -I $(GEN) $(SRC)/analyzer.ml
	@mv $(SRC)/analyzer.cmo $(OBJ)/analyzer.cmo
	@mv $(SRC)/analyzer.cmi $(GEN)/analyzer.cmi
	@$(PRINT_OK)

Compile-IR:
	@$(OCC1) -I $(GEN) $(SRC)/ir.ml
	@mv $(SRC)/ir.cmi $(GEN)/ir.cmi
	@mv $(SRC)/ir.cmo $(OBJ)/ir.cmo
	@$(PRINT_OK)

Compile-Transltor:
	@$(OCC1) -I $(GEN) $(SRC)/translator.ml
	@mv $(SRC)/translator.cmi $(GEN)/translator.cmi
	@mv $(SRC)/translator.cmo $(OBJ)/translator.cmo
	@$(PRINT_OK)

Compile-Executor:
	@$(OCC1) -I $(GEN) $(SRC)/executor.ml
	@mv $(SRC)/executor.cmo $(OBJ)/executor.cmo
	@mv $(SRC)/executor.cmi $(GEN)/executor.cmi
	@$(PRINT_OK)

Compile-Generator:
	@$(OCC1) -I $(GEN) $(SRC)/generator.ml
	@mv $(SRC)/generator.cmo $(OBJ)/generator.cmo
	@mv $(SRC)/generator.cmi $(GEN)/generator.cmi
	@$(PRINT_OK)

Compile-Blox:
	@$(OCC1) -I $(GEN) $(SRC)/blox.ml
	@mv $(SRC)/blox.cmo $(OBJ)/blox.cmo
	@mv $(SRC)/blox.cmi $(GEN)/blox.cmi
	@$(PRINT_OK)

Link-Objects:
	@$(OCC2) $(EXEC) $(OBJ)/ast.cmo $(OBJ)/scanner.cmo $(OBJ)/parser.cmo \
	$(OBJ)/pprint.cmo $(OBJ)/analyzer.cmo $(OBJ)/ir.cmo $(OBJ)/translator.cmo \
	$(OBJ)/executor.cmo $(OBJ)/generator.cmo $(OBJ)/blox.cmo
	@$(BUILD_OK)
	@echo "\n-------------------------------------------------------\n"

AST-Test:	Compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -a $(SRC)/$(HELLO).blox
	@$(PRINT_OK)

IR-Test:	Compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -i $(SRC)/$(HELLO).blox
	@$(PRINT_OK)

Compile-Test:	Compiler
	@echo "[$(HELLO).blox:]\n"
	@./$(EXEC) -c $(SRC)/$(HELLO).blox
	@$(PRINT_OK)

Run-Test-Script:
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(GEN)/$(TESTSH).log
	@sleep .3
	@echo "[Opening $(TESTSH).sh log ...]"
	@cat $(GEN)/$(TESTSH).log
	@$(PRINT_OK)

Run-Menhir-Test:
	menhir --interpret --interpret-show-cst $(SRC)/parser.mly
	@$(PRINT_OK)

Demo:	Clean
	@ocamlc -o demo src/exedemo.ml
	@mv src/exedemo.cmi gen/
	@mv src/exedemo.cmo obj/
	@./demo
	@$(PRINT_OK)

Clean:
	@rm -rf $(GEN)/* $(OBJ)/* $(EXEC) demo *.amf
	@$(PRINT_OK)

Blox.tar.gz :	$(TARFILES) Clean
	@cd .. && tar czf Blox/Blox.tar.gz $(TARFILES:%=Blox/%)
	@$(PRINT_OK)
