.PHONY:	Clean Build Demo AST-Test Compile-Test Run-Menhir-Test Blox.tar.gz test\
	Compiler Test-AST 

TESTSH   = testsuite
FILEIN   = HelloWorld
EXEC     = blox.native
SRC      = src
GEN      = junk
OBJ      = junk
TST      = tests
AMF      = amf
CLIB     = bloxlib

TARFILES = $(SRC) $(TST) $(TESTSH).sh README.md Makefile $(TESTFILES:%=tests/%)

OK_STR   = \033[32;01m[OK]\033[0m
SUC_STR  = \033[32;01m[BUILD-SUCCESSFUL]\033[0m
PRT_CMD  = awk '{ printf "\n%-20s %-10s\n",$$1, $$2; }'
PRT_OK   = printf "$@ $(OK_STR)"  | $(PRT_CMD)
BLD_OK   = printf "$@ $(SUC_STR)" | $(PRT_CMD)

# default target: build using ocamlbuild
Build:
	@clang-3.8 -c -emit-llvm $(SRC)/$(CLIB).c # clang-3.8
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
	$(SRC)/$(EXEC)
	@$(BLD_OK)
	
Clean:
	@rm -rf $(SRC)/*.o $(SRC)/*.cm* $(EXEC) demo *.amf _build build_results \
	*.bc *.diff *.out *.ll *.err testsuite.log *.c.out fail* $(GEN)/parser.output
	@$(PRT_OK)

AST-Test:
	@echo "[$(FILEIN).blox:]\n"
	@./$(EXEC) -a $(SRC)/$(FILEIN).blox
	@$(PRT_OK)

IR-Test:
	@echo "[$(FILEIN).blox:]\n"
	@./$(EXEC) -i $(SRC)/$(FILEIN).blox
	@$(PRT_OK)

Compile-Test:
	@echo "[$(FILEIN).blox:]\n"
	@./$(EXEC) -c $(SRC)/$(FILEIN).blox
	@$(PRT_OK)

Run-Menhir-Test:
	menhir --interpret --interpret-show-cst $(SRC)/parser.mly
	@$(PRT_OK)

Test-Suite:
	@rm -rf tests/logs/*
	@./$(TESTSH).sh
	@mv *.ll $(TST)/logs/
	@mv *.err $(TST)/logs/
	@mv *.diff $(TST)/logs/
	@#mv *.out $(TST)/logs/
	@#mv $(TESTSH).log $(TST)/$(TESTSH).log
	@#echo "\n[$(TESTSH).sh log:]"
	@#cat $(TESTSH).log
	@#~/.vim/bundle/vimpager/vimcat $(TESTSH).log
	@$(PRT_OK)

Demo:	Clean
	@echo "\n[demo.blox:]\n"
	@ocamlc -o demo $(SRC)/exedemo.ml
	@./demo
	@echo "\n[$(TESTSH).sh log:]\n"
	@cat Result.amf
	@$(PRT_OK)

Blox.tar.gz:	$(TARFILES) Clean
	@cd .. && tar czf Blox/Blox.tar.gz $(TARFILES:%=Blox/%)
	@$(PRT_OK)



############## TESTING ONLY - REMOVE ##############

.PHONY:	Compiler Compile-AST Compile-Printer \
	Create-Scanner Create-Parser Compile-Scanner Compile-Parser \
	Compile-Analyzer Compile-Internal Compile-Generator \
	Compile-Blox Link-Objects

LEXGEN  = ocamllex
PARSGEN = ocamlyacc -v
OCC1    = ocamlc -c
OCC2    = ocamlc -o

#Compile-Generator
Compiler: Clean Compile-AST Compile-Printer \
	Create-Scanner Create-Parser Compile-Scanner Compile-Parser \
	Compile-Analyzer Compile-Internal \
	Compile-Blox Link-Objects

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
Compile-Internal:
	@$(OCC1) -I $(GEN) $(SRC)/internal.ml
	@mv $(SRC)/internal.cmo $(OBJ)/internal.cmo
	@mv $(SRC)/internal.cmi $(GEN)/internal.cmi
	@$(PRINT_OK)
Compile-Generator:
	@$(OCC1) -I $(GEN) $(SRC)/codegen.ml
	@mv $(SRC)/codegen.cmo $(OBJ)/codegen.cmo
	@mv $(SRC)/codegen.cmi $(GEN)/codegen.cmi
	@$(PRINT_OK)
Compile-Blox:
	@$(OCC1) -I $(GEN) $(SRC)/blox.ml
	@mv $(SRC)/blox.cmo $(OBJ)/blox.cmo
	@mv $(SRC)/blox.cmi $(GEN)/blox.cmi
	@$(PRINT_OK)

#$(OBJ)/codegen.cmo 
Link-Objects:
	@$(OCC2) blox $(OBJ)/ast.cmo $(OBJ)/scanner.cmo $(OBJ)/parser.cmo \
	$(OBJ)/pprint.cmo $(OBJ)/analyzer.cmo $(OBJ)/internal.cmo \
	$(OBJ)/blox.cmo
	@$(BUILD_OK)
	@echo "\n-------------------------------------------------------\n"
