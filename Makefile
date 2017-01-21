.PHONY:	Clean Build Demo AST-Test Compile-Test Run-Menhir-Test Blox.tar.gz

TARFILES = $(SRC) $(TST) $(TESTSH).sh README.md Makefile $(TESTFILES:%=tests/%)

OK_STR   = \033[32;01m[OK]\033[0m
SUC_STR  = \033[32;01m[BUILD-SUCCESSFUL]\033[0m
PRT_CMD  = awk '{ printf "\n%-20s %-10s\n",$$1, $$2; }'
PRT_OK   = printf "$@ $(OK_STR)"  | $(PRT_CMD)
BLD_OK   = printf "$@ $(SUC_STR)" | $(PRT_CMD)

TESTSH   = testAll
FILEIN   = HelloWorld
EXEC     = blox.native
SRC      = src
TST      = tests
AMF      = amf

# default target: build using ocamlbuild
Build: Clean
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 $(SRC)/$(EXEC)
	@$(BLD_OK)
	
Clean:
	@rm -rf $(SRC)/*.o $(SRC)/*.cm* $(EXEC) demo *.amf _build build_results
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

Run-Test-Script:
	@./$(TESTSH).sh
	@mv $(TESTSH).log $(TST)/$(TESTSH).log
	@echo "[$(TESTSH).sh log:]"
	@cat $(TST)/$(TESTSH).log
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
