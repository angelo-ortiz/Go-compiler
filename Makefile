CMO=lexer.cmo parser.cmo interp.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=pgoc
FLAGS=

all: $(BIN)
	./$(BIN) test.logo

.PHONY: test_syntax #test_typing test_exec

test_syntax: $(BIN)
	for f in tests/syntax/bad/*.go; do ./$(BIN) --parse-only $$f; if [ $? -neq 1 ]; then FAIL; fi; done
	for f in tests/syntax/good/*.go; do ./$(BIN) --parse-only $$f; if [ $? -neq 0 ]; then FAIL; fi; done
	for f in tests/typing/*/*.go; do ./$(BIN) --parse-only $$f; if [ $? -neq 0 ]; then FAIL; fi; done
	for f in tests/exec*/*.go; do ./$(BIN) --parse-only $$f; if [ $? -neq 0 ]; then FAIL; fi; done

test_typing: $(BIN)
	for f in tests/typing/bad/*.go; do ./$(BIN) --type-only $$f; if [ $? -neq 1 ]; then FAIL; fi; done
	for f in tests/typing/good/*.go; do ./$(BIN) --type-only $$f; if [ $? -neq 0 ]; then FAIL; fi; done
	for f in tests/exec*/*.go; do ./$(BIN) --type-only $$f; if [ $? -neq 0 ]; then FAIL; fi; done

test_exec: $(BIN)
	for f in tests/exec-fail/*.go; do ./$(BIN) $$f; if [ $? -neq 1 ]; then FAIL; fi; done
	for f in tests/exec/*.go; do ./$(BIN) $$f; if [ $? -neq 0 ]; then FAIL; fi; done

$(BIN): $(CMO)
	ocamlc $(FLAGS) -o $(BIN) zarith.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.automaton

parser.ml: ast.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



