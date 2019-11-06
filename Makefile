CMO=lexer.cmo parser.cmo main.cmo #interp.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=pgoc
TEST=./test.sh
FLAGS=

all: $(BIN)
	./$(BIN) test.logo

.PHONY: test_syntax #test_typing test_exec

test_syntax: $(BIN)

$(BIN): $(CMO)
	ocamlc $(FLAGS) -o $(BIN) nums.cma $(CMO) #zarith.cma

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


