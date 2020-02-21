CMO=asg.cmo utils.cmo lexer.cmo parser.cmo type_checker.cmo \
	label.cmo register.cmo is.cmo rtl.cmo ertl.cmo liveness.cmo interference.cmo colouring.cmo \
	ltl.cmo x86_64.cmo unionFind.cmo branch.cmo lin.cmo pretty_printer.cmo pp.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=pgoc
TEST=./test.sh
FLAGS=-I +zarith

all: $(BIN) test_all

.PHONY: test_syntax test_typing test_compil

test_syntax: $(BIN)
	cd tests && $(TEST) -1 ../$(BIN)

test_typing: $(BIN)
	cd tests && $(TEST) -2 ../$(BIN)

test_compil: $(BIN)
	cd tests && $(TEST) -3 ../$(BIN)

test_all: $(BIN)
	cd tests && $(TEST) -all ../$(BIN)

$(BIN): $(CMO)
	ocamlfind ocamlc -package zarith -linkpkg -g -o $(BIN) $(CMO)

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

parser.ml: ast.cmi utils.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend


