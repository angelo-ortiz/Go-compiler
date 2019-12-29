CMO=asg.cmo utils.cmo lexer.cmo parser.cmo type_checker.cmo is.cmo rtl.cmo ertl.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=pgoc
TEST=./test.sh
FLAGS=

all: $(BIN) test_typing
	#cd tests && $(TEST) -all ../$(BIN)

.PHONY: test_syntax test_typing test_compil

test_syntax: $(BIN)
	cd tests && $(TEST) -1 ../$(BIN)

test_typing: $(BIN)
	cd tests && $(TEST) -2 ../$(BIN)

test_compil: $(BIN)
	cd tests && $(TEST) -3 ../$(BIN)

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

parser.ml: ast.cmi utils.cmi
ltl.cmo: colouring.cmo
colouring.cmo: intf_graph.cmo
intf_graph.cmo: liveness.cmo
livenesss.cmo: register.cmi label.cmi
	
#type_checker.ml: asg.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend


