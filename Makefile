
BIN = pgoc
TEST = ./test.sh

FLAGS = -use-ocamlfind -use-menhir -I src -I lib
OCB = ocamlbuild $(FLAGS)

.PHONY: all clean debug sanity test-syntax test-typing test-compil test

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

all: $(BIN)

$(BIN): sanity
	$(OCB) main.native
	mv main.native $@

clean:
	$(OCB) -clean
	rm -f src/*~ lib/*~ *~

sanity:
	ocamlfind query zarith

test-syntax: $(BIN)
	cd tests && $(TEST) -1 ../$(BIN)

test-typing: $(BIN)
	cd tests && $(TEST) -2 ../$(BIN)

test-compil: $(BIN)
	cd tests && $(TEST) -3 ../$(BIN)

test: $(BIN)
	cd tests && $(TEST) -all ../$(BIN)

