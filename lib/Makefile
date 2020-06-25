
BUILDDIR = ../_build/lib/

FILES = utils label register x86_64 unionFind
CMI = $(addsuffix .cmi, $(FILES))
CMO = $(addsuffix .cmo, $(FILES))

FLAGS = -I +../zarith
OCB = ocamlbuild $(FLAGS)

.PHONY: all clean debug

all: $(CMI) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo

$(CMI):
	$(OCB) $@

$(CMO):
	$(OCB) $@

clean:
	rm -f ../_build/lib/*.cm[io] *~ 

debug:
	$(foreach cm, $(CMI) $(CMO), $(OCB) -tag debug $(cm);)

.depend depend:
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend

