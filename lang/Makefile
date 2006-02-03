OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES = langcompile.ml
USE_CAMLP4 =
INCDIRS =
OCAMLLDFLAGS =
OCAMLDEP =
RESULT  = langcompile
TRASH   = *.htbl

all: nc langmsgs

langmsgs: $(patsubst %.msg,%.htbl ,$(wildcard *.msg))
%.htbl: %.msg
	./langcompile import $(basename $@) $<


include $(OCAMLMAKEFILE)

