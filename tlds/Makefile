OCAMLMAKEFILE = ../../OCamlMakefile

SOURCES		= createtlds.ml
PACKS		= dbm
INCDIRS		=
OCAMLLDFLAGS	=
OCAMLFLAGS	=
OCAMLDEP	=
RESULT		= createtlds
TRASH		= tlds.db tlds.dir tlds.pag

all: nc tldsdb

tldsdb:
	rm -rf *.db *.dir *.pat
	./createtlds tlds

include $(OCAMLMAKEFILE)

