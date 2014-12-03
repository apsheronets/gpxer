PACKAGES = extlib,magick,xmlm,lwt,lwt.unix,lwt.extra
FILES = xmlm_shit.ml main.ml

NAME = gpxer
VERSION := $(shell head -n 1 VERSION)
CAMLC   = ocamlfind ocamlc   -thread $(LIB)
CAMLOPT = ocamlfind ocamlopt -thread $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package $(PACKAGES)
PP =

ifndef PREFIX
  PREFIX=/usr/local
endif

OBJS    = $(FILES:.ml=.cmo)
OPTOBJS = $(FILES:.ml=.cmx)

BYTE = bin/$(NAME)
OPT = bin/$(NAME).opt
STATIC = bin/$(NAME).static

all: $(BYTE) $(OPT)
static: $(STATIC)

$(BYTE): $(OBJS)
	mkdir -p bin
	$(CAMLC) -linkpkg -o $@ $^

$(OPT): $(OPTOBJS)
	mkdir -p bin
	$(CAMLOPT) -linkpkg -o $@ $^

$(STATIC): $(OPTOBJS)
	mkdir -p bin
	$(CAMLOPT) -linkpkg -noautolink -cclib '-Wl,-Bstatic -limagemagick_stubs -Wl,-Bdynamic -lMagickCore -lbigarray -lunix -lz' -o $@ $^

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(PP) -c $<
.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

install: $(BYTE) $(OPT)
	install -m 755 $(BYTE) $(PREFIX)/$(BYTE)
	install -m 755 $(OPT)  $(PREFIX)/$(OPT)
	install -d share/$(NAME) $(PREFIX)/share/$(NAME)
	install -m 644 share/$(NAME)/* $(PREFIX)/share/$(NAME)

uninstall:
	rm $(PREFIX)/$(BYTE)
	rm $(PREFIX)/$(OPT)
	rm -r $(PREFIX)/share/$(NAME)

clean:
	-rm -f *.cm[ioxa] *.cmx[as] *.o *.a *~
	-rm -f .depend
	-rm -f $(BYTE) $(OPT) $(STATIC)

depend: .depend

.depend: $(FILES)
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

-include .depend
