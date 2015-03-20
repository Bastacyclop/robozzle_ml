OC := ocamlfind ocamlopt
OL := ocamlfind ocamlopt

TRGT := robozzle_ml
SRCDIR := src
LIBS := str,sdl,sdl.sdlgfx,sdl.sdlimage,sdl.sdlttf
INTF := puzzle.cmi graphics.cmi

all: $(TRGT)

$(TRGT): puzzle.cmx graphics.cmx main.cmx
	$(OL) -o $@ -package $(LIBS) -linkpkg $^

%.cmx: $(SRCDIR)/%.ml $(INTF)
	$(OC) -o $@ -package $(LIBS) -c $<

%.cmi: $(SRCDIR)/%.mli
	$(OC) -o $@ -package $(LIBS) -c $<

clean:
	rm *.cm[iox]
