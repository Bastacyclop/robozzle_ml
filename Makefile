OC := ocamlfind ocamlopt
OL := ocamlfind ocamlopt

TRGT := robozzle_ml
SRCDIR := src
LIBS := str,sdl,sdl.sdlgfx,sdl.sdlimage,sdl.sdlttf
INTF := puzzle.cmi display.cmi events.cmi time.cmi vm.cmi code.cmi editor.cmi

all: $(TRGT)

$(TRGT): puzzle.cmx display.cmx events.cmx time.cmx vm.cmx code.cmx editor.cmx main.cmx
	$(OL) -o $@ -package $(LIBS) -linkpkg $^

%.cmx: $(SRCDIR)/%.ml $(INTF)
	$(OC) -o $@ -package $(LIBS) -c $<

%.cmi: $(SRCDIR)/%.mli
	$(OC) -o $@ -package $(LIBS) -c $<

clean:
	rm -f *.cm[iox] *.o
