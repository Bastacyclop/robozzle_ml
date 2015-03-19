OC := ocamlfind ocamlc
OL := ocamlfind ocamlc

SRCDIR := src
LIBS := sdl,sdl.sdlgfx,sdl.sdlimage,sdl.sdlttf

all: robozzle-ml

robozzle-ml: puzzle.cmo graphics.cmo main.cmo
	$(OL) -o $@ -package $(LIBS) -linkpkg $^

%.cmo: $(SRCDIR)/%.ml
	$(OC) -package $(LIBS) -c $<

%.cmi: $(SRCDIR)/%.mli
	$(OC) -package $(LIBS) -c %<

clean:
	rm *.cmi *.cmo

cleanall: clean
	rm $(TARGET)
