.PHONY: default all opt doc install uninstall clean
default: all opt
all:
	ocamlc -c -g utf8conv.mli
	ocamlc -c -g utf8conv.ml
	ocamlc -a -g -o utf8conv.cma utf8conv.cmo
opt:
	ocamlc -c -g utf8conv.mli
	ocamlopt -c -g utf8conv.ml
	ocamlopt -a -g -o utf8conv.cmxa utf8conv.cmx
doc:
	mkdir -p html
	ocamldoc -html -d html utf8conv.mli
install:
	ocamlfind install utf8conv META \
		$$(ls *.mli *.cm[ioxa] *.cmxa *.o *.a 2>/dev/null)
uninstall:
	ocamlfind remove utf8conv
clean:
	rm -f *.cm[ioxa] *.o *.cmxa *.a *~
	rm -rf html
