
build:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src ec.cmxa

html: man
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src doc/html.docdir/index.html
	cp _build/doc/html.docdir/* man/

man:
	mkdir man

test:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src src/test.native
	./test.native

installpkg:
	opam install zarith

clean:
	rm -r _build/
