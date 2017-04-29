
build:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src ec.cmxa

html: docs
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src doc/html.docdir/index.html
	cp _build/doc/html.docdir/* docs/

docs:
	mkdir docs

test:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src src/test.native
	./test.native

installpkg:
	opam install zarith

clean:
	rm -r _build/
