
build:
	ocamlbuild -I src src/field.native

html:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src doc/html.docdir/index.html

test:
	ocamlbuild -use-ocamlfind -pkgs 'unix,zarith' -I src src/test.native
	./test.native

clean:
	rm -r _build/
