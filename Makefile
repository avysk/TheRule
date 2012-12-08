all:
	@ocamlbuild -lib graphics therule.native

clean:
	@ocamlbuild -clean

.PHONY: all clean
