.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: 
	rm bisect*.coverage
	dune exec --instrument-with bisect_ppx ./test/test.exe
	bisect-ppx-report html

clean:
	dune clean

docs:
	dune clean
	dune build @doc