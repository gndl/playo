.PHONY: default build install uninstall test clean

default: build

build:
	dune build src/playo.exe

dbg:
	dune build src/playo.bc
	cp ./_build/default/src/playo.bc .

test:
	dune runtest -f

exec:
	dune exec src/playo.exe

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
#git clean -dfXq
