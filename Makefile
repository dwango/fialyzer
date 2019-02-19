INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build @install

beam:
	test/blackbox-test/compile.sh

test: unit-test beam blackbox-test

unit-test:
	dune runtest test/unit-test

blackbox-test:
	dune runtest test/blackbox-test

promote:
	dune promote

doc: odoc pdf

odoc:
	dune build @doc

pdf:
	ls docs/*.saty | xargs -L 1 docker run --rm -v $(PWD):/home/opam/satysfi amutake/satysfi:0.0.3-dev2019.02.13 satysfi {}

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

clean:
	rm -rf _build

.PHONY: default install beam test unit-test blackbox-test promote doc uninstall reinstall clean
