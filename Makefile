INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
MINIMAL_PLT := test/blackbox-test/minimal.plt

# Default rule
default:
	dune build @install

beam:
	test/blackbox-test/compile.sh

$(MINIMAL_PLT):
	dialyzer --build_plt --output_plt $(MINIMAL_PLT) \
	  --apps stdlib

test: unit-test beam $(MINIMAL_PLT) blackbox-test

unit-test:
	dune runtest test/unit-test

blackbox-test:
	dune runtest test/blackbox-test

comparison:
	dune build test/blackbox-test/run_time_comparison.sh

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
	rm -f $(MINIMAL_PLT)

.PHONY: default install beam test unit-test blackbox-test comparison promote doc uninstall reinstall clean
