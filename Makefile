INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build @install

test_beam:
	test/blackbox-test/compile.sh

benchmark_beam:
	benchmark/compile.sh

test: unit-test test_beam $(MINIMAL_PLT) blackbox-test

unit-test:
	dune runtest test/unit-test

blackbox-test:
	dune runtest test/blackbox-test

comparison:
	dune build test/blackbox-test/run_time_comparison.svg

benchmark:
	dune build benchmark/benchmark.svg

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

.PHONY: default install test_beam test unit-test blackbox-test comparison benchmark promote doc uninstall reinstall clean
