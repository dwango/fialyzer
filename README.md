[WIP] fialyzer
==============

[![CircleCI](https://circleci.com/gh/dwango/fialyzer.svg?style=svg)](https://circleci.com/gh/dwango/fialyzer)

Faster Implementation of Dialyzer

Prerequisite
------------

- ocaml-4.07.0 or higher
- opam-2.0.0 or higher
- GMP
- Perl
- odoc (optional, for `make doc`)
- Erlang/OTP (optional, for `make test`)

Build
-----

```shell
# If you prefer to use per-package environment, do:
$ opam switch create fialyzer ocaml-base-compiler.4.07.1
$ eval $(opam env)

# Clone repo:
$ git clone git@github.com:dwango/fialyzer.git
$ cd fialyzer

# Install dependencies:
$ opam pin add -y fialyzer .

# Build package:
$ make
```
