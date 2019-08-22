# fialyzer

[![CircleCI](https://circleci.com/gh/dwango/fialyzer.svg?style=svg)](https://circleci.com/gh/dwango/fialyzer)

Faster Implementation of Dialyzer

## How to Use

### Using Docker

```console
docker run --rm -v $(pwd):/home/opam/fialyzer yoshihiro503/fialyzer --plt <plt_file> <beam_files>
```

### Commandline Options

|Option|Description|
|---|----|
|`--plt <plt_file>`|Use the specified plt as the initial plt (optional)|
|`--debug`|Print debug logs (optional)|
|`--help`|Display this list of options|


## How to Build (for developers)
### Prerequisite

- ocaml-4.07.0 or higher
- opam-2.0.0 or higher
- GMP
- Perl
- odoc (optional, for `make odoc`)
- Erlang/OTP (optional, for `make test`)
- Docker (optional, for `make pdf`)

### Build

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
