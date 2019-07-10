#!/bin/bash

set -eu
cd $(dirname $0)

cd cases
for case in $(ls); do
  pushd $case
  erlc +debug_info *.erl
  popd
done
