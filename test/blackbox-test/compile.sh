#!/bin/bash

set -eu
cd $(dirname $0)

dir="test-cases"

for erl in $(ls ${dir}/*.erl); do
  erlc +debug_info -o $dir $erl
done
