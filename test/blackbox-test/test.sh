#!/usr/bin/env sh

set -eu
cd $(dirname $0)

fialyzer=$1
dir="test-cases"

for beam in $(ls ${dir}/*.beam); do
  testname=$(basename $beam .beam)
  expected="${dir}/${testname}.expected"
  output="${dir}/${testname}.output"
  $fialyzer --plt ./sample_plt $beam > $output
  diff $output $expected
done
