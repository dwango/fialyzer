#!/bin/bash

set -eu
cd $(dirname $0)

fialyzer=$1
plt=$2
dir="test-cases"
performance="performance.log"

do_fialyzer() { # $1 = beam, $2 = output
    $fialyzer --plt $plt $1 > $2 2>&1
}

date "+%Y-%m-%d-%H:%M:%S" > $performance
for beam in $(ls ${dir}/*.beam); do
  echo $beam >> $performance
  testname=$(basename $beam .beam)
  expected="${dir}/${testname}.expected"
  output="${dir}/${testname}.output"
  # We need the following `true` to circumvent that this script fails due to an error caused by fialyzer.
  # In this script, we are only interested in the results of diffs.
  (time (do_fialyzer $beam $output) >> $performance 2>&1) || true
  diff -u $expected $output
done
