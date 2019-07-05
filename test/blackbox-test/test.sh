#!/bin/bash

set -eu
cd $(dirname $0)

fialyzer=$1
plt=$2
dir="test-cases"
performance="performance.log"

date "+%Y-%m-%d-%H:%M:%S" > $performance
for beam in $(ls ${dir}/*.beam); do
  echo $beam >> $performance
  testname=$(basename $beam .beam)
  expected="${dir}/${testname}.expected"
  output="${dir}/${testname}.output"
  time ($fialyzer --plt $plt $beam &> $output) &>> $performance || true
  diff -u $expected $output
done
