#!/bin/bash

set -u
cd $(dirname $0)

fialyzer=$1
plt=$2
dir="test-cases"
data_file=$(mktemp)
trap "rm $data_file" 0 1 2 3 15

for beam in $(ls ${dir}/*.beam); do
  testname=$(basename $beam .beam)
  echo -n "$testname " >> $data_file
  dialyzer_time=$(/usr/bin/time -f '%e' sh -c  "dialyzer --plt $plt $beam >/dev/null 2>&1 || true" 2>&1)
  fialyzer_time=$(/usr/bin/time -f '%e' sh -c "$fialyzer --plt $plt $beam >/dev/null 2>&1 || true" 2>&1)
  echo $dialyzer_time $fialyzer_time >> $data_file
done

gnuplot << EOS
set terminal svg;
set termoption noenhanced;
set output "run_time_comparison.svg";
set ylabel "Run time (seconds), less is better" offset 1.5,0;
set yrange [0.0:];
set offsets 1,1,0.5,0
set style data boxes;
set style fill solid 1.0;
set boxwidth 0.5 absolute;
set bar 0.6;
set grid ytics noxtics;
set ytics 0, 0.5;
set xtics nomirror scale 0 rotate by -90;
set grid ytics noxtics;
set key;
plot \
 "$data_file" using (\$0*2):2 with boxes lc rgb "black" fs solid 0.5 title "dialyzer", \
 "$data_file" using (\$0*2+0.5):3:xticlabels(1) with boxes lc rgb "black" fs solid 0.2 title "fialyzer"
EOS
