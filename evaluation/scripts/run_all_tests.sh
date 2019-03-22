#!/bin/bash
run_test () {
  for i in `seq 5 8`;
  do
    bash $1
    base="$(basename $1)"
    mv log.txt $base.$i.log
  done

  mv *.log ../all_logs/
}

for i in ../scripts/tests/*tmp*;
do
  run_test $i
done
