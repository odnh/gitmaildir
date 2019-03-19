#!/bin/bash
run_test () {
  for i in `seq 1 10`;
  do
    bash ../scripts/tests/$1
    mv log.txt $1.$i.log
  done

  mv *.log ../all_logs/
}

for i in ../scripts/tests/*;
do
  run_test $i
done
