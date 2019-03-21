#! /bin/bash
# Times how long to deliver n emails at different levels of parallelism

for i in `seq 1 1 20`
do
   touch mbox
	
   echo -n "$i " >> log.txt

   { time -p seq -f "data_75kB/file.%05g" 0 999 | tr '\n' '\0' | xargs -P$i -n1 -I% -0 sh -c 'cat % | ../../gitmaildir/_build/default/evaluation/performance_test/mbox_cli.exe deliver --dir=mbox' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf mbox
done
