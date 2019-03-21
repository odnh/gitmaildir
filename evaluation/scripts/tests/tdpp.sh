#! /bin/bash
# Times how long to deliver n emails at different levels of parallelism

for i in `seq 1 1 20`
do
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   echo -n "$i " >> log.txt

   { time -p seq -f "data_75kB/file.%05g" 0 999 | tr '\n' '\0' | xargs -P$i -n1 -I% -0 sh -c 'cat % | gitmaildir_cli deliver --dir=gmd' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf gmd
done
