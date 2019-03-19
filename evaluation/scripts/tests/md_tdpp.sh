#! /bin/bash
# Times how long to deliver n emails at different levels of parallelism

for i in `seq 1 1 10`
do
   mkdir gmd
   mkdir gmd/{tmp,cur,new}

   echo -n "$i " >> log.txt

   { time -p find data_75kB/ -mindepth 1 -print0 | xargs -P$i -n1 -I% -0 sh -c 'cat % | ../../gitmaildir/_build/default/evaluation/maildir/maildir_cli.exe deliver --dir=gmd' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf gmd
done
