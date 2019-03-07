#! /bin/bash
cd "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/automated_timing"

for i in `seq 1 1 20`
do
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   echo -n "$i " >> log.txt

   { time -p echo ../1000rand/* | xargs -P$i -n1 -I% sh -c 'cat % | gitmaildir_cli deliver --dir=gmd' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   rm -rf gmd
done
