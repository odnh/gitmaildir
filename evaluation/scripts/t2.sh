#! /bin/bash
cd "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/automated_timing"

for i in `seq 1 1 20`
do
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   echo -n "$i " >> log.txt

   echo "10" >> log.txt

   rm -rf gmd
done
