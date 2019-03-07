#! /bin/bash
cd "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/automated_timing"

for i in `seq 10 10 200`
do
   mkdir test_files
   cd test_files
   seq -w 1 $i | xargs -n1 -I% sh -c 'dd if=/dev/urandom of=file.% count=1024 &> /dev/null'
   cd ..
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   time echo test_files/* | xargs -P20 -n1 -I% sh -c 'cat % | gitmaildir_cli deliver --dir=gmd'

   rm -rf test_files gmd
done
