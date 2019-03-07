#! /bin/bash
cd "/Users/oliver/Google Drive/Cambridge/CST_II/project/testing/automated_timing"

for i in `seq 1 50 1000`
do
   mkdir test_files
   cd test_files
   seq -w 1 $i | xargs -n1 -I% sh -c 'dd if=/dev/urandom of=file.% count=1024 &> /dev/null'
   cd ..
   mkdir gmd

   time gitmaildir_cli convert gmd test_files &> /dev/null

   rm -rf test_files gmd
done
