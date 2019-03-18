#! /bin/bash
# Times how long to deliver n emails in parallel

for i in `seq 100 100 1000`
do
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   mkdir data
   seq -f "1000rand/file.%04g" 1 1 $i | xargs cp -t data

   echo -n "$i " >> log.txt

   tds.exe gmd data log.txt

   echo >> log.txt # adds new line

   rm -rf data
   rm -rf gmd
done
