#! /bin/bash
# Times how long to deliver n emails in parallel

for i in `seq 100 100 1000`
do
   cp -r gmd_data gmd
   gitmaildir_cli init --dir=gmd

   echo -n "$i " >> log.txt

   { time -p seq -f "1000rand/file.%04g" 0 $i | tr '\n' '\0' | xargs -P10 -n1 -I% -0 sh -c 'cat % | gitmaildir_cli move "new/%" "cur/%2,F" --dir=gmd' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf gmd
done
