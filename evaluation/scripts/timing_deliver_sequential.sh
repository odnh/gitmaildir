#! /bin/bash
# Times how long to deliver n emails in sequentially where n is between 20 and 1000 in steps of 20

for i in `seq 20 20 1000`
do
   mkdir gmd
   gitmaildir_cli init --dir=gmd

   { time -p seq -f "1000rand/file.%04g" 0 $i | tr '\n' '\0' | xargs -P1 -n1 -I% -0 sh -c 'cat % | gitmaildir_cli deliver --dir=gmd' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf gmd
done
