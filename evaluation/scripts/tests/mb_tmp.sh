#! /bin/bash
# Times how long to deliver n emails in parallel

for i in `seq 100 100 1000`
do
   cp -r mb_data mb

   echo -n "$i " >> log.txt

   { time -p seq -f "1000rand/file.%04g" 0 $i | tr '\n' '\0' | xargs -P10 -n1 -I% -0 sh -c 'cat % | ../../gitmaildir/_build/default/evaluation/maildir/mbox_cli.exe move "new/%" "cur/%2,F" --dir=mb' ; } 2>&1 | grep real | sed 's/real //' | awk 1 ORS='' >> log.txt

   echo >> log.txt # adds new line

   rm -rf mb
done
