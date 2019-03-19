#! /bin/bash
# Times how long to convert 1000 random files to a gitmaildir

for i in `seq 1 50 1000`
do
   mkdir gmd

   time gitmaildir_cli convert gmd 1000rand &> /dev/null

   rm -rf gmd
done
