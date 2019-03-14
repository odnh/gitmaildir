#!/bin/bash
for i in `seq 1 10`;
do
  <INSERT COMMAND>
  mv log.txt <logname>.$i.log
done   
