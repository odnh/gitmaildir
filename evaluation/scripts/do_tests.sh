for i in `seq 1 10`; do bash ../scripts/timing_deliver_sequential.sh  ; mv log.txt tds.$i.log; done
mv tds.* ../logs/
for i in `seq 1 10`; do bash ../scripts/timing_deliver_parallel.sh  ; mv log.txt tdp.$i.log; done
mv tdp.* ../logs/
