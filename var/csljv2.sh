#!/bin/bash
echo '"THge08", "N", "REDET_ε", "RECH_ε", "RESHR_ε", "RESHI_ε"' > cljv2.csv
echo '"THge08", "N", "REDET_ε", "RECH_ε", "RESH_ε"' > sljv2.csv
for ((I=1;I<=$1;++I))
do
	./cljv2t.exe -$2 >> cljv2.csv 2>> cljv2.txt &
	./sljv2t.exe -$2 >> sljv2.csv 2>> sljv2.txt &
	sleep 1
done
wait
unset I
