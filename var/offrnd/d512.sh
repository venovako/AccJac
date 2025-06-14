#!/bin/bash
source env.sh
for S in 1 7 9
do
	OMP_NUM_THREADS=1 ./djevdt.exe -512  256 $S d512 > d512_$S.out 2> d512_$S.err &
	OMP_NUM_THREADS=1 ./djevdt.exe -512 -257 $S d512 > d512-$S.out 2> d512-$2.err &
done
wait
unset S
