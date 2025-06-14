#!/bin/bash
source env.sh
for S in 1 7 9
do
	OMP_NUM_THREADS=1 ./zjevdt.exe -512  256 $S z512 > z512_$S.out 2> z512_$S.err
	mv [Zz]512.csv z512_$S.csv
	cat z512_$S.err z512_$S.out > z512_$S.txt
	rm -fv z512_$S.err z512_$S.out
	OMP_NUM_THREADS=1 ./zjevdt.exe -512 -257 $S z512 > z512-$S.out 2> z512-$S.err
	mv [Zz]512.csv z512-$S.csv
	cat z512-$S.err z512-$S.out > z512-$S.txt
	rm -fv z512-$S.err z512-$S.out
done
unset S
rm -fv [Zz]512???.txt
