#!/bin/bash
# ./varjjac.sh N Minc SeqExes ParExes Threads
source $4/env.sh
for T in d z
do
	for S in 0 2 4 6
	do
		echo '"N", "J", "SWEEPS", "TRANSFS", "GS", "TIMEs", "MAXREΛ", "max||GVj-UjΣj||_F", "||G-UΣV^-1||_F/||G||_F", "||U^HU-I||_F", "||V^HJV-J||_F"' > $T$1-$S.csv
	done
done
for ((J=0;J<$1;J+=$2))
do
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo "Running $J $T $S ..."
			cd $S
			$3/${T}jsvdx.exe $1 $1 $J $S $T$1 > $T$1-$S.out 2> $T$1-$S.err &
			cd ..
		done
	done
	wait
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo "Postprocessing $J $T $S ..."
			cd $S
			OMP_NUM_THREADS=$5 $4/${T}jsvrr.exe $1 $1 $T$1 > $T$1-$S.txt 2>> $T$1-$S.err
			tr '\n' , < $T$1-$S.out > $T$1-$S.tuo
                        tail -2 $T$1-$S.txt | cut -d',' -f2 | tr '\n' , >> $T$1-$S.tuo
			OMP_NUM_THREADS=$5 $4/${T}jsvor.exe $1 $1 $T$1.YU >> $T$1-$S.tuo 2>> $T$1-$S.err
			tr '\n' , < $T$1-$S.tuo > $T$1-$S.out
			OMP_NUM_THREADS=$5 $4/${T}jsvor.exe $1 $J $T$1.YV >> $T$1-$S.out 2>> $T$1-$S.err
			rm -fv $T$1-$S.tuo
			cd ..
			printf "%4d, %4d," $1 $J >> $T$1-$S.csv
			cat $S/$T$1-$S.out >> $T$1-$S.csv
			cat $S/$T$1-$S.err $S/$T$1-$S.txt > $T$1-$S-$J.txt
		done
	done
done
unset S T J
