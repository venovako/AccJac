#!/bin/bash
source $3/env.sh
for ((I=$1;I<=$2;I*=2))
do
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo $I $T $S
			cd $S
			OMP_NUM_THREADS=$4 $3/${T}jsvrr.exe $I $I $T$I > $T$I-$S.txt 2>> $T$I-$S.err
			tr '\n' , < $T$I-$S.out > $T$I-$S.tuo
			OMP_NUM_THREADS=$4 $3/${T}jsvor.exe $I $I $T$I.YU >> $T$I-$S.tuo 2>> $T$I-$S.err
			tr '\n' , < $T$I-$S.tuo > $T$I-$S.out
			OMP_NUM_THREADS=$4 $3/${T}jsvor.exe $I -1 $T$I.YV >> $T$I-$S.out 2>> $T$I-$S.err
			rm -fv $T$I-$.tuo
			cd ..
		done
	done
done
unset S T I
