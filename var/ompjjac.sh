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
			OMP_NUM_THREADS=$4 $3/${T}jsvdx.exe $I $5$I -1 $S $T$I > $T$I-$S.out 2> $T$I-$S.err
			cd ..
		done
	done
done
unset S T I
