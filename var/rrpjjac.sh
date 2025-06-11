#!/bin/bash
source $3/var/env.sh
for ((I=$1;I<=$2;I*=2))
do
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo $I $T $S
			cd $S
			OMP_NUM_THREADS=$4 $3/src/${T}jsvrr.exe $I $I $T$I > $T$I-$S.rr 2>> $T$I-$S.err
			cd ..
		done
	done
done
unset S T I
