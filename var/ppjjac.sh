#!/bin/bash
for T in d z
do
	for S in 1 3 5 7
	do
		echo '"N", "J", "GS", "SWEEPS", "TIMEs", "MAXREΛ", "||G-UΣV^-1||_F/||G||_F"' > $T-$S.csv
		for ((I=1;I<=$1;I*=2))
		do
			let "J=I/2"
			printf "%3d, %3d," $I $J >> $T-$S.csv
			cat $T$I-$S.out >> $T-$S.csv
		done
	done
done
unset S T J I
