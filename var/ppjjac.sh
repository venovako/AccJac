#!/bin/bash
for T in d z
do
	for S in 0 2 4 6
	do
		echo '"N", "J", "SWEEPS", "TRANSFS", "GS", "TIMEs", "MAXREΛ", "max||GVj-UjΣj||_F", "||G-UΣV^-1||_F/||G||_F", "||U^HU-I||_F", "||V^HJV-J||_F"' > $T-$S.csv
		for ((I=$1;I<=$2;I*=2))
		do
			let "J=I/2"
			printf "%4d, %4d," $I $J >> $T-$S.csv
			cat $S/$T$I-$S.out >> $T-$S.csv
			cat $S/$T$I-$S.err $S/$T$I-$S.txt > $T$I-$S.txt
		done
	done
done
unset S T J I
