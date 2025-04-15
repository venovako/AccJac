#!/bin/bash
for ((I=$1;I<=$2;I*=2))
do
	for T in d z
	do
		for S in 1 3 5 7
		do
			echo $I $T $S
			$3/${T}jsvdt.exe $4$I $5$I -1 $S $T$I > $T$I-$S.out 2> $T$I-$S.err
		done
	done
done
unset S T I
