#!/bin/bash
for ((I=1;I<=$1;I*=2))
do
	for T in d z
	do
		for S in 1 3
		do
			echo $I $T $S
			$2/${T}jsvdt.exe $I $I -1 $S $T$I > $T$I-$S.out 2> $T$I-$S.err
		done
	done
done
unset S T I
