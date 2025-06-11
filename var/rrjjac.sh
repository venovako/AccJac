#!/bin/bash
for ((I=$1;I<=$2;I*=2))
do
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo $I $T $S
			cd $S
			$3/${T}jsvrr.exe $I $I $T$I > $T$I-$S.txt 2>> $T$I-$S.err &
			cd ..
		done
	done
done
wait
unset S T I
