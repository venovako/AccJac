#!/bin/bash
for ((J=0;J<$1;J+=$2))
do
	for T in d z
	do
		for S in 0 2 4 6
		do
			echo $J $T $S
			cd $S
			$3/${T}jsvdx.exe $1 $4$1 $J $S $T$1 > $T$1-$J-$S.out 2> $T$1-$J-$S.err &
			cd ..
		done
	done
done
wait
unset S T J
