#!/bin/bash
for B in 1 2
do
	for P in 23 52 112
	do
		echo "p=$P b=$B"
		./thm1.wls $P 100 $B 8 > thm1-$P-$B.txt
	done
done
unset B P
