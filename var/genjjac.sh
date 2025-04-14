#!/bin/bash
for ((I=$1;I<=$2;I*=2))
do
	$3/gentxt.exe $I -1D0 1D0
	$3/dgenhsvd.exe F.txt 1 $I d$I
	$3/zgenhsvd.exe F.txt 2 $I z$I
done
unset I
