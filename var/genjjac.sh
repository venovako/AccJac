#!/bin/bash
for ((I=1;I<=$1;I*=2))
do
	$2/gentxt.exe $I -1D0 1D0
	$2/dgenhsvd.exe F.txt 1 $I d$I
	$2/zgenhsvd.exe F.txt 2 $I z$I
done
unset I
