#!/bin/bash
for p in 24 53 64
do
	echo -n "${p} -113 9 ... "
	./rejv2.wls ${p} -113 9 > rejv2-${p}.txt
	echo "rejv2-${p}.txt"
	echo -n "${p}  113 9 ... "
	./rejv2.wls ${p}  113 9 > rejv2_${p}.txt
	echo "rejv2_${p}.txt"
done
unset p
