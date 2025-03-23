#!/bin/bash
for p in 24 53 64
do
	./rejv2.wls ${p} -113 9 > rejv2-${p}.txt
	./rejv2.wls ${p}  113 9 > rejv2_${p}.txt
done
unset p
