#!/bin/bash
if [ -n "${KMP_DETERMINISTIC_REDUCTION}" ]
then
	unset KMP_DETERMINISTIC_REDUCTION
fi
export KMP_DETERMINISTIC_REDUCTION=TRUE
if [ -n "${OMP_PLACES}" ]
then
	unset OMP_PLACES
fi
export OMP_PLACES=CORES
if [ -n "${OMP_PROC_BIND}" ]
then
	unset OMP_PROC_BIND
fi
export OMP_PROC_BIND=SPREAD
