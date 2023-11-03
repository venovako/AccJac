# Additional materials

## thm1.wls

Usage:
```bash
./thm1.wls p n β d
```
where `p` and `β` are described in the paper, `n` is the number of digits of precision (e.g., `n=100`), and `d` is the number of digits after the decimal point to round the results to (e.g., `d=8`).

## thm1.sh

Calls `thm1.wls` for all combinations of `p` and `β`, producing `thm1-p-β.txt`.

## thm1tβ.txt

The outputs of `thm1.wls` for `t=s,d,q` (i.e., `p=23,52,112`) and `β=1,2`, named differently than the outputs of `thm1.sh` for safety.

## tdet.pdf

For `t=c,d,s,z`, the test results for departure of `U` from unitarity.

## trot.pdf

For `t=c,d,s,z`, the test results for accuracy of the elements of `U`.

## tort.pdf

For `t=c,z`, the order-`n` test results for orthogonality of `U`.
