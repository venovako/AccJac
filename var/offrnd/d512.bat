REM 1
djevdt.exe -512  256 1 d512 > d512_1.out 2> d512_1.err
MOVE /Y d512.csv d512_1.csv
COPY /Y /V d512_1.err+d512_1.out d512_1.txt
DEL /F d512_1.err d512_1.out
djevdt.exe -512 -257 1 d512 > d512-1.out 2> d512-1.err
MOVE /Y d512.csv d512-1.csv
COPY /Y /V d512-1.err+d512-1.out d512-1.txt
DEL /F d512-1.err d512-1.out
REM 7
djevdt.exe -512  256 7 d512 > d512_7.out 2> d512_7.err
MOVE /Y d512.csv d512_7.csv
COPY /Y /V d512_7.err+d512_7.out d512_7.txt
DEL /F d512_7.err d512_7.out
djevdt.exe -512 -257 7 d512 > d512-7.out 2> d512-7.err
MOVE /Y d512.csv d512-7.csv
COPY /Y /V d512-7.err+d512-7.out d512-7.txt
DEL /F d512-7.err d512-7.out
REM 9
djevdt.exe -512  256 9 d512 > d512_9.out 2> d512_9.err
MOVE /Y d512.csv d512_9.csv
COPY /Y /V d512_9.err+d512_9.out d512_9.txt
DEL /F d512_9.err d512_9.out
djevdt.exe -512 -257 9 d512 > d512-9.out 2> d512-9.err
MOVE /Y d512.csv d512-9.csv
COPY /Y /V d512-9.err+d512-9.out d512-9.txt
DEL /F d512-9.err d512-9.out
REM clean
DEL /F d512.L d512.V
