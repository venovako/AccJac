@ECHO OFF
ECHO 1..4096 > d-0.txt
..\src\djsvdx.exe    1    1 -1 0    d1 >> d-0.txt 2> d-0_1.trc
..\src\djsvdx.exe    2    2 -1 0    d2 >> d-0.txt 2> d-0_2.trc
..\src\djsvdx.exe    4    4 -1 0    d4 >> d-0.txt 2> d-0_4.trc
..\src\djsvdx.exe    8    8 -1 0    d8 >> d-0.txt 2> d-0_8.trc
..\src\djsvdx.exe   16   16 -1 0   d16 >> d-0.txt 2> d-0_16.trc
..\src\djsvdx.exe   32   32 -1 0   d32 >> d-0.txt 2> d-0_32.trc
..\src\djsvdx.exe   64   64 -1 0   d64 >> d-0.txt 2> d-0_64.trc
..\src\djsvdx.exe  128  128 -1 0  d128 >> d-0.txt 2> d-0_128.trc
..\src\djsvdx.exe  256  256 -1 0  d256 >> d-0.txt 2> d-0_256.trc
..\src\djsvdx.exe  512  512 -1 0  d512 >> d-0.txt 2> d-0_512.trc
..\src\djsvdx.exe 1024 1024 -1 0 d1024 >> d-0.txt 2> d-0_1024.trc
..\src\djsvdx.exe 2048 2048 -1 0 d2048 >> d-0.txt 2> d-0_2048.trc
..\src\djsvdx.exe 4096 4096 -1 0 d4096 >> d-0.txt 2> d-0_4096.trc
