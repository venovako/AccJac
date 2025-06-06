@ECHO OFF
ECHO 1..4096 > d-2.txt
..\src\djsvdx.exe    1    1 -1 2    d1 >> d-2.txt 2> d-2_1.trc
..\src\djsvdx.exe    2    2 -1 2    d2 >> d-2.txt 2> d-2_2.trc
..\src\djsvdx.exe    4    4 -1 2    d4 >> d-2.txt 2> d-2_4.trc
..\src\djsvdx.exe    8    8 -1 2    d8 >> d-2.txt 2> d-2_8.trc
..\src\djsvdx.exe   16   16 -1 2   d16 >> d-2.txt 2> d-2_16.trc
..\src\djsvdx.exe   32   32 -1 2   d32 >> d-2.txt 2> d-2_32.trc
..\src\djsvdx.exe   64   64 -1 2   d64 >> d-2.txt 2> d-2_64.trc
..\src\djsvdx.exe  128  128 -1 2  d128 >> d-2.txt 2> d-2_128.trc
..\src\djsvdx.exe  256  256 -1 2  d256 >> d-2.txt 2> d-2_256.trc
..\src\djsvdx.exe  512  512 -1 2  d512 >> d-2.txt 2> d-2_512.trc
..\src\djsvdx.exe 1024 1024 -1 2 d1024 >> d-2.txt 2> d-2_1024.trc
..\src\djsvdx.exe 2048 2048 -1 2 d2048 >> d-2.txt 2> d-2_2048.trc
..\src\djsvdx.exe 4096 4096 -1 2 d4096 >> d-2.txt 2> d-2_4096.trc
