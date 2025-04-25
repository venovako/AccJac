@ECHO OFF
ECHO 1..4096 > d-1.txt
..\src\djsvdt.exe    1    1 -1 1    d1 >> d-1.txt 2> d-1_1.trc
..\src\djsvdt.exe    2    2 -1 1    d2 >> d-1.txt 2> d-1_2.trc
..\src\djsvdt.exe    4    4 -1 1    d4 >> d-1.txt 2> d-1_4.trc
..\src\djsvdt.exe    8    8 -1 1    d8 >> d-1.txt 2> d-1_8.trc
..\src\djsvdt.exe   16   16 -1 1   d16 >> d-1.txt 2> d-1_16.trc
..\src\djsvdt.exe   32   32 -1 1   d32 >> d-1.txt 2> d-1_32.trc
..\src\djsvdt.exe   64   64 -1 1   d64 >> d-1.txt 2> d-1_64.trc
..\src\djsvdt.exe  128  128 -1 1  d128 >> d-1.txt 2> d-1_128.trc
..\src\djsvdt.exe  256  256 -1 1  d256 >> d-1.txt 2> d-1_256.trc
..\src\djsvdt.exe  512  512 -1 1  d512 >> d-1.txt 2> d-1_512.trc
..\src\djsvdt.exe 1024 1024 -1 1 d1024 >> d-1.txt 2> d-1_1024.trc
..\src\djsvdt.exe 2048 2048 -1 1 d2048 >> d-1.txt 2> d-1_2048.trc
..\src\djsvdt.exe 4096 4096 -1 1 d4096 >> d-1.txt 2> d-1_4096.trc
