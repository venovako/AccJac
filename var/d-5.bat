@ECHO OFF
ECHO 1..4096 > d-5.txt
..\src\djsvdt.exe    1    1 -1 5    d1 >> d-5.txt 2> d-5_1.trc
..\src\djsvdt.exe    2    2 -1 5    d2 >> d-5.txt 2> d-5_2.trc
..\src\djsvdt.exe    4    4 -1 5    d4 >> d-5.txt 2> d-5_4.trc
..\src\djsvdt.exe    8    8 -1 5    d8 >> d-5.txt 2> d-5_8.trc
..\src\djsvdt.exe   16   16 -1 5   d16 >> d-5.txt 2> d-5_16.trc
..\src\djsvdt.exe   32   32 -1 5   d32 >> d-5.txt 2> d-5_32.trc
..\src\djsvdt.exe   64   64 -1 5   d64 >> d-5.txt 2> d-5_64.trc
..\src\djsvdt.exe  128  128 -1 5  d128 >> d-5.txt 2> d-5_128.trc
..\src\djsvdt.exe  256  256 -1 5  d256 >> d-5.txt 2> d-5_256.trc
..\src\djsvdt.exe  512  512 -1 5  d512 >> d-5.txt 2> d-5_512.trc
..\src\djsvdt.exe 1024 1024 -1 5 d1024 >> d-5.txt 2> d-5_1024.trc
..\src\djsvdt.exe 2048 2048 -1 5 d2048 >> d-5.txt 2> d_5_2048.trc
