@ECHO OFF
ECHO 1..4096 > z-5.txt
..\src\zjsvdt.exe    1    1 -1 5    z1 >> z-5.txt 2> z-5_1.trc
..\src\zjsvdt.exe    2    2 -1 5    z2 >> z-5.txt 2> z-5_2.trc
..\src\zjsvdt.exe    4    4 -1 5    z4 >> z-5.txt 2> z-5_4.trc
..\src\zjsvdt.exe    8    8 -1 5    z8 >> z-5.txt 2> z-5_8.trc
..\src\zjsvdt.exe   16   16 -1 5   z16 >> z-5.txt 2> z-5_16.trc
..\src\zjsvdt.exe   32   32 -1 5   z32 >> z-5.txt 2> z-5_32.trc
..\src\zjsvdt.exe   64   64 -1 5   z64 >> z-5.txt 2> z-5_64.trc
..\src\zjsvdt.exe  128  128 -1 5  z128 >> z-5.txt 2> z-5_128.trc
..\src\zjsvdt.exe  256  256 -1 5  z256 >> z-5.txt 2> z-5_256.trc
..\src\zjsvdt.exe  512  512 -1 5  z512 >> z-5.txt 2> z-5_512.trc
..\src\zjsvdt.exe 1024 1024 -1 5 z1024 >> z-5.txt 2> z-5_1024.trc
..\src\zjsvdt.exe 2048 2048 -1 5 z2048 >> z-5.txt 2> z_5_2048.trc
