@ECHO OFF
ECHO 1..4096 > z-2.txt
..\src\zjsvdx.exe    1    1 -1 2    z1 >> z-2.txt 2> z-2_1.trc
..\src\zjsvdx.exe    2    2 -1 2    z2 >> z-2.txt 2> z-2_2.trc
..\src\zjsvdx.exe    4    4 -1 2    z4 >> z-2.txt 2> z-2_4.trc
..\src\zjsvdx.exe    8    8 -1 2    z8 >> z-2.txt 2> z-2_8.trc
..\src\zjsvdx.exe   16   16 -1 2   z16 >> z-2.txt 2> z-2_16.trc
..\src\zjsvdx.exe   32   32 -1 2   z32 >> z-2.txt 2> z-2_32.trc
..\src\zjsvdx.exe   64   64 -1 2   z64 >> z-2.txt 2> z-2_64.trc
..\src\zjsvdx.exe  128  128 -1 2  z128 >> z-2.txt 2> z-2_128.trc
..\src\zjsvdx.exe  256  256 -1 2  z256 >> z-2.txt 2> z-2_256.trc
..\src\zjsvdx.exe  512  512 -1 2  z512 >> z-2.txt 2> z-2_512.trc
..\src\zjsvdx.exe 1024 1024 -1 2 z1024 >> z-2.txt 2> z-2_1024.trc
..\src\zjsvdx.exe 2048 2048 -1 2 z2048 >> z-2.txt 2> z-2_2048.trc
..\src\zjsvdx.exe 4096 4096 -1 2 z4096 >> z-2.txt 2> z-2_4096.trc
