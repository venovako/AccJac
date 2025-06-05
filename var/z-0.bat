@ECHO OFF
ECHO 1..4096 > z-0.txt
..\src\zjsvdx.exe    1    1 -1 0    z1 >> z-0.txt 2> z-0_1.trc
..\src\zjsvdx.exe    2    2 -1 0    z2 >> z-0.txt 2> z-0_2.trc
..\src\zjsvdx.exe    4    4 -1 0    z4 >> z-0.txt 2> z-0_4.trc
..\src\zjsvdx.exe    8    8 -1 0    z8 >> z-0.txt 2> z-0_8.trc
..\src\zjsvdx.exe   16   16 -1 0   z16 >> z-0.txt 2> z-0_16.trc
..\src\zjsvdx.exe   32   32 -1 0   z32 >> z-0.txt 2> z-0_32.trc
..\src\zjsvdx.exe   64   64 -1 0   z64 >> z-0.txt 2> z-0_64.trc
..\src\zjsvdx.exe  128  128 -1 0  z128 >> z-0.txt 2> z-0_128.trc
..\src\zjsvdx.exe  256  256 -1 0  z256 >> z-0.txt 2> z-0_256.trc
..\src\zjsvdx.exe  512  512 -1 0  z512 >> z-0.txt 2> z-0_512.trc
..\src\zjsvdx.exe 1024 1024 -1 0 z1024 >> z-0.txt 2> z-0_1024.trc
..\src\zjsvdx.exe 2048 2048 -1 0 z2048 >> z-0.txt 2> z-0_2048.trc
..\src\zjsvdx.exe 4096 4096 -1 0 z4096 >> z-0.txt 2> z-0_4096.trc
