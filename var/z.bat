ECHO 1..4096 > z-%1.txt
..\src\zjsvdx.exe    1    1 -1 %1    z1 >> z-%1.txt 2> z-%1_1.trc
..\src\zjsvdx.exe    2    2 -1 %1    z2 >> z-%1.txt 2> z-%1_2.trc
..\src\zjsvdx.exe    4    4 -1 %1    z4 >> z-%1.txt 2> z-%1_4.trc
..\src\zjsvdx.exe    8    8 -1 %1    z8 >> z-%1.txt 2> z-%1_8.trc
..\src\zjsvdx.exe   16   16 -1 %1   z16 >> z-%1.txt 2> z-%1_16.trc
..\src\zjsvdx.exe   32   32 -1 %1   z32 >> z-%1.txt 2> z-%1_32.trc
..\src\zjsvdx.exe   64   64 -1 %1   z64 >> z-%1.txt 2> z-%1_64.trc
..\src\zjsvdx.exe  128  128 -1 %1  z128 >> z-%1.txt 2> z-%1_128.trc
..\src\zjsvdx.exe  256  256 -1 %1  z256 >> z-%1.txt 2> z-%1_256.trc
..\src\zjsvdx.exe  512  512 -1 %1  z512 >> z-%1.txt 2> z-%1_512.trc
..\src\zjsvdx.exe 1024 1024 -1 %1 z1024 >> z-%1.txt 2> z-%1_1024.trc
..\src\zjsvdx.exe 2048 2048 -1 %1 z2048 >> z-%1.txt 2> z-%1_2048.trc
..\src\zjsvdx.exe 4096 4096 -1 %1 z4096 >> z-%1.txt 2> z-%1_4096.trc
