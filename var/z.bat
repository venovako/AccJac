ECHO 1..4096 > z-%1.txt
SET KMP_DETERMINISTIC_REDUCTION=TRUE
SET OMP_PLACES=CORES
SET OMP_PROC_BIND=SPREAD
SET OMP_NUM_THREADS=%2
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
..\src\zjsvrr.exe    1    1    z1 >    z1-%1.rrs 2>> z-%1_1.trc
..\src\zjsvrr.exe    2    2    z2 >    z2-%1.rrs 2>> z-%1_2.trc
..\src\zjsvrr.exe    4    4    z4 >    z4-%1.rrs 2>> z-%1_4.trc
..\src\zjsvrr.exe    8    8    z8 >    z8-%1.rrs 2>> z-%1_8.trc
..\src\zjsvrr.exe   16   16   z16 >   z16-%1.rrs 2>> z-%1_16.trc
..\src\zjsvrr.exe   32   32   z32 >   z32-%1.rrs 2>> z-%1_32.trc
..\src\zjsvrr.exe   64   64   z64 >   z64-%1.rrs 2>> z-%1_64.trc
..\src\zjsvrr.exe  128  128  z128 >  z128-%1.rrs 2>> z-%1_128.trc
..\src\zjsvrr.exe  256  256  z256 >  z256-%1.rrs 2>> z-%1_256.trc
..\src\zjsvrr.exe  512  512  z512 >  z512-%1.rrs 2>> z-%1_512.trc
..\src\zjsvrr.exe 1024 1024 z1024 > z1024-%1.rrs 2>> z-%1_1024.trc
..\src\zjsvrr.exe 2048 2048 z2048 > z2048-%1.rrs 2>> z-%1_2048.trc
..\src\zjsvrr.exe 4096 4096 z4096 > z4096-%1.rrs 2>> z-%1_4096.trc
..\src\zjsvor.exe    1    1    z1.YU >>    z1-%1.rrs 2>> z-%1_1.trc
..\src\zjsvor.exe    2    2    z2.YU >>    z2-%1.rrs 2>> z-%1_2.trc
..\src\zjsvor.exe    4    4    z4.YU >>    z4-%1.rrs 2>> z-%1_4.trc
..\src\zjsvor.exe    8    8    z8.YU >>    z8-%1.rrs 2>> z-%1_8.trc
..\src\zjsvor.exe   16   16   z16.YU >>   z16-%1.rrs 2>> z-%1_16.trc
..\src\zjsvor.exe   32   32   z32.YU >>   z32-%1.rrs 2>> z-%1_32.trc
..\src\zjsvor.exe   64   64   z64.YU >>   z64-%1.rrs 2>> z-%1_64.trc
..\src\zjsvor.exe  128  128  z128.YU >>  z128-%1.rrs 2>> z-%1_128.trc
..\src\zjsvor.exe  256  256  z256.YU >>  z256-%1.rrs 2>> z-%1_256.trc
..\src\zjsvor.exe  512  512  z512.YU >>  z512-%1.rrs 2>> z-%1_512.trc
..\src\zjsvor.exe 1024 1024 z1024.YU >> z1024-%1.rrs 2>> z-%1_1024.trc
..\src\zjsvor.exe 2048 2048 z2048.YU >> z2048-%1.rrs 2>> z-%1_2048.trc
..\src\zjsvor.exe 4096 4096 z4096.YU >> z4096-%1.rrs 2>> z-%1_4096.trc
..\src\zjsvor.exe    1 -1    z1.YV >>    z1-%1.rrs 2>> z-%1_1.trc
..\src\zjsvor.exe    2 -1    z2.YV >>    z2-%1.rrs 2>> z-%1_2.trc
..\src\zjsvor.exe    4 -1    z4.YV >>    z4-%1.rrs 2>> z-%1_4.trc
..\src\zjsvor.exe    8 -1    z8.YV >>    z8-%1.rrs 2>> z-%1_8.trc
..\src\zjsvor.exe   16 -1   z16.YV >>   z16-%1.rrs 2>> z-%1_16.trc
..\src\zjsvor.exe   32 -1   z32.YV >>   z32-%1.rrs 2>> z-%1_32.trc
..\src\zjsvor.exe   64 -1   z64.YV >>   z64-%1.rrs 2>> z-%1_64.trc
..\src\zjsvor.exe  128 -1  z128.YV >>  z128-%1.rrs 2>> z-%1_128.trc
..\src\zjsvor.exe  256 -1  z256.YV >>  z256-%1.rrs 2>> z-%1_256.trc
..\src\zjsvor.exe  512 -1  z512.YV >>  z512-%1.rrs 2>> z-%1_512.trc
..\src\zjsvor.exe 1024 -1 z1024.YV >> z1024-%1.rrs 2>> z-%1_1024.trc
..\src\zjsvor.exe 2048 -1 z2048.YV >> z2048-%1.rrs 2>> z-%1_2048.trc
..\src\zjsvor.exe 4096 -1 z4096.YV >> z4096-%1.rrs 2>> z-%1_4096.trc
