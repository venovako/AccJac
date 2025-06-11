ECHO 1..4096 > d-%1.txt
SET KMP_DETERMINISTIC_REDUCTION=TRUE
SET OMP_PLACES=CORES
SET OMP_PROC_BIND=SPREAD
SET OMP_NUM_THREADS=%2
..\src\djsvdx.exe    1    1 -1 %1    d1 >> d-%1.txt 2> d-%1_1.trc
..\src\djsvdx.exe    2    2 -1 %1    d2 >> d-%1.txt 2> d-%1_2.trc
..\src\djsvdx.exe    4    4 -1 %1    d4 >> d-%1.txt 2> d-%1_4.trc
..\src\djsvdx.exe    8    8 -1 %1    d8 >> d-%1.txt 2> d-%1_8.trc
..\src\djsvdx.exe   16   16 -1 %1   d16 >> d-%1.txt 2> d-%1_16.trc
..\src\djsvdx.exe   32   32 -1 %1   d32 >> d-%1.txt 2> d-%1_32.trc
..\src\djsvdx.exe   64   64 -1 %1   d64 >> d-%1.txt 2> d-%1_64.trc
..\src\djsvdx.exe  128  128 -1 %1  d128 >> d-%1.txt 2> d-%1_128.trc
..\src\djsvdx.exe  256  256 -1 %1  d256 >> d-%1.txt 2> d-%1_256.trc
..\src\djsvdx.exe  512  512 -1 %1  d512 >> d-%1.txt 2> d-%1_512.trc
..\src\djsvdx.exe 1024 1024 -1 %1 d1024 >> d-%1.txt 2> d-%1_1024.trc
..\src\djsvdx.exe 2048 2048 -1 %1 d2048 >> d-%1.txt 2> d-%1_2048.trc
..\src\djsvdx.exe 4096 4096 -1 %1 d4096 >> d-%1.txt 2> d-%1_4096.trc
..\src\djsvrr.exe    1    1    d1 >    d1-%1.rrs 2>> d-%1_1.trc
..\src\djsvrr.exe    2    2    d2 >    d2-%1.rrs 2>> d-%1_2.trc
..\src\djsvrr.exe    4    4    d4 >    d4-%1.rrs 2>> d-%1_4.trc
..\src\djsvrr.exe    8    8    d8 >    d8-%1.rrs 2>> d-%1_8.trc
..\src\djsvrr.exe   16   16   d16 >   d16-%1.rrs 2>> d-%1_16.trc
..\src\djsvrr.exe   32   32   d32 >   d32-%1.rrs 2>> d-%1_32.trc
..\src\djsvrr.exe   64   64   d64 >   d64-%1.rrs 2>> d-%1_64.trc
..\src\djsvrr.exe  128  128  d128 >  d128-%1.rrs 2>> d-%1_128.trc
..\src\djsvrr.exe  256  256  d256 >  d256-%1.rrs 2>> d-%1_256.trc
..\src\djsvrr.exe  512  512  d512 >  d512-%1.rrs 2>> d-%1_512.trc
..\src\djsvrr.exe 1024 1024 d1024 > d1024-%1.rrs 2>> d-%1_1024.trc
..\src\djsvrr.exe 2048 2048 d2048 > d2048-%1.rrs 2>> d-%1_2048.trc
..\src\djsvrr.exe 4096 4096 d4096 > d4096-%1.rrs 2>> d-%1_4096.trc
..\src\djsvor.exe    1    1    d1.YU >>    d1-%1.rrs 2>> d-%1_1.trc
..\src\djsvor.exe    2    2    d2.YU >>    d2-%1.rrs 2>> d-%1_2.trc
..\src\djsvor.exe    4    4    d4.YU >>    d4-%1.rrs 2>> d-%1_4.trc
..\src\djsvor.exe    8    8    d8.YU >>    d8-%1.rrs 2>> d-%1_8.trc
..\src\djsvor.exe   16   16   d16.YU >>   d16-%1.rrs 2>> d-%1_16.trc
..\src\djsvor.exe   32   32   d32.YU >>   d32-%1.rrs 2>> d-%1_32.trc
..\src\djsvor.exe   64   64   d64.YU >>   d64-%1.rrs 2>> d-%1_64.trc
..\src\djsvor.exe  128  128  d128.YU >>  d128-%1.rrs 2>> d-%1_128.trc
..\src\djsvor.exe  256  256  d256.YU >>  d256-%1.rrs 2>> d-%1_256.trc
..\src\djsvor.exe  512  512  d512.YU >>  d512-%1.rrs 2>> d-%1_512.trc
..\src\djsvor.exe 1024 1024 d1024.YU >> d1024-%1.rrs 2>> d-%1_1024.trc
..\src\djsvor.exe 2048 2048 d2048.YU >> d2048-%1.rrs 2>> d-%1_2048.trc
..\src\djsvor.exe 4096 4096 d4096.YU >> d4096-%1.rrs 2>> d-%1_4096.trc
..\src\djsvor.exe    1 -1    d1.YV >>    d1-%1.rrs 2>> d-%1_1.trc
..\src\djsvor.exe    2 -1    d2.YV >>    d2-%1.rrs 2>> d-%1_2.trc
..\src\djsvor.exe    4 -1    d4.YV >>    d4-%1.rrs 2>> d-%1_4.trc
..\src\djsvor.exe    8 -1    d8.YV >>    d8-%1.rrs 2>> d-%1_8.trc
..\src\djsvor.exe   16 -1   d16.YV >>   d16-%1.rrs 2>> d-%1_16.trc
..\src\djsvor.exe   32 -1   d32.YV >>   d32-%1.rrs 2>> d-%1_32.trc
..\src\djsvor.exe   64 -1   d64.YV >>   d64-%1.rrs 2>> d-%1_64.trc
..\src\djsvor.exe  128 -1  d128.YV >>  d128-%1.rrs 2>> d-%1_128.trc
..\src\djsvor.exe  256 -1  d256.YV >>  d256-%1.rrs 2>> d-%1_256.trc
..\src\djsvor.exe  512 -1  d512.YV >>  d512-%1.rrs 2>> d-%1_512.trc
..\src\djsvor.exe 1024 -1 d1024.YV >> d1024-%1.rrs 2>> d-%1_1024.trc
..\src\djsvor.exe 2048 -1 d2048.YV >> d2048-%1.rrs 2>> d-%1_2048.trc
..\src\djsvor.exe 4096 -1 d4096.YV >> d4096-%1.rrs 2>> d-%1_4096.trc
