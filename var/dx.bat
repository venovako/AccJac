..\src\dxresc.exe 128 128 64 -33 1 d128
..\src\xjsvdx.exe 128 128 64 0 d128
MOVE /Y d128.SY d128.SX
MOVE /Y d128.X d128.Y
..\src\djsvdx.exe 128 128 64 0 d128
..\src\dxsvrr.exe 128 d128 > dx-0.txt
..\src\djsvdx.exe 128 128 64 2 d128
..\src\dxsvrr.exe 128 d128 > dx-2.txt
