..\src\dxresc.exe 256 256 128 -64 1 d256
..\src\xjsvdx.exe 256 256 128 0 d256
MOVE /Y d256.SY d256.SX
MOVE /Y d256.X d256.Y
..\src\djsvdx.exe 256 256 128 0 d256
..\src\dxsvrr.exe 256 d256 > dx-0.txt 2>&1
..\src\djsvdx.exe 256 256 128 2 d256
..\src\dxsvrr.exe 256 d256 > dx-2.txt 2>&1
