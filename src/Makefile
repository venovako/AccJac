RM=del /F
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FC=ifx.exe
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # NDEBUG
FCFLAGS=/nologo /fpp /recursive /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /I. /MT /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8
!ENDIF # ilp64
LDFLAGS=/link /RELEASE /LIBPATH:. jaevd.lib jaev2.lib /LIBPATH:..\..\libpvn\src pvn.lib

OBJ2= \
cjaev2.obj \
djaev2.obj \
qjaev2.obj \
sjaev2.obj \
yjaev2.obj \
zjaev2.obj \
cjiev2.obj \
djiev2.obj \
qjiev2.obj \
sjiev2.obj \
yjiev2.obj \
zjiev2.obj \
clju2.obj  \
dlju2.obj  \
slju2.obj  \
zlju2.obj  \
cljv2.obj  \
dljv2.obj  \
sljv2.obj  \
zljv2.obj  \
dljau2.obj \
zljau2.obj \
dljav2.obj \
zljav2.obj \
qlaev2.obj \
ylaev2.obj

# e.g., LAPACK=/Qmkl:sequential
!IFDEF LAPACK
FCFLAGS=$(FCFLAGS) $(LAPACK)
!ELSE # !LAPACK
OBJ2=$(OBJ2) \
claev2.obj   \
dlaev2.obj   \
slaev2.obj   \
zlaev2.obj
!ENDIF # ?LAPACK

OBJD= \
bfopen.obj \
pascal.obj \
cjaevd.obj \
zjaevd.obj \
drsafe.obj \
qrsafe.obj \
srsafe.obj \
qre.obj    \
qdetm1.obj \
ydetm1.obj \
croth.obj  \
droth.obj  \
sroth.obj  \
zroth.obj  \
crott.obj  \
drott.obj  \
srott.obj  \
zrott.obj  \
crtvh.obj  \
drtvh.obj  \
srtvh.obj  \
zrtvh.obj  \
crtvt.obj  \
drtvt.obj  \
srtvt.obj  \
zrtvt.obj  \
drtlt.obj  \
drtrt.obj  \
zrtlt.obj  \
zrtrt.obj  \
drtlh.obj  \
drtrh.obj  \
zrtlh.obj  \
zrtrh.obj  \
cnrmf.obj  \
dnrmf.obj  \
snrmf.obj  \
znrmf.obj  \
csdp.obj   \
dsdp.obj   \
ssdp.obj   \
zsdp.obj   \
cgram.obj  \
dgram.obj  \
sgram.obj  \
zgram.obj  \
cscalg.obj \
dscalg.obj \
sscalg.obj \
zscalg.obj \
dscala.obj \
zscala.obj \
dswpc.obj  \
dswpr.obj  \
zswpc.obj  \
zswpr.obj  \
dswpxd.obj \
zswpxd.obj \
cinisv.obj \
dinisv.obj \
sinisv.obj \
zinisv.obj \
ctrans.obj \
dtrans.obj \
strans.obj \
ztrans.obj \
dtrana.obj \
ztrana.obj \
coffa.obj  \
doffa.obj  \
soffa.obj  \
zoffa.obj  \
dtrack.obj \
strack.obj \
dtrace.obj \
ztrace.obj \
dnrmoa.obj \
znrmoa.obj \
dtrkoa.obj \
ztrkoa.obj \
dtrcoa.obj \
ztrcoa.obj \
cjsvdc.obj \
djsvdc.obj \
sjsvdc.obj \
zjsvdc.obj \
cjsvdr.obj \
djsvdr.obj \
sjsvdr.obj \
zjsvdr.obj \
djevdr.obj \
zjevdr.obj
LIBS= \
jaev2.lib \
jaevd.lib
EXES= \
past.exe   \
cevdj.exe  \
zevdj.exe  \
cljv2t.exe \
dljv2t.exe \
sljv2t.exe \
zljv2t.exe \
cjsvdt.exe \
djsvdt.exe \
sjsvdt.exe \
zjsvdt.exe \
djevdt.exe \
zjevdt.exe \
dpproc.exe \
zpproc.exe

all: $(LIBS) $(EXES)

jaev2.lib: $(OBJ2) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ2)

jaevd.lib: $(OBJD) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJD)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=lp64|ilp64] [LAPACK=...] [all|help|clean]"

.f90.obj:
	$(FC) $(FCFLAGS) /c $< /Fo$@

.F90.exe:
	$(FC) $(FCFLAGS) $< /Fe$@ $(LDFLAGS)

clean:
	-$(RM) *.exe
	-$(RM) *.lib
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
