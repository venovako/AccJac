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
clju2.obj    \
dlju2.obj    \
slju2.obj    \
zlju2.obj    \
cljv2.obj    \
dljv2.obj    \
sljv2.obj    \
zljv2.obj    \
qlaev2.obj   \
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
cjaevd.obj \
zjaevd.obj \
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
cinisv.obj \
dinisv.obj \
sinisv.obj \
zinisv.obj \
ctrans.obj \
dtrans.obj \
strans.obj \
ztrans.obj \
coffa.obj  \
doffa.obj  \
soffa.obj  \
zoffa.obj  \
dtrack.obj \
strack.obj \
dtrkoa.obj \
ztrkoa.obj \
cjsvdc.obj \
djsvdc.obj \
sjsvdc.obj \
zjsvdc.obj \
cjsvdr.obj \
djsvdr.obj \
sjsvdr.obj \
zjsvdr.obj
LIBS= \
jaev2.lib \
jaevd.lib
EXES= \
cevdj.exe  \
zevdj.exe  \
cljv2t.exe \
dljv2t.exe \
sljv2t.exe \
zljv2t.exe \
dth2t.exe  \
sth2t.exe  \
cjsvdt.exe \
djsvdt.exe \
sjsvdt.exe \
zjsvdt.exe

all: $(LIBS) $(EXES)

jaev2.lib: $(OBJ2) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ2)

jaevd.lib: $(OBJD) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJD)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=lp64|ilp64] [all|help|clean]"

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
