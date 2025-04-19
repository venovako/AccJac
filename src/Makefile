RM=del /F
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FC=ifx.exe
FCLAGS=/nologo /fpp /recursive /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /I. /EHsc /MT /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprec-sqrt /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # NDEBUG
OBJ2= \
clju2.obj \
dlju2.obj \
slju2.obj \
zlju2.obj \
cljv2.obj \
dljv2.obj \
sljv2.obj \
zljv2.obj
OBJD= \
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

LDFLAGS=/link /RELEASE /LIBPATH:. jaevd.lib jaev2.lib /LIBPATH:..\..\libpvn\src pvn.lib

all: $(LIBS) $(EXES)

jaev2.lib: $(OBJ2) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ2)

jaevd.lib: $(OBJD) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJD)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [all|help|clean]"

.f90.obj:
	$(FC) $(FCFLAGS) /c $< /Fo$@

.F90.exe:
	$(FC) $(FCFLAGS) $< /Fe$@ $(LDFLAGS)

clean:
	-$(RM) *.exe
	-$(RM) *.lib
	-$(RM) *.obj
	-$(RM) *.optrpt
