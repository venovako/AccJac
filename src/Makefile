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
# /DCARITH_PVN="cma"
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8
!ENDIF # ilp64
LDFLAGS=/link /RELEASE /LIBPATH:. jaevd.lib jaev2.lib /LIBPATH:..\..\libpvn\src pvn.lib

OBJ2= \
sfma.obj dfma.obj xfma.obj \
cfma.obj zfma.obj wfma.obj \
cjaev2.obj djaev2.obj qjaev2.obj sjaev2.obj yjaev2.obj zjaev2.obj \
cjiev2.obj djiev2.obj qjiev2.obj sjiev2.obj yjiev2.obj zjiev2.obj \
clju2.obj dlju2.obj slju2.obj zlju2.obj \
cljv2.obj dljv2.obj sljv2.obj zljv2.obj \
cljau2.obj dljau2.obj sljau2.obj wljau2.obj xljau2.obj zljau2.obj \
cljav2.obj dljav2.obj sljav2.obj wljav2.obj xljav2.obj zljav2.obj \
qlaev2.obj ylaev2.obj

!IFDEF LAPACK
FCFLAGS=$(FCFLAGS) $(LAPACK)
!ELSE # !LAPACK
OBJ2=$(OBJ2) \
claev2.obj dlaev2.obj slaev2.obj zlaev2.obj
!ENDIF # ?LAPACK

OBJD= \
bfopen.obj pascal.obj \
cjaevd.obj zjaevd.obj \
drsafe.obj qrsafe.obj srsafe.obj \
qre.obj qdetm1.obj ydetm1.obj \
croth.obj droth.obj sroth.obj zroth.obj \
crott.obj drott.obj srott.obj zrott.obj \
crtvh.obj drtvh.obj srtvh.obj wrtvh.obj xrtvh.obj zrtvh.obj \
crtvt.obj drtvt.obj srtvt.obj wrtvt.obj xrtvt.obj zrtvt.obj \
crtlt.obj drtlt.obj srtlt.obj wrtlt.obj xrtlt.obj zrtlt.obj \
crtrt.obj drtrt.obj srtrt.obj wrtrt.obj xrtrt.obj zrtrt.obj \
crtlh.obj drtlh.obj srtlh.obj wrtlh.obj xrtlh.obj zrtlh.obj \
crtrh.obj drtrh.obj srtrh.obj wrtrh.obj xrtrh.obj zrtrh.obj \
cnrmf.obj dnrmf.obj snrmf.obj znrmf.obj \
csdp.obj dsdp.obj ssdp.obj zsdp.obj \
cgram.obj dgram.obj sgram.obj zgram.obj \
cscalg.obj dscalg.obj sscalg.obj zscalg.obj \
cscala.obj dscala.obj sscala.obj wscala.obj xscala.obj zscala.obj \
cswpc.obj dswpc.obj sswpc.obj wswpc.obj xswpc.obj zswpc.obj \
cswpr.obj dswpr.obj sswpr.obj wswpr.obj xswpr.obj zswpr.obj \
cswpxd.obj dswpxd.obj sswpxd.obj wswpxd.obj xswpxd.obj zswpxd.obj \
cinisv.obj dinisv.obj sinisv.obj zinisv.obj \
ctrans.obj dtrans.obj strans.obj ztrans.obj \
ctrana.obj dtrana.obj strana.obj wtrana.obj xtrana.obj ztrana.obj \
coffa.obj doffa.obj soffa.obj zoffa.obj \
dtrack.obj strack.obj \
ctrace.obj dtrace.obj strace.obj wtrace.obj xtrace.obj ztrace.obj \
cnrmoa.obj dnrmoa.obj snrmoa.obj wnrmoa.obj xnrmoa.obj znrmoa.obj \
dtrkoa.obj ztrkoa.obj \
ctrcoa.obj dtrcoa.obj strcoa.obj wtrcoa.obj xtrcoa.obj ztrcoa.obj \
dmmmsq.obj jmmmsq.obj qmmmsq.obj wmmmsq.obj xmmmsq.obj ymmmsq.obj zmmmsq.obj \
cjsvdc.obj djsvdc.obj sjsvdc.obj zjsvdc.obj \
cjsvdr.obj djsvdr.obj sjsvdr.obj zjsvdr.obj \
cjevdc.obj djevdc.obj sjevdc.obj wjevdc.obj xjevdc.obj zjevdc.obj \
cjevdr.obj djevdr.obj sjevdr.obj wjevdr.obj xjevdr.obj zjevdr.obj \
jstep.obj jsweep.obj djevdm.obj zjevdm.obj

LIBS= \
jaev2.lib \
jaevd.lib

EXES= \
past.exe ransym.exe \
dxgtg.exe zwghg.exe \
cevdj.exe zevdj.exe \
cljv2t.exe dljv2t.exe sljv2t.exe zljv2t.exe \
cjsvdt.exe djsvdt.exe sjsvdt.exe zjsvdt.exe \
cjevdt.exe djevdt.exe sjevdt.exe wjevdt.exe xjevdt.exe zjevdt.exe \
cpproc.exe dpproc.exe spproc.exe wjevdt.exe xpproc.exe zpproc.exe

all: $(LIBS) $(EXES)

jaev2.lib: $(OBJ2) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ2)

jaevd.lib: $(OBJD) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJD)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=lp64|ilp64] [LAPACK=/Qmkl:sequential] [all|help|clean]"

.f90.obj:
	$(FC) $(FCFLAGS) /c $< /Fo$@

.F90.exe:
	$(FC) $(FCFLAGS) $< /Fe$@ $(LDFLAGS)

clean:
	-$(RM) *.exe
	-$(RM) *.lib
	-$(FM) *.mod
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
