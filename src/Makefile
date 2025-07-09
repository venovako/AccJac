RM=DEL /F
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FC=ifx.exe
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # NDEBUG
# /DUSE_IEEE_INTRINSIC=IEEE_FMA
!IF "$(CRT)"=="MD"
FCFLAGS=/nologo /fpp /Qopenmp /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /I. /MD /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!ELSE # MT
FCFLAGS=/nologo /fpp /recursive /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /I. /MT /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!ENDIF # ?MD
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8
!ENDIF # ilp64
LDFLAGS=/link /RELEASE /LIBPATH:. jaevd.lib jaev2.lib /LIBPATH:..\..\libpvn\src pvn.lib

OBJ2=qclear.obj yclear.obj drsafe.obj qrsafe.obj srsafe.obj \
cjaev2.obj djaev2.obj qjaev2.obj sjaev2.obj yjaev2.obj zjaev2.obj \
cjiev2.obj djiev2.obj qjiev2.obj sjiev2.obj yjiev2.obj zjiev2.obj \
cljtu2.obj dljtu2.obj sljtu2.obj wljtu2.obj xljtu2.obj zljtu2.obj \
cljtv2.obj dljtv2.obj sljtv2.obj wljtv2.obj xljtv2.obj zljtv2.obj \
cljau2.obj dljau2.obj sljau2.obj wljau2.obj xljau2.obj zljau2.obj \
cljav2.obj dljav2.obj sljav2.obj wljav2.obj xljav2.obj zljav2.obj \
qlaev2.obj ylaev2.obj

!IFNDEF CUTOFF
CUTOFF=0.8
!ENDIF # !CUTOFF
FCFLAGS=$(FCFLAGS) /DCUTOFF=$(CUTOFF)_K

!IFDEF LAPACK
FCFLAGS=$(FCFLAGS) $(LAPACK)
!ELSE # !LAPACK
OBJ2=$(OBJ2) \
claev2.obj dlaev2.obj slaev2.obj zlaev2.obj
!ENDIF # ?LAPACK

OBJD=bfopen.obj wbrd1.obj wbwr1.obj xbrd1.obj xbwr1.obj pascal.obj \
crn.obj wrn.obj zrn.obj \
cjaevd.obj zjaevd.obj \
qre.obj qdetm1.obj ydetm1.obj \
crth.obj drth.obj srth.obj wrth.obj xrth.obj zrth.obj \
crtt.obj drtt.obj srtt.obj wrtt.obj xrtt.obj zrtt.obj \
crtvh.obj drtvh.obj srtvh.obj wrtvh.obj xrtvh.obj zrtvh.obj \
crtvt.obj drtvt.obj srtvt.obj wrtvt.obj xrtvt.obj zrtvt.obj \
crtlt.obj drtlt.obj srtlt.obj wrtlt.obj xrtlt.obj zrtlt.obj \
crtrt.obj drtrt.obj srtrt.obj wrtrt.obj xrtrt.obj zrtrt.obj \
crtlh.obj drtlh.obj srtlh.obj wrtlh.obj xrtlh.obj zrtlh.obj \
crtrh.obj drtrh.obj srtrh.obj wrtrh.obj xrtrh.obj zrtrh.obj \
cnrmf.obj dnrmf.obj snrmf.obj wnrmf.obj xnrmf.obj znrmf.obj \
csdp.obj dsdp.obj ssdp.obj wsdp.obj xsdp.obj zsdp.obj \
cgram.obj dgram.obj sgram.obj wgram.obj xgram.obj zgram.obj \
cscalg.obj dscalg.obj sscalg.obj wscalg.obj xscalg.obj zscalg.obj \
cscala.obj dscala.obj sscala.obj wscala.obj xscala.obj zscala.obj \
cswpc.obj dswpc.obj sswpc.obj wswpc.obj xswpc.obj zswpc.obj \
cswpr.obj dswpr.obj sswpr.obj wswpr.obj xswpr.obj zswpr.obj \
cswpxd.obj dswpxd.obj sswpxd.obj wswpxd.obj xswpxd.obj zswpxd.obj \
cinisx.obj dinisx.obj sinisx.obj winisx.obj xinisx.obj zinisx.obj \
ccnrmf.obj dcnrmf.obj scnrmf.obj wcnrmf.obj xcnrmf.obj zcnrmf.obj \
dnsort.obj snsort.obj xnsort.obj \
cprcyc.obj dprcyc.obj sprcyc.obj wprcyc.obj xprcyc.obj zprcyc.obj \
ctrana.obj dtrana.obj strana.obj wtrana.obj xtrana.obj ztrana.obj \
ctrnsf.obj dtrnsf.obj strnsf.obj wtrnsf.obj xtrnsf.obj ztrnsf.obj \
ctrnsp.obj dtrnsp.obj strnsp.obj wtrnsp.obj xtrnsp.obj ztrnsp.obj \
ctruti.obj dtruti.obj struti.obj wtruti.obj xtruti.obj ztruti.obj \
dtrack.obj strack.obj xtrack.obj \
ctrace.obj dtrace.obj strace.obj wtrace.obj xtrace.obj ztrace.obj \
cnrmoa.obj dnrmoa.obj snrmoa.obj wnrmoa.obj xnrmoa.obj znrmoa.obj \
ctrcoa.obj dtrcoa.obj strcoa.obj wtrcoa.obj xtrcoa.obj ztrcoa.obj \
dmmmsq.obj jmmmsq.obj qmmmsq.obj wmmmsq.obj xmmmsq.obj ymmmsq.obj zmmmsq.obj \
cdsort.obj ddsort.obj sdsort.obj wdsort.obj xdsort.obj zdsort.obj \
cjsvdf.obj djsvdf.obj sjsvdf.obj wjsvdf.obj xjsvdf.obj zjsvdf.obj \
cjsvdp.obj djsvdp.obj sjsvdp.obj wjsvdp.obj xjsvdp.obj zjsvdp.obj \
cjevdc.obj djevdc.obj sjevdc.obj wjevdc.obj xjevdc.obj zjevdc.obj \
cjevdr.obj djevdr.obj sjevdr.obj wjevdr.obj xjevdr.obj zjevdr.obj \
jstep.obj jsweep.obj djevdm.obj zjevdm.obj

LIBS=jaev2.lib jaevd.lib

EXES=past.exe \
crnher.exe drnsym.exe srnsym.exe wrnher.exe xrnsym.exe zrnher.exe \
dxgtg.exe zwghg.exe \
cevdj.exe zevdj.exe \
cljv2t.exe dljv2t.exe sljv2t.exe zljv2t.exe \
cjevdt.exe djevdt.exe sjevdt.exe wjevdt.exe xjevdt.exe zjevdt.exe \
cpproc.exe dpproc.exe spproc.exe wpproc.exe xpproc.exe zpproc.exe \
cjsvdx.exe djsvdx.exe sjsvdx.exe wjsvdx.exe xjsvdx.exe zjsvdx.exe \
cjsvrr.exe djsvrr.exe sjsvrr.exe zjsvrr.exe \
cjsvor.exe djsvor.exe sjsvor.exe zjsvor.exe \
dxresc.exe zwresc.exe dxsvrr.exe

all: $(LIBS) $(EXES)
!IF "$(CRT)"=="MD"
	COPY /Y /V ..\..\libpvn\src\*.dll .
!ENDIF # MD

jaev2.lib: $(OBJ2) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ2)

jaevd.lib: $(OBJD) Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJD)

help:
	@ECHO "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [ABI=lp64|ilp64] [CUTOFF=0.8] [LAPACK=/Qmkl:sequential] [CRT=MT|MD] [all|help|clean]"
!IF "$(CRT)"=="MD"
	@ECHO "libpvn has to be build beforehand with the CRT=MD build option"
!ENDIF # MD

.f90.obj:
	$(FC) $(FCFLAGS) /c $< /Fo$@

.F90.exe:
	$(FC) $(FCFLAGS) $< /Fe$@ $(LDFLAGS)

clean:
	-$(RM) *.exe
	-$(RM) *.dll
	-$(RM) *.lib
	-$(RM) *.obj
	-$(RM) *.optrpt
	-$(RM) *.pdb
