include system.mk
include $(COMPILER).mk
include debug.mk
ifneq ($(ABI),lp64)
include $(ABI).mk
endif # !lp64
MKFS=GNUmakefile system.mk $(COMPILER).mk debug.mk
ifneq ($(ABI),lp64)
MKFS += $(ABI).mk
endif # !lp64

OBJL=\
qlaev2.o ylaev2.o
ifndef LAPACK
OBJL += \
claev2.o dlaev2.o slaev2.o zlaev2.o
endif # !LAPACK

ifdef MPFR
OBJ2=mpfr_f.o
else # !MPFR
OBJ2=
endif # ?MPFR

OBJ2 += xsqrt.o\
cfma.o dfma.o sfma.o wfma.o xfma.o zfma.o\
cjaev2.o djaev2.o qjaev2.o sjaev2.o yjaev2.o zjaev2.o\
cjiev2.o djiev2.o qjiev2.o sjiev2.o yjiev2.o zjiev2.o\
cljtu2.o dljtu2.o sljtu2.o wljtu2.o xljtu2.o zljtu2.o\
cljtv2.o dljtv2.o sljtv2.o wljtv2.o xljtv2.o zljtv2.o\
cljau2.o dljau2.o sljau2.o wljau2.o xljau2.o zljau2.o\
cljav2.o dljav2.o sljav2.o wljav2.o xljav2.o zljav2.o

LIB2=libjaev2.a

EXE2=ctest.exe dtest.exe qtest.exe stest.exe ytest.exe ztest.exe\
cljv2t.exe dljv2t.exe sljv2t.exe zljv2t.exe

ifdef MPFR
OBJ2 += mljau2.o mljav2.o
EXE2 += sth2t.exe
endif # MPFR

OBJD=bfopen.o pascal.o\
crn.o wrn.o zrn.o\
cjaevd.o zjaevd.o\
drsafe.o qrsafe.o srsafe.o\
qre.o qdetm1.o ydetm1.o\
crth.o drth.o srth.o wrth.o xrth.o zrth.o\
crtt.o drtt.o srtt.o wrtt.o xrtt.o zrtt.o\
crtvh.o drtvh.o srtvh.o wrtvh.o xrtvh.o zrtvh.o\
crtvt.o drtvt.o srtvt.o wrtvt.o xrtvt.o zrtvt.o\
crtlt.o drtlt.o srtlt.o wrtlt.o xrtlt.o zrtlt.o\
crtrt.o drtrt.o srtrt.o wrtrt.o xrtrt.o zrtrt.o\
crtlh.o drtlh.o srtlh.o wrtlh.o xrtlh.o zrtlh.o\
crtrh.o drtrh.o srtrh.o wrtrh.o xrtrh.o zrtrh.o\
cnrmf.o dnrmf.o snrmf.o wnrmf.o xnrmf.o znrmf.o\
csdp.o dsdp.o ssdp.o wsdp.o xsdp.o zsdp.o\
cgram.o dgram.o sgram.o wgram.o xgram.o zgram.o\
cscalg.o dscalg.o sscalg.o wscalg.o xscalg.o zscalg.o\
cscala.o dscala.o sscala.o wscala.o xscala.o zscala.o\
cswpc.o dswpc.o sswpc.o wswpc.o xswpc.o zswpc.o\
cswpr.o dswpr.o sswpr.o wswpr.o xswpr.o zswpr.o\
cswpxd.o dswpxd.o sswpxd.o wswpxd.o xswpxd.o zswpxd.o\
cinisx.o dinisx.o sinisx.o winisx.o xinisx.o zinisx.o\
ccnrmf.o dcnrmf.o scnrmf.o wcnrmf.o xcnrmf.o zcnrmf.o\
dnsort.o snsort.o xnsort.o\
cprcyc.o dprcyc.o sprcyc.o wprcyc.o xprcyc.o zprcyc.o\
ctrana.o dtrana.o strana.o wtrana.o xtrana.o ztrana.o\
ctrnsf.o dtrnsf.o strnsf.o wtrnsf.o xtrnsf.o ztrnsf.o\
ctruti.o wtruti.o ztruti.o\
dtrack.o strack.o xtrack.o\
ctrace.o dtrace.o strace.o wtrace.o xtrace.o ztrace.o\
cnrmoa.o dnrmoa.o snrmoa.o wnrmoa.o xnrmoa.o znrmoa.o\
ctrcoa.o dtrcoa.o strcoa.o wtrcoa.o xtrcoa.o ztrcoa.o\
dmmmsq.o jmmmsq.o qmmmsq.o wmmmsq.o xmmmsq.o ymmmsq.o zmmmsq.o\
cdsort.o ddsort.o sdsort.o wdsort.o xdsort.o zdsort.o\
cjsvdf.o djsvdf.o sjsvdf.o wjsvdf.o xjsvdf.o zjsvdf.o\
cjevdc.o djevdc.o sjevdc.o wjevdc.o xjevdc.o zjevdc.o\
cjevdr.o djevdr.o sjevdr.o wjevdr.o xjevdr.o zjevdr.o\
jstep.o jsweep.o djevdm.o zjevdm.o

LIBD=libjaevd.a

EXED=past.exe\
crnher.exe drnsym.exe srnsym.exe wrnher.exe xrnsym.exe zrnher.exe\
dxgtg.exe zwghg.exe\
cevdj.exe zevdj.exe\
cjevdt.exe djevdt.exe sjevdt.exe wjevdt.exe xjevdt.exe zjevdt.exe\
cpproc.exe dpproc.exe spproc.exe wpproc.exe xpproc.exe zpproc.exe\
cjsvdx.exe djsvdx.exe sjsvdx.exe zjsvdx.exe

ifdef MPFR
OBJD += \
mrtvh.o mrtvt.o\
mrtlt.o mrtrt.o mrtlh.o mrtrh.o\
mswpc.o mswpr.o mswpxd.o\
mtrana.o mtrace.o\
mdsort.o mjevdr.o
EXED += mjevdt.exe
endif # MPFR

ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJL += wlaev2.o xlaev2.o
OBJ2 += \
wjaev2.o xjaev2.o\
wjiev2.o xjiev2.o
EXE2 += \
wtest.exe xtest.exe\
wljv2t.exe xljv2t.exe
OBJD += xrsafe.o
EXED += wjsvdx.exe xjsvdx.exe
endif # x86_64 or i86pc
endif # gfortran

ifeq ($(findstring MINGW64,$(OS)),MINGW64)
LDFLAGS=
else # !MINGW64
LDFLAGS=-rdynamic
endif # ?MINGW64
LDFLAGS += -static-libgcc
ifeq ($(COMPILER),gfortran)
LDFLAGS += -static-libgfortran -static-libquadmath
endif # gfortran
LDFLAGS += -L. -ljaevd -ljaev2 -L../../libpvn/src -lpvn $(LAPACK)
ifdef MPFR
LDFLAGS += -L$(MPFR)/lib -lmpfr
endif # MPFR
ifdef GMP
LDFLAGS += -L$(GMP)/lib -lgmp
endif # GMP
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
endif # ANIMATE
ifneq ($(findstring MINGW64,$(OS)),MINGW64)
LDFLAGS += -ldl
endif # !MINGW64
LDFLAGS += -lm

.PHONY: all help clean

all: $(LIB2) $(LIBD) $(EXE2) $(EXED)
ifeq ($(findstring MINGW64,$(OS)),MINGW64)
	cp -fuv /ucrt64/bin/libwinpthread-1.dll .
endif

$(LIB2): $(OBJ2) $(OBJL)
	$(AR) $(ARFLAGS) $@ $(OBJ2) $(OBJL)

$(LIBD): $(OBJD)
	$(AR) $(ARFLAGS) $@ $(OBJD)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [MARCH=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [LAPACK=...] [GMP=...] [MPFR=...] [PROFILE=...] [ANIMATE=ppe] [all|help|clean]"

%.o : %.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c $< -o $@

%.exe : %.F90 $(MKFS) $(LIB2) $(LIBD)
	$(FC) $(FCFLAGS) $< -o $@ $(LDFLAGS)

clean:
	-$(DEL) *.o
	-$(DEL) *.a
	-$(DEL) *.exe
	-$(DEL) *.mod
	-$(DEL) *.dSYM
	-$(DEL) *.optrpt
