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

OBJ2 += \
sfma.o dfma.o xfma.o\
cfma.o zfma.o wfma.o\
cjaev2.o djaev2.o qjaev2.o sjaev2.o yjaev2.o zjaev2.o\
cjiev2.o djiev2.o qjiev2.o sjiev2.o yjiev2.o zjiev2.o\
clju2.o dlju2.o slju2.o zlju2.o\
cljv2.o dljv2.o sljv2.o zljv2.o\
cljau2.o dljau2.o sljau2.o wljau2.o xljau2.o zljau2.o\
cljav2.o dljav2.o sljav2.o wljav2.o xljav2.o zljav2.o

LIB2=libjaev2.a

EXE2=\
ctest.exe dtest.exe qtest.exe stest.exe ytest.exe ztest.exe\
cljv2t.exe dljv2t.exe sljv2t.exe zljv2t.exe

ifdef MPFR
OBJ2 += mljau2.o mljav2.o
EXE2 += sth2t.exe
endif # MPFR

OBJD=\
bfopen.o pascal.o\
cjaevd.o zjaevd.o\
drsafe.o qrsafe.o srsafe.o\
qre.o qdetm1.o ydetm1.o\
croth.o droth.o sroth.o zroth.o\
crott.o drott.o srott.o zrott.o\
crtvh.o drtvh.o srtvh.o wrtvh.o xrtvh.o zrtvh.o\
crtvt.o drtvt.o srtvt.o wrtvt.o xrtvt.o zrtvt.o\
crtlt.o drtlt.o srtlt.o wrtlt.o xrtlt.o zrtlt.o\
crtrt.o drtrt.o srtrt.o wrtrt.o xrtrt.o zrtrt.o\
crtlh.o drtlh.o srtlh.o wrtlh.o xrtlh.o zrtlh.o\
crtrh.o drtrh.o srtrh.o wrtrh.o xrtrh.o zrtrh.o\
cnrmf.o dnrmf.o snrmf.o znrmf.o\
csdp.o dsdp.o ssdp.o zsdp.o\
cgram.o dgram.o sgram.o zgram.o\
cscalg.o dscalg.o sscalg.o zscalg.o\
cscala.o dscala.o sscala.o wscala.o xscala.o zscala.o\
cswpc.o dswpc.o sswpc.o wswpc.o xswpc.o zswpc.o\
cswpr.o dswpr.o sswpr.o wswpr.o xswpr.o zswpr.o\
cswpxd.o dswpxd.o sswpxd.o wswpxd.o xswpxd.o zswpxd.o\
cinisv.o dinisv.o sinisv.o zinisv.o\
ctrans.o dtrans.o strans.o ztrans.o\
ctrana.o dtrana.o strana.o wtrana.o xtrana.o ztrana.o\
coffa.o doffa.o soffa.o zoffa.o\
dtrack.o strack.o\
ctrace.o dtrace.o strace.o wtrace.o xtrace.o ztrace.o\
cnrmoa.o dnrmoa.o snrmoa.o wnrmoa.o xnrmoa.o znrmoa.o\
dtrkoa.o ztrkoa.o\
ctrcoa.o dtrcoa.o strcoa.o wtrcoa.o xtrcoa.o ztrcoa.o\
dmmmsq.o jmmmsq.o qmmmsq.o wmmmsq.o xmmmsq.o ymmmsq.o zmmmsq.o\
cjsvdc.o djsvdc.o sjsvdc.o zjsvdc.o\
cjsvdr.o djsvdr.o sjsvdr.o zjsvdr.o\
cjevdc.o djevdc.o sjevdc.o wjevdc.o xjevdc.o zjevdc.o\
cjevdr.o djevdr.o sjevdr.o wjevdr.o xjevdr.o zjevdr.o\
jstep.o jsweep.o djevdm.o zjevdm.o

LIBD=libjaevd.a

EXED=past.exe ransym.exe\
dxgtg.exe zwghg.exe\
cevdj.exe zevdj.exe\
cjsvdt.exe djsvdt.exe sjsvdt.exe zjsvdt.exe\
cjevdt.exe djevdt.exe sjevdt.exe wjevdt.exe xjevdt.exe zjevdt.exe\
cpproc.exe dpproc.exe spproc.exe wpproc.exe xpproc.exe zpproc.exe

ifdef MPFR
OBJD += \
mrtvh.o mrtvt.o\
mrtlt.o mrtrt.o mrtlh.o mrtrh.o\
mswpc.o mswpr.o mswpxd.o\
mtrana.o mtrace.o\
mjevdr.o
EXED += mjevdt.exe
endif # MPFR

ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJL += wlaev2.o xlaev2.o

OBJ2 += \
wjaev2.o xjaev2.o\
wjiev2.o xjiev2.o\
wlju2.o xlju2.o\
xljv2.o wljv2.o

EXE2 += \
wtest.exe xtest.exe\
wljv2t.exe xljv2t.exe

OBJD += xrsafe.o\
wroth.o xroth.o\
wrott.o xrott.o\
wnrmf.o xnrmf.o\
wsdp.o xsdp.o\
wgram.o xgram.o\
wscalg.o xscalg.o\
winisv.o xinisv.o\
wtrans.o xtrans.o\
woffa.o xoffa.o\
xtrack.o\
wjsvdc.o xjsvdc.o\
wjsvdr.o xjsvdr.o\
wjevdc.o wjevdr.o

EXED += \
wjsvdt.exe xjsvdt.exe
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
